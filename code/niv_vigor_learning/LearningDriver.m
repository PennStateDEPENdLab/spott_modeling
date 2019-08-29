%---------------------------------------------------
% driver for running online RL for rates of behavior 
%---------------------------------------------------

% 26 June 2007:
% got rid of recurrent state in direct-actor M-learning 

% 12 Feb 2007:
% implement variable EatTimes
% made sure that dependence on average reward rate works
% M/Q implementation seamless through a binary parameter 'Mlearning'

% 9 Feb 2007:
%
% critic values updated only once in a cycle to prevent drift of the values
%
% both interval and ratio work and are stable through mnultiplying both
% Beta and Alpha learning rates by exp(|Alpha|+|Beta|) (this reduces the
% learning rate when values deviate a lot from zero)
%
% implement Q learning for action values (averaging over tau) for
% unconstrained only (changes for swapping between this and M-learning marked with %M/%Q)

% 4 Feb 2007:
% Random EatTimes not implemented
% Ratio constrained: does not work

% Actions = {'LP','NP'};
Actions = {'LP','Other','NP'};              % the possible actions
Nactions = length(Actions);

EatTime = 6;                                % average time needed to consume a reward

% rho = [-.15,-.05];                          % internal rewards per unit + cost per unit action
rho = [-.15,0.15,-.15];                      % for RI10 use rho(other) = 1.7; for RR10 use 0.7 and after learning decrease M to 0.5M to promote exploration

s = 0.5;             
d = 1.5;
% k_v = [s d;d s];                            % transition costs from action i to action j
k_v = [s d d;d s d;d d s];                  % make self transitions to continuous actions 'free'

% check to make sure we did everything right so far
if (length(rho) ~= Nactions || length(k_v) ~= Nactions)
    error('DRIVER ERROR: you forgot to change RHO or K_V to match the number of actions!')
end

figures = 1;                                % draw figures of results?

schedule_type = 'Random';                   % random, variable or fixed schedule
schedule = 'Interval';                      % interval or ratio schedule                         
ratio = 10;                                 % mean ratio for ratio schedules
interval = 30;                              % mean interval between rewards
Utility = 90;                               % utility of external reward
fprintf('\n The utility of the reward in this run is %d',Utility);

% learning mode: we've tried several things here (in principle all of these should work):
% LearnMode = 'OnPolicyQ';                    % On policy Q learning
% LearnMode = 'OffPolicyQ';                   % Off policy Q learning
% LearnMode = 'HybridSarsa';                  % Q learning using values
LearnMode = 'DirectActor';                  % Parametrizing the policy and following the gradient of the expected reward per state
constrained = 0;                            % impose constraints on parameter ranges implicitly (0) or explicitly (1)
includeR = 1;                               % include R_avg in parametrized policy or not (we don't support this option for 'constrained' yet)
Mlearning = 1;                              % whether to parametrize the policy with M or learn Q values for (state,action) pairs
learn = 1;                                  % learn new parameters or use old policy to just simulate data?
% R = R*0.4;                                  % use previous parameters and possibly change R_avg
Nsmooth = 5;                                % how many neighboring Qs to update from each side (to get smoother function) w/o direct policy 

% Data will be generated according to requested # of trials or sessions, whichever
% is satisfied first
Reward_times = 0; % Rewards;                % use previous reward times? (to yoke animals)
Ndata = 2000;                                % trials to run (real rats: 30*5*40 = 6000)
Nsessions = 1000;                           % number of sessions to run
max_actions = 200;                          % per trial, to avoid runaway bad policies

% dt = 10;                                  % discretization of time within a second
if strcmpi(schedule,'interval')
    dt = 10;
    times = (1/dt:1/dt:30)';                    % Possible times for actions. The real time is i/dt
else
    dt = 20;
    times = (1/dt:1/dt:5)';
end
Ntimes = length(times);  

% Indexing the states so we will be able to refer to them intuitively later:
n = 0; Sindex = [];
if (strcmpi(schedule,'ratio'))
    if (strcmpi(schedule_type,'random'))      % RR -- care only about last action and i_r           
        for i = 1:Nactions; 
            for j = 1:2
                n = n+1;
                Sindex(1,i,j) = n;
            end
        end
        %fprintf('Deleting the nonexistent state # %d out of %d',Sindex(1,Nactions,2),n)
        Sindex(1,Nactions,2) = NaN; n=n-1;    % as the state of having a reward after NP does not exist
        Slog = Sindex(1,1,1);                 % the state for which to log parameter changes
    else                                      % FR -- also count how many LPs already made
        for i = 1:Nactions; 
            for j = 1:2; 
                for k = 1:ratio; 
                    n = n+1;
                    Sindex(k,i,j) = n;
                end
            end
        end
        %fprintf('Deleting the nonexistent states # %d-%d out of %d',Sindex(1,Nactions,2),Sindex(ratio,Nactions,2),n)
        for k = 1:ratio;
            Sindex(k,Nactions,2)=NaN;n=n-1;   % as the state of having a reward after NP does not exist   
        end
        Slog = Sindex(2,1,1);                 % the state for which to log parameter changes
    end
else                                          % RI/VI -- also count time since last LP
    for i = 1:Nactions; 
        for j = 1:2; 
            for k = 1:Ntimes; 
                n = n+1;
                Sindex(k,i,j) = n;
            end
        end
    end
    %fprintf('Deleting the nonexistent states # %d-%d out of %d',Sindex(1,Nactions,2),Sindex(Ntimes,Nactions,2),n)
    for k = 1:Ntimes;
        Sindex(k,Nactions,2) = NaN; n=n-1;    % as the state of having a reward after NP does not exist
    end
    Slog = Sindex(1,1,1);                     % the state for which to log parameter changes
end
Nstates = n;

Bait = zeros(1,Ndata);
% Determining the rewards for RI schedules in advance
if (strcmpi(schedule,'interval'))
    if (length(Reward_times) == Ndata)        % use previous reward times
        Bait = Reward_times;
    else
        if (strcmpi(schedule_type,'random'))  % decide when will we get rewards in each trial, truncated at 3*mean interval
            for i = 1:Ndata                   % RI: do this manually to avoid using the stats toolbox
                a = rand*10*interval;
                while (rand>(exp(-a/interval)/interval))
                    a = rand*10*interval;
                end
                Bait(i) = a;
            end
        else                                  % VI: start with CRF then VI2 then VI15*2 and then VI30 (between 15 and 45 seconds) thereafter
            Bait = [ones(1,30)*1/dt rand(1,30)*2+1 rand(1,60)*15+7.5 rand(1,Ndata-120)*30+15];
            % Bait = rand(1,Ndata)*30 + 15;
        end
    end
end

fprintf('\n%s %s\n',schedule_type,schedule)

% -----------------------------------------
% Initializing the main loop parameters
% -----------------------------------------

% first we need to initialize the state for the start of the session
i_r = 1;           % no reward
a_prev = Nactions; % just nosepoked
t_s = 1;           % whatever this means (depending on the schedule)

Temp = 7;                                  % initial temperature of the softmax for action choice
minTemp = 0.5;                            % miminal temperature to decay to
etaQ = 0.05;                               % learning rate for Q learning
etaV = 0.05;                               % learning rate for V learning
etaM = 0.01;                               % learning rate for learning M parms
etaA = 0.01;                               % learning rate for learning Alpha parms
etaB = 0.01;                               % learning rate for learning Beta parms
etaR = 0.0005; logEtaR = log(etaR);        % learning rate for average reward (should be slow compared to all else)
minA = 1.05; minB = 0.05;                  % constrains on minimum values of Alpha and Beta parms
Rmin = 0.01;                               % Minimum average reward (higher emulates more hunger at start of training)
TempDecayRate = 0.9997;                    % rate of decay of Temp
EtaDecayRate = 0.999;                     % rate of decay of learning rate
Mdecay = 1;                                % rate of decay to 0 of Mixture parms -- set to 1 to prevent decay
N_R = 10000;                               % number of last actions (not trials!) to compute R according t

% to continue a run from the previous end point - don't initialize the parms below
if (learn)
    V = zeros(1,Nstates);                      % Initial V values
    dV = zeros(size(V)); 
    R = Rmin;                                  % initial average reward
    % R_Costs = zeros(1,N_R);                    % to cyclically log costs of last N_R actions
    % R_Latencies = zeros(1,N_R);                % to cyclically log latencies
    if ~(strcmpi(LearnMode,'DirectActor')) 
        Q = zeros(Ntimes,Nactions,Nstates);    % Initial Q values
        dQ = zeros(size(Q));
        L_Q = Ntimes*Nactions;                 % the length of the Q vector when in a specific state
    else
        if (Mlearning)
            M = ones(Nactions,Nstates);        % Parametrized policy: equal mixture components
            % dM = zeros(size(M));
        else
            Q = zeros(Nactions,Nstates);       % Initial Q values for each state and action 
            dQ = zeros(size(Q));
        end
        Alpha = times(end)/3*ones(Nactions,Nstates); % Parametrized policy: the event number to wait for
        %dA = zeros(size(Alpha)); dB = zeros(size(Beta)); 
        Beta  = ones(Nactions,Nstates);        % Parametrized policy: Poisson rate of 1
        if ~(constrained)                      % Implicitly constrained version: remap parameters to new range
            if (Mlearning)
                M = log(M);
            end
            Alpha = 1*ones(Nactions,Nstates);  % a good starting point is 2;   1 will give a nice initial spread 
            Beta = -1.5*ones(Nactions,Nstates);% a good starting point is -7; -1.5 will give a nice initial spread
        end
    end
end

cum_count = 0; Reward = 0; Nresponses = 0; 
t = 0; T = 0; N_session = 1; a = 0; Nhalfway = 0; deltas = [];
DM = zeros(1,Nactions); DAlpha = 0; DBeta = 0;
if (strcmpi(schedule,'interval'))
    initlength = Ndata*20;
else
    initlength = Ndata*ratio*2;
end
Responses = zeros(1,initlength);    % to log responses chosen
Times = zeros(1,initlength);        % to log times of responding
Latencies = zeros(1,initlength);    % to log latencies of actions chosen
EatTimes = zeros(1,initlength);     % to log how long we spend eating
EatTrials = zeros(1,initlength);    % th log when we spend time eating
Deltas = zeros(1,initlength);       % to log temporal difference errors 
Rewards = zeros(1,Ndata); 
Tconsume = zeros(1,Ndata); 
RateLP = zeros(1,times(end)*2*dt/5);      % binning of action rates
BaitNorm = min(Bait,times(end)*2);
Session_lp = zeros(1,Nsessions+1);
R_log = zeros(1,initlength); R_log_real = []; Rreal = 0; % to log the R_avg during learning 
if strcmpi(LearnMode,'DirectActor')
    LogBeta = zeros(1,initlength);
    LogAlpha = zeros(1,initlength);
    LogM = zeros(Nactions,initlength);
    AlphaInt = 1; BetaInt = 1; 
end
LogS = zeros(1,initlength);
if (strcmpi(schedule_type,'random') && strcmpi(schedule,'ratio'))
    LogV = zeros(Nstates,initlength);
else
    LogV = zeros(1,initlength);
end

if strcmpi(LearnMode,'OnPolicyQ')          % for the first iteration we need this
    Qtemp = reshape(Q(:,:,Sindex(t_s,a_prev,i_r)),1,L_Q);
    Qtemp = Qtemp - max(Qtemp);       
    SoftQtemp = exp(Qtemp/Temp)/sum(exp(Qtemp/Temp)); 
end

Wsmooth = 1 - [1:Nsmooth]/(Nsmooth+1);     % the weights to use for smoothing (updating neighboring Qs)
Wsmooth = [Wsmooth(end:-1:1) 1 Wsmooth]';

RunAborted = 0;

%-----------------------------------------------
% THE MAIN LOOP - behave; observe; update
%-----------------------------------------------

for trial = 1:Ndata
    
    if ~mod(trial,1000)
        fprintf('\n ---> working... already at trial %d',trial)
    end
    
    if strcmpi(LearnMode,'DirectActor') && learn
        % Update the Vs and Qs according to the updates we have accumulated
        V = V + etaV*dV;
        dV = dV*0;
        %%%V = V - mean(V);  % commented 26/6/07
        %Beta = Beta + etaB*dB;
        %Alpha = Alpha + etaA*dA;
        %M = M + etaM*dM;
        %dA = dA*0; dB = dB*0; dM = dM*0;
        
        if ~(Mlearning)
            % update the Qs according to the deltas we have accumulated
            Q = Q + etaQ*dQ;
            dQ = dQ*0;
            Q = Q - max(max(Q)); % the Qs are independent of the Vs so we can normalize them by themselves
        end
    end
    
    if strcmpi(LearnMode,'HybridSARSA')
        V = V + etaV*dV;
        g = max(V);
        V = V - g;                        % setting one V to zero to give baseline
        Q = Q - g;                        % and as the Qs are all dependent on the values, they should be updated too
        Q = Q + etaQ*dQ;
        dV = dV*0; dQ = dQ*0;
    end
    
    if (trial == Ndata/2)  
        Nhalfway = Nresponses; % we'll use this later to get a cleaner measure of performance
    end
    
    rewarded = 0;
    t = 0;
    num_actions = 0;
    
    while (~rewarded && num_actions<max_actions)
        
        % BEHAVE: choose an action to emit according to softmax, or to the
        % ------  parametrized policy
        
        S = Sindex(t_s,a_prev,i_r);
        i_prev = i_r; t_prev = t_s;   % se we can know what these were later
        
        if (strcmpi(LearnMode,'DirectActor'))
            % choose action by drawing from parametrized policy: first
            % decide on the action based on mixture probs M: 
            if (constrained)
                action = find((rand*sum(M(:,S)))<cumsum(M(:,S)));
                action = action(1);
                % Now, given the action, draw the latency from the corresponding gamma distribution
                % fprintf('\n Drawing a latency from gamma(%3.4f,%3.4f)',Alpha(action,S),Beta(action,S))
                tau = gamrnd(Alpha(action,S),Beta(action,S)); 
                gammean = Alpha(action,S)*Beta(action,S);  % we'll use these to detect outliers later
                gamstd = (Alpha(action,S)^.5)*Beta(action,S);
            else                                % unconstained parameters -- the probability function p(a,tau) is different 
                if Mlearning
                    action = find(rand<cumsum(exp(M(:,S))/sum(exp(M(:,S)))));
                    action = action(1);
                    if (isempty(action))            % the exponent overflowed
                        [x,action] = max(M(:,S));   % its obvious what we want to do   
                    end
                else
                    % choose the action identity based on Q values that gloss over taus
                    Qtemp = exp((Q(:,S)-max(Q(:,S)))/Temp);
                    action = find(rand<cumsum(Qtemp/(sum(Qtemp))));
                    action = action(1);
                end
               
                % given this action determine the latency
                Atilda = exp(Alpha(action,S))+minA;  % we'll also be able to use these later when we update Alpha and Beta
                Btilda = exp(Beta(action,S))+minB;
                if (includeR)                   % policy parametrized using the average reward (for tonic DA)
                    % Btilda = Btilda/(1+R*Btilda);
                    Btilda = Btilda/(R^.5);
                end
                tau = gamrnd(Atilda,Btilda);
                gammean = Atilda*Btilda;        % we'll use these to detect outliers below
                gamstd = (Atilda^.5)*Btilda;
            end
            tau = max(tau,1/dt);                % to make sure we don't get extra small taus
            tau = min(tau,max(3*gamstd+gammean,200));  % to make sure we don't get extra long taus (larger than 3std from mean)
            %tau = min(tau,120);                 % just to make double sure - cap at two minutes
            if (tau>200)
                if (constrained)
                    fprintf('\nStep %d: Exceptionally long tau (%3.2f) for action %d and state (%d,%d,%d),A=%3.2f,B=%3.2f',Nresponses+1,tau,action,t_s,a_prev,i_r,Alpha(action,S),Beta(action,S));
                else
                    fprintf('\nStep %d: Exceptionally long tau (%3.2f) for action %d and state (%d,%d,%d),A=%3.2f,B=%3.2f',Nresponses+1,tau,action,t_s,a_prev,i_r,Atilda,Btilda);
                end
            end
            tau = tau*dt;                       % to use the same 'units' as the Q-derived actions
        else
            % choose actions by softmax over Qs
            if ~strcmpi(LearnMode,'OnPolicyQ')
                Qtemp = reshape(Q(:,:,S),1,L_Q);
                Qtemp = Qtemp - max(Qtemp);     % as softmax doesn't care about absolute values and to replace overflow with underflow
                index = find(rand<cumsum(exp(Qtemp/Temp)/sum(exp(Qtemp/Temp))));
            else          % in on policy we already computed a lot in the update step
                index = find(rand<cumsum(SoftQtemp));
            end

            action = ceil(index(1)/Ntimes);
            tau    = index(1) - (action-1)*Ntimes;
        end
        
        % we are committed to the action -- advance time accordingly
        t = t + tau./dt;                % counting time within a trial
        T = T + tau./dt;                % counting time within a session

        % OBSERVE: what does the world gives us in return?
        % -------

        Reward = 0;

        if (i_r == 2) % when a reward is waiting, the schedule is suspended and all we can do is wait for the consumption
            if (action == Nactions)             
                Reward = 1;          % consume the reward
                i_r = 1;
            end
            if (strcmpi(schedule,'interval'))
                if (strcmpi(schedule_type,'variable'))    % VI
                    t_s = min(t_s + ceil(tau),Ntimes);    % time from previous reward moves even when the schedule is suspended
                else                                      % RI
                    if (action ~= 1)
                        t_s = min(t_s+ceil(tau),Ntimes);  % time from previous LP moves even when the schedule is suspended
                    end
                end
            end
        else          % run schedule to see what happens
            if (action == 1)         % we've lever pressed
                if (strcmpi(schedule_type,'random') || strcmpi(schedule_type,'variable'))
                    if (strcmpi(schedule,'ratio'))        % RR - don't care about tau, t_s doesn't change
                        cum_count = cum_count + 1;        % to ensure we don't press more than 3*ratio for a reward
                        if (rand < 1/ratio || cum_count == 3*ratio) 
                            i_r = 2; rewarded = 1;        % get reward
                            Bait(trial) = t;
                            cum_count = 0;
                        end
                    else                                  % RI/VI - baiting already determined
                        if (t > Bait(trial))
                            i_r = 2; rewarded = 1;        % get reward
                        end
                        t_s = 1;                          % we've just leverpressed
                    end
                else
                    if (strcmpi(schedule_type,'fixed'))   % FR is the only option here...
                        if (t_s == ratio)                 % we've pressed enough
                            i_r = 2; rewarded = 1;
                            Bait(trial) = t;
                            t_s = 1;
                        else
                            t_s = t_s + 1;                % continue counting toward the reward
                        end
                    else                                  % VI is the only option left
                        if (t > Bait(trial))
                            i_r = 2; rewarded = 1;        % get reward
                            t_s = 1;                      % we've just been rewarded
                        else
                            t_s = min(t_s + ceil(tau),Ntimes); 
                        end
                    end
                end
            else                      % nosepoke or 'other'
                if (strcmpi(schedule,'interval'))
                    t_s = min(t_s + ceil(tau),Ntimes);
                end
            end
        end

        % DOCUMENT what just happened
        % --------

        Nresponses = Nresponses+1;      % overall response count
        Responses(Nresponses) = action;
        Latencies(Nresponses) = tau/dt;
        Times(Nresponses) = t;
        num_actions = num_actions + 1;  % to avoid runaways
        EatTrials(Nresponses) = 0; 
        EatTimes(Nresponses) = 0;       % we are not eating in this trial (so far at least)
        
        if (Reward)                     % we just consumed a reward
	    % fprintf('\nWHOOPY!!! just ate a reward\n')
            EatTrials(Nresponses) = 1;    % now we eat
            % Eat = EatTime/2+rand*EatTime; % variable eating times
            Eat = EatTime;                % constant eating times (to check for possible bug)
            EatTimes(Nresponses) = Eat;   % remember how long we consumed here (for average reward calculations)
            Tconsume(trial-1) = t;        % keep track of latency to consumption
            t = t + Eat;                  % advance time according to the eating time
            T = T + Eat;
            if strcmpi(schedule,'interval') % this is actually yet another part of the state update
                t_s = min(t_s + round(Eat*dt),Ntimes);
            end
        end
                
        % keep track of LPs for interval schedules only when reward is not baited to prevent confound
        if (action == 1)
            Session_lp(N_session) = Session_lp(N_session) + 1; 
            if (strcmpi(schedule,'interval') && t < BaitNorm(trial) && Nhalfway)  % do this only for the second half of training
                RateLP(ceil(t*dt/5)) = RateLP(ceil(t*dt/5)) + 1;
            end
        end

        if (rewarded)                   % we just got a reward
            % fprintf('getting GOOD... we just got a reward\n')
            Rewards(trial) = t;
        end
        
        % LEARN: update our Vs and Qs or Ms Alphas and Betas accordingly, as well as R (the average reward)
        % -----
        if (learn)                            % to simulate DA depletion without learning put a 0 here
        
        Cost = Reward*Utility + rho(action) - k_v(a_prev,action)/(tau/dt);
        timeInt = exp(log(1-exp(-etaR*(tau/dt+EatTimes(Nresponses))))-logEtaR); % the integral over the decay of the average reward rate between from the time of the previous action
        
        if (strcmpi(LearnMode,'DirectActor')) % Direct learning of the policy parameters
            % CRITIC
            delta  = Cost - timeInt*R + V(Sindex(t_s,action,i_r)) - V(S);
            % once in a while we get a crazily large PE because of a very
            % low probability or very delayed action: we want to know about this
            if (abs(delta)>2*Utility)
                fprintf('\nUnusually large PE (%3.2f) at trial %d timestep %d: state #%d action %d, latency %3.2f',delta,trial,Nresponses,S,action,tau/dt);
                fprintf('\n PE breakdown: (Cost) %3.2f - (latency) %3.2f*%3.2f (R) + (V(%d,%d,%d)) %3.2f - (V(%d,%d,%d)) %3.2f',Cost,tau/dt+EatTimes(Nresponses),R,t_s,action,i_r,V(Sindex(t_s,action,i_r)),t_prev,a_prev,i_prev,V(S));
                % delta = min(delta,2*Utility); delta = max(delta,-2*Utility);
            end
            % debug: print out whenever there is an eating step
            %if (Nhalfway)
            %if Reward                  % look at the time of consumption (updates the state 1,1,2)
            %if (S==2)  % look at the time right before eating (updates the state 1,1,2)    
            %    fprintf('\nStep %d: (t(%d)=%3.2f), Cost = (%3.2f)-(%3.2f) - R*t=%3.2f*%3.2f + V(%d,%d,%d)=%3.2f - V(now)=%3.2f, delta=%3.2f --> V(new) = %3.2f',...
            %        Nresponses,action,tau/dt,Reward*Utility+rho(action),k_v(a_prev,action)/(tau/dt),...
            %        R,timeInt,t_s,action,i_r,V(Sindex(t_s,action,i_r)),V(S),delta,V(S)+etaV*delta);
            %end
            %end

            if ~(Mlearning)
                % instead of Ms we will learn Q values for each action at each
                % state, to prevent maximizing by performing one action
                % exclusively
                deltaQ = Cost - timeInt*R + max(Q(:,Sindex(t_s,action,i_r))) - Q(action,S);
                dQ(action,S) = dQ(action,S) + deltaQ;
            end
            
            % rather than updating here, we will accumulate updates and do
            % this once per trial, to avoid inaccuracies because of recurrent states
            % V(S)   = V(S) + etaV*delta;
            % V = V - max(V);
            dV(S) = dV(S) + delta;  
            
            % fprintf('\n %3.2f\t %3.2f\t %3.2f\t %3.2f\t %3.2f\t, updated state %d',V,S); 
            
            Deltas(Nresponses) = delta;       % documenting the TD error
            
            % ACTOR:
            
            % here we cap the PE to avoid perturbing the policy too much and losing stability 
            % if (abs(delta)> 2*Utility)
            %     delta = min(delta,2*Utility);
            %     delta = max(delta,-2*Utility);
            % end
            
            if (constrained)                  % impose constraints on parameter ranges manually
                % first we decide by how much we need to change each parameter:
                DAlpha = (log(tau/(dt*Beta(action,S)))-psi(Alpha(action,S))); % psi = digamma function = d(log(Gamma(x)))/dx
                DBeta  = 1/Beta(action,S)*(tau/(dt*Beta(action,S))-Alpha(action,S));
                % now updating both at once:
                Alpha(action,S) = Alpha(action,S) + etaA*delta*DAlpha;
                Beta(action,S)  = Beta(action,S) + etaB*delta*DBeta;
                % same for the Ms:
                for i = 1:Nactions
                     DM(i) = (i==action)/M(action,S)-1/sum(M(:,S));
                end
                M(:,S)  = M(:,S) + etaM*delta*DM';
                % imposing range constraints by brute force
                Alpha(action,S) = max(Alpha(action,S),1.05); % to leave us in the range in which Gamma is not only descending
                Beta(action,S) = max(Beta(action,S),0.05);   % to avoid too narrow distributions (maintain variability)
                % together these constraints mean that the mean of the Gamma is at least 0.3 and the peak at least 1/Beta(Alpha-1) = 0.1
                M(:,S) = max(M(:,S),0);                     % M must be non-negative (mixing proportion) 
            else                                            % parametrize with the constraints inside
                % the internal derivatives for each:
                Stabilize = -abs(Alpha(action,S))-abs(Beta(action,S));
                AlphaInt = exp(Alpha(action,S)+Stabilize); % Stabilize is used to avoid large abs value paramters
                BetaInt = exp(Beta(action,S)+Stabilize);
                if (includeR)                               % there is another internal derivative to use here                       
                    % BetaInt = BetaInt/((1+R*(exp(Beta(action,S))+minB))^2); % this turned out to be wrong
                    BetaInt = BetaInt/(R^.5);
                end
                DAlpha = (log(tau/(dt*Btilda))-psi(Atilda)); % psi = digamma function = d(log(Gamma(x)))/dx
                DBeta = 1/Btilda*(tau/(dt*Btilda)-Atilda);
                % now updating both at once:
                % fprintf('%d:(%d,%3.2f), Cost %3.2f, R %3.4f, Beta is %3.2f, DBeta is %3.2f, delta is %3.2f\n',trial,action,tau/dt,Cost,R,Beta(action,S),DBeta*BetaInt,delta);
                if (abs(delta)>2*Utility)
                    fprintf('\nGradients: dA=%3.2f*Aint=%3.2f,dB=%3.2f*Bint=%3.2f, delta=%3.2f',DAlpha,AlphaInt,DBeta,BetaInt,delta);
                    fprintf('\n this is because (A) log(tau/B)=%3.2f and psi(A)=%3.2f, and (B) tau/B=%3.2f-A=%3.2f\n',log(tau/(dt*Btilda)),psi(Atilda),tau/(dt*Btilda),Atilda);
                end
                %if (trial>2000)
                %    fprintf('\nS(%d,%d,%d) Chose (a,tau)=(%d,%3.2f) (mean=%3.2f), delta=%3.2f, dA=%3.2f, dB=%3.2f',t_prev,a_prev,i_prev,action,tau/dt,Atilda*Btilda,delta,DAlpha,DBeta);
                %end
                %dA(action,S) = dA(action,S) + AlphaInt*DAlpha*delta;
                %dB(action,S) = dB(action,S) + BetaInt*DBeta*delta;
                Alpha(action,S) = Alpha(action,S) + etaA*AlphaInt*delta*DAlpha;
                Beta(action,S)  = Beta(action,S) + etaB*BetaInt*delta*DBeta;
                if (Mlearning)
                    % same for the Ms (and also rescaling by exp(-M)):
                    DM = ([1:Nactions]'==action) - exp(M(:,S))./sum(exp(M(:,S))); 
                    M(:,S) = M(:,S) + etaM*delta.*DM;               % heterosynaptic updating
                    % dM(:,S) = dM(:,S) + DM*delta;
                    % To induce exploration we may want to decay the mixture weights
                    % M(:,S) = Mdecay*M(:,S);
                    % M(action,S) = M(action,S) + etaM*delta*DM(action); % homosynaptic updating to maintain more exploration?
                end
                if (Alpha(action,S)>700 || Beta(action,S)>700 || M(action,S)>700)
                    RunAborted = 1;
                    fprintf('\nSorry... run aborted to prevent overflow. Try again...\n') 
                    fprintf('The problem is with state %d (%d,%d,%d), action %d\n',S,t_s,a_prev,i_r,action)
                    return
                end
            end
            %fprintf('\n Updated: state %d, delta %3.2f, action %d, latency %3.2f',S,delta,action,tau/dt)
            %fprintf('\n          new state values : %3.4f %3.4f %3.4f',V)
            %fprintf('\n          updates  : M - %3.2f %3.2f A - %3.2f B - %3.2f',DM,DAlpha,DBeta)
            %fprintf('\n          new parms: M - %3.2f %3.2f A - %3.2f B - %3.2f\n',M(:,S),Alpha(action,S),Beta(action,S))      
            
            LogS(Nresponses) = S;
            if (Mlearning)
                LogM(:,Nresponses) = M(:,S);
            else
                LogM(:,Nresponses) = Q(:,S);  % save the Q values here instead (remember, these are batch-updated only once a trial)
            end
            LogBeta(Nresponses) = Beta(1,S);
            LogAlpha(Nresponses) = Alpha(1,S);
            if (strcmpi(schedule_type,'random') && strcmpi(schedule,'ratio'))
                LogV(:,Nresponses) = V'; % we can save all the Vs in random ratio
            else
                LogV(Nresponses) = V(Slog);
            end
        end
        
        if (strcmpi(LearnMode,'HybridSarsa')) % Q learning using values
            delta = Cost - timeInt*R + V(Sindex(t_s,action,i_r));   % NOTE: this is not the regular form of delta -- see below
            dV(S) = dV(S) + delta - V(S);
            if (tau < Nsmooth + 1 || tau+Nsmooth > Ntimes)
                dQ(tau,action,S) = dQ(tau,action,S) + delta - Q(tau,action,S);
            else
                dQ(max(tau-Nsmooth,1):tau+Nsmooth,action,S) = dQ(max(tau-Nsmooth,1):tau+Nsmooth,action,S) ...
                    + (delta - Q(tau,action,S))*Wsmooth;
            end
            V(S) = (1-etaV) * V(S) + etaV*delta;                    % same as V = V + eta(delta-V)
            %g = max(V);
            %V = V - g;                        % setting one V to zero to give baseline
            %Q = Q - g;                        % and as the Qs are all dependent on the values, they should be updated too
            %delta = delta-g - Q(tau,action,S);% of course we should update the delta as it has the Vs in it, and here we will convert it to the normal form too
            %if (tau < Nsmooth + 1 || tau+Nsmooth > Ntimes)
            %    Q(tau,action,S) = Q(tau,action,S) + etaQ*delta;
            %else                              % For smoothing update some neighboring Qs too
            %    Q(max(tau-Nsmooth,1):tau+Nsmooth,action,S) = Q(tau-Nsmooth:tau+Nsmooth,action,S) + etaQ*Wsmooth*delta; 
            %end
            Deltas(Nresponses) = delta-Q(tau,action,S);       % documenting the TD error
        end
        
        if (strcmpi(LearnMode,'OffPolicyQ'))  % Off policy Q learning
            delta = -Q(tau,action,S) + Cost - (tau/dt+EatTimes(Nresponses))*R + max(max(Q(:,:,Sindex(t_s,action,i_r))));
            Q(tau,action,S) = Q(tau,action,S) + etaQ*delta;
            Q = Q - max(max(max(Q)));         % normalizing as additive constants don't matter
            Deltas(Nresponses) = delta;       % documenting the TD error
        end 
        
        if (strcmpi(LearnMode,'OnPolicyQ'))   % On policy Q learning
            Qtemp = reshape(Q(:,:,Sindex(t_s,action,i_r)),1,L_Q);
            Qtemp2 = Qtemp - max(Qtemp);      % as softmax doesn't care about absolute values and to replace overflow with underflow
            SoftQtemp = exp(Qtemp2/Temp)/sum(exp(Qtemp2/Temp));    % we'll actually be able to use this for the next action choice
            delta = -Q(tau,action,S) + Cost - (tau/dt+EatTimes(Nresponses))*R + SoftQtemp*Qtemp';
            Q(tau,action,S) = Q(tau,action,S) + etaQ*delta;
            Q = Q - max(max(max(Q)));         % normalizing as additive constants don't matter
            Deltas(Nresponses) = delta;       % documenting the TD error
        end 
        
        % fprintf('Q error was %5.4f, new Q is %5.4f\n',delta,Q(tau,action,S))

        % we have to be careful here to not fall prey to the falacy of the
        % averages: the right way to compute R(i) (after the ith action)
        % from R(i-1) is R(i)=[R(i-1)*T(i-1)+(rewards-costs)]/T(i). But
        % we will compute a local average of the last N_R actions, as we
        % want R to be policy dependent
        
        % Aha.. according to Singh we only have to use greedy actions. But
        % -- this is not true with softmax, and even wrong. So we use the
        % on policy average...

        % -----------------------------------------------------------------
        % compute R incrementally as should be for a real online learning algorithm
        % -----------------------------------------------------------------
        Rold = R;
        R = R*exp(-etaR*(Latencies(Nresponses)+EatTimes(Nresponses)))+etaR*Cost;
        
        if abs(R-Rold)>0.2
            fprintf('\n Trial %d: latency is %3.4f cost is %3.4f Beta is %3.4f, R changed by %3.2f',trial,tau/dt,Cost,Btilda,R-Rold);
        end
        
        % just to make sure we are doing it right (and for comparison with
        % model-based Q learning) compute the average reward in the last
        % N_R actions (should this be trials?)
        % a = a+1; if (a>N_R); a=1; end
        % R_Costs(a) = Cost;
        % R_Latencies(a) = Latencies(Nresponses)+EatTimes(Nresponses);
        % if (Nresponses>N_R)     
        %    Rreal = sum(R_Costs)/sum(R_Latencies);
        % end
        % R_log_real(Nresponses) = Rreal;
        
        % It turns out that very small Rs (0 or -ve) are bad because they encourage laziness 
        % in the beginning, which is especially bad for interval schedules -- 
        % choose late actions and update to high Qs and then when R is
        % already known they are too high and the learning rate too low to
        % now reduce them properly
        R = max(R,Rmin);         % to avoid negative R before first reward which encourages laziness, and to simulate a 'hungry' rat
	    % fprintf('updated average reward is %5.4f\n',R)
        R_log(Nresponses) = R;
        
        end                      % up to here if(learn) 
        
        a_prev = action;         % this is one last part of the state update

        % check for end of 30 minute session (like Salamone)
        if (T > 30*60)             
            N_session = N_session + 1;
            T = 0;
        end
        
    end

    % A trial has just ended -- right before the next trial:
     
    % If we have reached the number of sessions we want then we can stop here
    if (N_session > Nsessions)
        Rewards(trial:end) = [];
        Tconsume(trial:end) = [];
        Ndata = trial;
        return
    end
    
    % If we have made far too many actions - stop here for sake of run time
    if (num_actions == max_actions) % runaway trial -- abort
    % if (Nresponses > max_actions*trial)
        fprintf('\nRun stopped after %d trials because of too many (%d) actions and no reward',trial,Nresponses)
        fprintf('\n the last %d responses were:',max_actions)
        fprintf('\n %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d',Responses(Nresponses-max_actions+1:Nresponses))
        Nacts_per_trial = Nresponses/trial;
        RunAborted = 1;
        return
    end
    
    % decay the learning and action parameters properly
    if ~(strcmpi(LearnMode,'DirectActor'))
        Temp = Temp*TempDecayRate; Temp = max(minTemp,Temp);
        etaQ = etaQ*EtaDecayRate;
        etaV = etaV*EtaDecayRate;
    %else
        %Temp = Temp*TempDecayRate; Temp = max(minTemp,Temp);
        %etaM = etaM*EtaDecayRate;
        %etaA = etaA*EtaDecayRate;
        %etaB = etaB*EtaDecayRate;
        %etaV = etaV*EtaDecayRate;
    end
    
end

%----------------------------------------------------------------
% Generate some statistics of the data to see what we have gotten
%----------------------------------------------------------------

% we'll do all these for the second half of training, to not be penalized
% by early learning trials:
Halfdata = ceil(Ndata/2);
% compute mean time to obtain reward 
Reward_summary = [mean(Rewards(Halfdata:Ndata-1)),std(Rewards(Halfdata:Ndata-1))];
% compute mean time to consumption
Consump_summary = [mean(Tconsume(Halfdata:Ndata-1)),std(Tconsume(Halfdata:Ndata-1))];
% compute # actions per trial in second half
Nacts_per_trial = (Nresponses-Nhalfway)/(trial/2);
% latencies to lever press
L_lp = Latencies(Nhalfway + find(Responses(Nhalfway+1:end) == 1));
% latencies to nose poke -- we will differentiate between the consumption
% ones and the spurious ones
L_np       = Latencies(Nhalfway + find(EatTrials(Nhalfway+1:end))); % Couldn't eat without a NP
L_np_extra = Latencies(Nhalfway + find((Responses(Nhalfway+1:end) == Nactions) & ~EatTrials(Nhalfway+1:end)));
% actual interval between reward baitings 
bait_interval = mean(Bait(1:Ndata));

% in order to compute lever pressing rate only when reward was not baited,
% we need to normalize by the number of trials that reached each point:
bins = [0:1/(dt/5):(round((Reward_summary(1)+2*Reward_summary(2))/dt)*dt)];  % draw until mean+2std time to reward
nrm = [Halfdata-cumsum(histc(Bait(Halfdata+1:Ndata),bins))];
nrm(end)    = [];  % the last one is always meaningless as we used histc
bins(nrm==0)= [];
nrm(nrm==0) = []; % to avoid dividing by zero later

% compute rate for each action as function of time within a trial
% ---------------------------------------------------------------
if strcmpi(schedule,'ratio')
    LPs = histc(Times(Nhalfway+find(Responses(Nhalfway+1:end)==1)),bins);
    N = sum(nrm>Ndata/5);
    LPs = LPs(1:N)*60*dt*.2./nrm(1:N);
else
    % for interval schedules we only look at unbaited times - we collected this earlier
    RateLP(length(nrm):end) = [];  % we may have started out with more bins than we really need
    LPs = (RateLP*60*dt*.2)./nrm(1:length(RateLP));
end
if (Nactions > 2 && sum(Responses(Nhalfway+1:end)==(Nactions-1))) % only do this if there are any Other responses
    Others = histc(Times(Nhalfway+find(Responses(Nhalfway+1:end)==(Nactions-1))),bins)*60*dt*.2;
    Others = Others(1:end-1)./nrm;
end
NPs = histc(Times(Nhalfway+find(Responses(Nhalfway+1:end)==Nactions)),bins)*60*dt*.2;
NPs = NPs(1:end-1)./nrm;

% shrink Log vectors to their proper size
LogBeta(Nresponses+1:end)=[];
LogAlpha(Nresponses+1:end)=[];
LogV(:,Nresponses+1:end)=[];
LogM(:,Nresponses+1:end)=[];
LogS(Nresponses+1:end)=[];
Responses(Nresponses+1:end)=[];
Times(Nresponses+1:end)=[];
Latencies(Nresponses+1:end)=[];
Deltas(Nresponses+1:end)=[]; 
R_log(Nresponses+1:end)=[];
EatTimes(Nresponses+1:end)=[];
EatTrials(Nresponses+1:end)=[];

% Plot results if we were asked to do this
% ----------------------------------------

if (figures)
    
    % Plotting rate of actions as function of time within a trial
    % -----------------------------------------------------------
    figure; 
    x = bins(1:end-1)+.5;  % these are the middle points of the bins
    h = plot (x(1:length(LPs)),LPs,'b*-',x,NPs,'r*:');
    hold on
    set(gca,'FontSize',14); set(h,'LineWidth',2);
    if (Nactions>2 && sum(Responses(Nhalfway+1:end)==(Nactions-1)))
        h = plot(x,Others,'go-'); set(h,'LineWidth',2);
    end
    title (sprintf('R_{avg}=%3.2f, <Trial length> = %3.2fsec',R,Reward_summary(1)));
    
    ylabel('response rate per min in 2^{nd} half of training'); xlabel('time (sec) after reward');
    if (Nactions > 2 && sum(Responses(Nhalfway+1:end)==(Nactions-1))) 
        legend('LP','NP','Other',0)
    else
        legend('LP','NP',0)
    end
    % v = axis; v(2) = length(LPs); axis(v); v(4) = ceil(max(LPs)); 
    %text(v(2)./12,v(4)*0.8,['\rho = ',sprintf('%3.2f,',rho),' k_v(same) = ',sprintf('%3.2f,',k_v(1,1)),' k_v(diff) = ',sprintf('%3.2f,',k_v(1,2)),...
    %    sprintf('\n\tU = %d, T = %3.2f',Utility,Temp)]);
    box off
    set(gcf,'Paperpositionmode','auto')
    
    % plotting the distribution of latencies of the different actions
    % ---------------------------------------------------------------
%     figure;
%     x = [.5/(dt/10):1/(dt/10):times(end)-.5/(dt/10)];
%     n1 = hist(L_lp,x)/length(L_lp);
%     % we will differentiate between consumption NPs and spurious NPs
%     n3 = hist(L_np,x)/length(L_np);
%     if (Nactions>2)    
%         L2 = Latencies(Nhalfway + find(Responses(Nhalfway+1:end) == (Nactions-1)));
%         n2 = hist(L2,x)/length(L2);
%         if (length(L_np_extra))
%             n3_extra = hist(L_np_extra,x)/length(L_np_extra);
%             h = plot(x,n1,'*-b',x,n2,'o-g',x,n3,'*:r',x,n3_extra,'*:k');
%             set(gca,'FontSize',14); set(h,'LineWidth',2);
%             legend('LP','Other','NP','spurious NP')
%             title(sprintf('Mean latency: LP %3.2fs, Other %3.2fs, NP %3.2fs, spurious NPs %3.2fs',mean(L_lp),mean(L2),mean(L_np),mean(L_np_extra)))
%         else
%             h = plot(x,n1,'*-b',x,n2,'o-g',x,n3,'*:r');
%             set(gca,'FontSize',14); set(h,'LineWidth',2);
%             legend('LP','Other','NP')
%             title(sprintf('Mean latency: LP %3.2fs, Other %3.2fs, NP %3.2fs',mean(L_lp),mean(L2),mean(L_np)))
%         end
%     else
%         if (length(L_np_extra))
%             n3_extra = hist(L_np_extra,x)/length(L_np_extra);
%             h = plot(x,n1,'*-b',x,n3,'*:r',x,n3_extra,'*:k');
%             set(gca,'FontSize',14); set(h,'LineWidth',2);
%             legend('LP','NP','spurious NP')
%             title(sprintf('Mean latency: LP %3.2fs, NP %3.2fs, spurious NPs %3.2fs',mean(L_lp),mean(L_np),mean(L_np_extra)))
%         else
%             h = plot(x,n1,'*-b',x,n3,'*:r');
%             set(gca,'FontSize',14); set(h,'LineWidth',2);
%             legend('LP','NP')
%             title(sprintf('Mean latency: LP %3.2fs, NP %3.2fs',mean(L_lp),mean(L_np)))
%         end 
%     end
%     xlabel('latency to actions (in second half of training)')
%     ylabel('probability')
%     v = axis; v(2) = times(end); v(4) = max(0.5,v(4)); axis(v);
%     box off
%     set(gcf,'Paperpositionmode','auto')

    if (strcmpi(LearnMode,'DirectActor'))
        % plotting the policy
        % -------------------
        figure;
        S = [];                                 % these will be the states for which we will plot the poliy
        if strcmpi(schedule,'interval')         % RI
            S = [Sindex(1,1,1),Sindex(1,1,2)];
        else
            if strcmpi(schedule_type,'Fixed')   % FR
                S = [Sindex(1,2,1),Sindex(2:end,1,1)',Sindex(1,1,2)];
            else                                % RR   
                S = [1:Nstates];
            end
        end
        for i = 1:length(S)
            if (length(S) < 5)
                subplot(length(S),1,i); hold on
            else
                subplot(ceil(length(S)/3),3,i); hold on
            end
            if (constrained)
                Atilda = Alpha(:,S(i)); 
                Btilda = Beta(:,S(i));
                Mtilda = M(:,S(i))/sum(M(:,S(i)));
            else
                Atilda = exp(Alpha(:,S(i)))+minA; 
                Btilda = exp(Beta(:,S(i)))+minB;
                if (Mlearning)
                    Mtilda = exp(M(:,S(i)))/sum(exp(M(:,S(i))));
                else
                    Mtilda = Q(:,S(1));
                end
                if (includeR)
                    %Btilda = Btilda./(1+Btilda*R);
                    Btilda = Btilda./(R^.5);
                end
            end
            h = plot(times,Mtilda(1)*gampdf(times,Atilda(1),Btilda(1)),'b'); 
            set(h,'LineWidth',2);
            h = plot(times,Mtilda(Nactions)*gampdf(times,Atilda(Nactions),Btilda(Nactions)),'r'); 
            set(h,'LineWidth',2);
            if (Nactions>2)
                h = plot(times,Mtilda(Nactions-1)*gampdf(times,Atilda(Nactions-1),Btilda(Nactions-1)),'g');
                set(h,'LineWidth',2);
                legend('LP','NP','Other')
            else
                legend('LP','NP')
            end
            [a,x] = find(Sindex(:,:,:)==S(i));
            [b,c] = find(squeeze(Sindex(a,:,:))==S(i));
            set(gca,'FontSize',12); 
            title(sprintf('Policy for state (t_s=%d,a_{prev}=%d,i_r=%d) with value %3.4f',a,b,c,V(S(i))))
            box off
            set(gcf,'Paperpositionmode','auto')
        end

    else
        % plotting the resulting Q values
        % -------------------------------
        figure; 
        if (strcmpi(schedule,'interval'))
            subplot(2,1,1)
        else
            subplot(3,1,1)
        end
        hold on;
        h = plot(times,squeeze(Q(:,:,Sindex(1,1,1))));
        v = axis;
        set(gca,'FontSize',14); set(h,'LineWidth',2);
        Qtemp = reshape(Q(:,:,Sindex(1,1,1)),1,L_Q);
        Qtemp = Qtemp - max(Qtemp);     % as softmax doesn't care about absolute values and to replace overflow with underflow
        QSoft = reshape(exp(Qtemp/Temp)/sum(exp(Qtemp/Temp)),Ntimes,Nactions); % these are the softmaxed Qs
        h = plot(times,5*(v(4)-v(3))*QSoft+v(3),'--');
        set(h,'LineWidth',2);
        title('Q values/Softmaxed values after an unrewarded LP')

        if (strcmpi(schedule,'interval'))
            subplot(2,1,2)
        else
            subplot(3,1,2)
        end
        hold on;
        h = plot(times,squeeze(Q(:,:,Sindex(1,1,2))));
        v = axis;
        set(gca,'FontSize',14); set(h,'LineWidth',2);
        Qtemp = reshape(Q(:,:,Sindex(1,1,2)),1,L_Q);
        QSoft = reshape(exp(Qtemp/Temp)/sum(exp(Qtemp/Temp)),Ntimes,Nactions); % these are the softmaxed Qs
        h = plot(times,5*(v(4)-v(3))*QSoft+v(3),'--'); set(h,'LineWidth',2);
        title('Q values/Softmaxed values after a rewarded LP')

        if (strcmpi(schedule,'ratio')) % in interval schedule this is meaningless as we don't know how much time has passed
            subplot(3,1,3); hold on;
            h = plot(times,squeeze(Q(:,:,Sindex(1,2,1))));
            v = axis;
            set(gca,'FontSize',14); set(h,'LineWidth',2);
            Qtemp = reshape(Q(:,:,Sindex(1,2,1)),1,L_Q);
            Qtemp = Qtemp - max(Qtemp);     % as softmax doesn't care about absolute values and to replace overflow with underflow
            QSoft = reshape(exp(Qtemp/Temp)/sum(exp(Qtemp/Temp)),Ntimes,Nactions); % these are the softmaxed Qs
            h = plot(times,5*(v(4)-v(3))*QSoft+v(3),'--');
            set(h,'LineWidth',2);
            title('Q values/Softmaxed values after a NP')
        end
        if (Nactions>2)
            legend('LP','Other','NP')
        else
            legend('LP','NP')
        end
        box off
        set(gcf,'Paperpositionmode','auto')
    end

    if (learn)                % without learning the below are meaningless
        
        % Plotting the average rewards over time
        % --------------------------------------
        figure; 
        h = plot([1:length(R_log)],R_log,'b',[1,length(R_log)],[Rmin,Rmin],'k:');
        set(gca,'FontSize',14); set(h(1),'LineWidth',2);
        xlabel('actions'); ylabel('estimated average reward')
        box off
        set(gcf,'Paperpositionmode','auto')

        % Plotting the deltas at different times to see how learning proceeded
        % --------------------------------------------------------------------
        figure
        y = [Deltas(EatTrials(1:Nresponses)>0),0];
        y(floor(length(y)/100)*100+1:end) = [];       % to have a round number here
        h = plot(1:length(y),y,'b',51:100:length(y),sum(reshape(y,100,length(y)/100))/100,'r',[1,length(y)],[0,0],'k--');
        set(gca,'FontSize',14); set(h,'LineWidth',2);
        title('Prediction errors at time of reward throughout training')
        box off
        set(gcf,'Paperpositionmode','auto')

        figure
        y = [Deltas(Nhalfway+find(EatTrials(Nhalfway+1:Nresponses)==0)),0];
        a = floor(length(y)/100)*100;
        y(1:length(y)-a) = [];
        h = plot(1:a,y,'b',51:100:a,sum(reshape(y,100,a/100))/100,'r',[1,a],[0,0],'k--');
        set(gca,'FontSize',14); set(h,'LineWidth',2);
        title('Prediction errors between rewards in the last half of training')
        box off
        set(gcf,'Paperpositionmode','auto')

        % Plotting the behavior of the policy parameters during learning
        % --------------------------------------------------------------
        if (strcmpi(LearnMode,'DirectActor'))
            figure
            subplot(2,3,[1,4]);
            s = find(Responses==1 & LogS==Slog);
            if (constrained)
                x = LogAlpha(s);
                y = LogBeta(s);
            else
                x = exp(LogAlpha(s)) + minA;
                y = exp(LogBeta(s))  + minB;
                if (includeR)
                    y = y ./ (R_log(s).^.5);
                end
            end
            plot(x,y,'g',x(1),y(1),'r*')
            xlabel ('Alpha(LP,1)'), ylabel('Beta(LP,1)');
            subplot(2,3,2);
            plot(y);
            title('Beta(LP,1) over the trials');
            subplot(2,3,3);
            plot(x);
            title('Alpha(LP,1) over the trials');
            subplot(2,3,5);
            if (Nactions > 2)
                if (constrained)
                    plot(1:length(s),LogM(1,s),'b',1:length(s),LogM(Nactions-1,s),'g',1:length(s),LogM(Nactions,s),'r');
                else
                    plot(1:length(s),exp(LogM(1,s)),'b',1:length(s),exp(LogM(Nactions-1,s)),'g',1:length(s),exp(LogM(Nactions,s)),'r');
                end
                legend('LP','Other','NP');
            else
                if (constrained)
                    plot(1:length(s),LogM(1,s),'b',1:length(s),LogM(Nactions,s),'r');
                else
                    plot(1:length(s),exp(LogM(1,s)),'b',1:length(s),exp(LogM(Nactions,s)),'r');
                end
                legend('LP','NP');
            end
            if (Mlearning)
                title('M(:,1) over the trials');
            else
                title('Q(:,1) over the trials');
            end
            subplot(2,3,6);
            plot(LogV');
            title('V over the trials');
            box off
            set(gcf,'Paperpositionmode','auto')
        end
    end
end

% Print some notes about what just happened 
%------------------------------------------

fprintf('\nRun ended: In the second half there were %3.2f actions per trial \n\t the mean time to reward was %3.2f(%3.2f) \n\t the mean time to consume was %3.2f(%3.2f)',...
    Nacts_per_trial, Reward_summary(1), Reward_summary(2), Consump_summary(1), Consump_summary(2))
fprintf('\nThis run included %d sessions, %d trials and %d actions',N_session,Ndata,Nresponses)
fprintf('\nThe average time to baiting was %3.2fsec',bait_interval)
% IFRT = (FreeR(2:end)-FreeR(1:end-1)); IFRT = IFRT(IFRT>0);   % mean time between free rewards
% fprintf('\nIn this run %d free rewards were delivered (p=%3.2f), mean free inter-reward interval was %3.2f',...
%     length(FreeR),p_non,mean(IFRT))
fprintf('\nIn the second half, the mean latency to LP was %3.2f(%3.2f)sec and to NP was %3.2fsec\n\n',mean(L_lp),std(L_lp),mean(L_np))
