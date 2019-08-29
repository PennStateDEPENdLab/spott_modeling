%---------------------------------------------------------------------
% driver for running RL for rates of behavior in RATIO SCHEDULES only!
%---------------------------------------------------------------------

% Actions = {'LP','NP'}
Actions = {'LP','Other','NP'};              % the possible actions
Nactions = length(Actions);

EatTime = 6;                                % minimal time needed to consume a reward

% rho = [-.15,-.05];                          % internal rewards per unit + cost per unit action
rho = [-.15,.15,-.15];   

s = 0.5;             
d = 1.5;
% k_v = [s d;d s];                           % transition costs from action i to action j
k_v = [s d d;d s d;d d s];                  % (make self transitions to continuous actions 'free')

% check to make sure we did everything right so far
if (length(rho) ~= Nactions | length(k_v) ~= Nactions)
    error('DRIVER ERROR: you forgot to change RHO or K_V to match the number of actions!')
end
 
Beta = 0;                                   % time discount factor is 0.05. To use average reward RL Beta should be 0
Temp = 0.25;                                % temperature of the softmax for action choice at end of learning
%Temp = 0.2488;                              % temperature of the softmax for action choice
Softmax = 1;                                % choosing actions greedily or by softmax?
figures = 0;                                % draw figures of results?

schedule_type = 'Random';                   % random or fixed schedule
schedule = 'Ratio';                         
ratio = 10;                                 % mean ratio for ratio schedules
%Utility = 10;                               % utility of external reward

% Data will be generated according to requested # of trials or sessions, whichever
% is satisfied first
Reward_times = 0; % Rewards;                % use previous reward times?
Ndata = 0;                                  % trials to run (real rats: 30*5*40 = 6000; 0-only summary; 1-extinction)
Nsessions = 300;                            % number of sessions to run
max_actions = 500;                          % per trial, to avoid runaway bad policies

dt = 20;                                    % discretization of time within a second
times = (1/dt:1/dt:10)';                    % Possible times for actions. The real time is i/dt
Ntimes = length(times);   
Nstates = (Ntimes+1)*Nactions*2;

if ~exist('V')
    V = zeros(Ntimes+1,Nactions,2);
    fprintf ('\nStarting with new Vs')
end
if ~exist('r_avg')
    r_avg = 0;
end
%TimeShare = zeros(length(ratio),Nactions);
%TimeShareDA = zeros(length(ratio),Nactions);
p_r = [];
for i = 1:length(ratio)
    fprintf('\n%s %s %d',schedule_type,schedule,ratio(i))
    if strcmpi(schedule_type,'fixed')           
        Nstates = ratio(i)*Nactions*2;          % using first state index to count LPs rather than time
    end
    % determining the schedule: what is the probability of reward?
    if (strcmpi(schedule_type,'random') && strcmpi(schedule,'ratio'))
        p_r = ones(Ntimes,Ntimes+1)/ratio;      % RR: reward probability for each press is constant
        p_non = 0;                              % probability of noncontingent rewards (per second)
        p_bsr = 0;                              % rate of ongoing 'BSR' (direct) rewards
    end
    if (strcmpi(schedule_type,'fixed') && strcmpi(schedule,'ratio'))
        p_r = ratio(i);   % we will use this for finding V, and later to work out the Q values
        p_non = 0; p_bsr = 0;
    end
        
    % Running value iteration to get the optimal values
    if (strcmpi(schedule_type,'fixed') && Beta == 0) 
        % can't use V iteration in this case
        fprintf('\nFinding values by solving the cyclic equations...\n')
        r_avg = 0; % r_avg = rates(i)*dt; 
        [V,r_avg] = V_Solve_FR(ratio(i),k_v,rho,Utility,Nactions,r_avg,EatTime);
        fprintf('\nWe did it, and got an average reward rate of %3.4f',r_avg)
    else
        fprintf('\nFinding values by value iteration...\n')
        [V,r_avg] = V_iteration(V,rho,k_v,Utility(i),Temp,dt,times,p_r,p_non,p_bsr,Nactions,Nstates,EatTime,Beta,r_avg,schedule_type);
    end
    
    % Generating data to see what we have got
    fprintf ('\nNow generating data...')
    [Responses,Latencies,Times,R_summary,Consump_summary,Nacts_per_trial,...
            LPs,NPs,Session_lp,CacheQ,CacheIndex,Rewards,L_lp,L_np,Bait_interval,LPrate,X] = ...
        GenerateData(V,r_avg,rho,k_v,Utility,[],times,p_r,p_non,p_bsr,EatTime,Nactions,...
        Nstates,dt,Temp,Beta,Ndata,Nsessions,max_actions,Softmax,figures,Reward_times,schedule,schedule_type); 
    fprintf ('\n\n ----------------------------- \n')
    %Summary(i,:) = X;
    rates(i) = r_avg;
    SessLP(i,1) = mean(Session_lp);
    % TimeShare(i,:) = X;
%     if isempty(Y)
%         Y = [r_avg R_summary(end-1:end)]';
%     else
%         Y(:,end+1) = [r_avg R_summary(end-1:end)]';   % we will run FR10,RR10 and RI10 one after the other to get the difference between obtained and greedy reward for Ch5
%     end
    % Generating data with lower tonic DA
%     fprintf ('\nNow generating data...')
%     [Responses,Latencies,Times,R_summary,Consump_summary,Nacts_per_trial,...
%             LPs,NPs,Session_lp,CacheQ,CacheIndex,Rewards,L_lp,L_np,Bait_interval,LPrate,X] = ...
%         GenerateData(V,r_avg*0.4,rho,k_v,Utility,[],times,p_r,p_non,p_bsr,EatTime,Nactions,...
%         Nstates,dt,Temp,Beta,Ndata,Nsessions,max_actions,Softmax,figures,Reward_times,schedule,schedule_type); 
%     fprintf ('\n\n ----------------------------- \n')
%     TimeShareDA(i,:) = X;
%     int(i) = R_summary(1);
%    Bait(i) = Bait_interval;
%    ActualLPRate{i} = LPs;
%    SessLP(i,2) = mean(Session_lp);
%     V_final = V;
end
% save -V6 ratio2 rates Bait ActualLPRate SessLP Summary