% Actions = {'LP','Other','NP'};              % the possible actions
Actions = {'LP','NP'}
Nactions = length(Actions);

EatTime = 3;                                % minimal time needed to consume a reward

rho = [-.15,-.05];                            % internal rewards per unit + cost per unit action
% rho = [-.02,.10,-.02];   

s = 0.5;             
d = 1.0;
k_v = [s d;d s];                            % transition costs from action i to action j
% k_v = [s d d;d s d;d d s];                  % make self transitions to continuous actions 'free'

% check to make sure we did everything right so far
if (length(rho) ~= Nactions | length(k_v) ~= Nactions)
    error('DRIVER ERROR: you forgot to change RHO or K_V to match the number of actions!')
end
 
Beta = 0.05;                                   % time discount factor is 0.05. To use average reward RL Beta should be 0
Temp = 0.1;                                 % temperature of the softmax for action choice
Softmax = 1;                                % choosing actions greedily or by softmax?
figures = 1;                                % draw figures of results?

schedule_type = 'Variable'                  % random, fixed or variable schedule
schedule = 'Interval'                       % Ratio or interval schedule (note we can't do fixed interval w/o timing noise)
interval = 30;                              % mean interval for interval schedules
ratio = [1 3 9 27];                         % mean ratio for ratio schedules
Utility = 40;                               % utility of external reward

S_lp = zeros(length(ratio),2);
Latencies = zeros(size(S_lp));

dt = 10;                                    % discretization of time within a second
times = (1/dt:1/dt:30)';                    % Possible times for actions. The real time is i/dt
Ntimes = length(times);   
Nstates = (Ntimes+1)*Nactions*2;

% determining the schedule: what is the probability of reward? (for
% variable schedule only -- the rest are in the loop)
if (strcmpi(schedule_type,'variable'))
    startRew = 15*dt+1;
    endRew = 45*dt+1;
    for tau = 1:Ntimes
        for t_r = 1:Ntimes+1
            if (t_r+tau)<=startRew
                p_r(tau,t_r) = 0;
            else if (t_r+tau)>=endRew
                    p_r(tau,t_r) = 1;
                else 
                    p_r(tau,t_r) = ...
                        (tau+min(t_r-startRew-1,0))/((endRew-startRew)-max(t_r-startRew-1,0));
                end
            end
        end
    end
end

if ~exist('V')
    V = zeros(Ntimes+1,Nactions,2);
    fprintf ('\nStarting with new Vs')
end
if ~exist('r_avg')
    r_avg = 0;
end

for i = 1:length(interval)
    fprintf('\n%s %s %3.2f',schedule_type,schedule,interval(i))
    if strcmpi(schedule_type,'fixed')           
        Nstates = ratio(i)*Nactions*2;          % using first state index to count LPs rather than time
    end
    % determining the schedule: what is the probability of reward?
    if (strcmpi(schedule_type,'random') && strcmpi(schedule,'interval'))
        % sprintf('random interval schedule')
        for tau = 1:Ntimes                      % RI: reward probability depends on sum of t_lp and tau
            for t_lp = 1:Ntimes+1
                p_r(tau,t_lp) = 1-exp(-((tau+t_lp-1)/(interval(i)*dt))); 
            end
        end
    end
    if (strcmpi(schedule_type,'random') && strcmpi(schedule,'ratio'))
        % sprintf('random ratio schedule')
        p_r = ones(Ntimes,Ntimes+1)/ratio(i);   % RR: reward probability for each press is constant
    end
    if (strcmpi(schedule_type,'fixed') && strcmpi(schedule,'ratio'))
        % sprintf('fixed ratio schedule')
        p_r = ratio(i);   % we will use this for finding V, and later to work out the Q values
    end
    % r_avg = 0.1;
    
    % Running value iteration to get the optimal values
    if (strcmpi(schedule_type,'fixed') && Beta == 0) 
        % can't use V iteration in this case
        % rates(i) = 0;
        [V,r_avg] = V_Solve_FR(ratio(i),k_v,rho,Utility,Nactions,rates(i)*.4*dt,EatTime);
        r_avg = r_avg/dt;    % to convert to the dt units we will later use
    else
        % sprintf('finding values by value interation')
        [V,SoftQ,LPrate,r_avg] = ...
             V_iteration(V,rho,k_v,Utility,Temp,dt,times,p_r,Nactions,Nstates,EatTime,Beta,r_avg,schedule_type);
        % figure; plot(SoftQ)
        LP_rates(i) = LPrate;
        % r_rate(i) = r_avg;
    end
     
    rates(i) = r_avg
    fprintf ('\nNow generating data...')
    % Generating data to see what we have got
    Reward_times = 0;%Rewards;          % use previous reward times
    Ndata = 10000;                        % generate only half the data since 23.5.2005 to save time
    Nsessions = 200;                    % count sessions instead of data points
    [FirstLP_summary,R_summary,IRT_LP_summary,Consump_summary,Nacts_per_trial,LPs,NPs,Session_lp,CacheQ,CacheIndex,Rewards,L_lp,L_np,actual_interval,R,LP] = ...
        GenerateData(V,r_avg,rho,k_v,Utility,1/interval(i),times,p_r,EatTime,Nactions,Nstates,dt,Temp,Beta,Ndata,Nsessions,Softmax,figures,Reward_times,schedule,schedule_type);
    fprintf ('\n\n ----------------------------- \n')
    % figure;plot(CacheQ(CacheIndex(1,1,1),1:Ntimes)')
    int(i) = R_summary(1);
    actual(i) = actual_interval;
    S_lp(i,:) = [mean(Session_lp),std(Session_lp)];
    Latencies(i,:) = [L_lp, L_np];
end