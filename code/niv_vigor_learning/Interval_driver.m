%------------------------------------------------------------------------
% driver for running RL for rates of behavior in INTERVAL SCHEDULES only!
%------------------------------------------------------------------------

% Actions = {'LP','NP'};                      % the possible actions
Actions = {'LP','Other','NP'}
Nactions = length(Actions);

EatTime = 6;                                  % minimal time needed to consume a reward

% rho = [-.15,-.15];                             % internal rewards per unit + cost per unit action
rho = [-.15,.15,-.15];   

s = 0.5;             
d = 1.5;
% k_v = [s .15;d s];                            % transition costs from action i to action j
k_v = [s d d;d s d;d d s];                  % make self transitions to continuous actions 'free'

% check to make sure we did everything right so far
if (length(rho) ~= Nactions | length(k_v) ~= Nactions)
    error('DRIVER ERROR: you forgot to change RHO or K_V to match the number of actions!')
end
 
Beta = 0;                                   % time discount factor is 0.05. To use average reward RL Beta should be 0
Temp = 0.25;                                % temperature of the softmax for action choice
Softmax = 1;                                % choosing actions greedily or by softmax?
figures = 0;                                % draw figures of results?

schedule_type = 'Random';                   % random or variable schedule (we can't do fixed interval without timing noise)
schedule = 'Interval';                      
interval = 30;                              % mean interval for interval schedules
% Utility = 60;                               % utility of external reward

% Data will be generated according to requested # of trials or sessions, whichever
% is satisfied first
Reward_times = 0; % Rewards;                % use previous reward times?
Ndata = 0;                                  % trials to run (real rats: 30*5*40 = 6000; 0-analyze only; 1-extinction)
Nsessions = 600;                             % number of sessions to run
max_actions = 500;                          % per trial, to avoid runaway bad policies

dt = 10;                                    % discretization of time within a second
times = (1/dt:1/dt:20)';                    % Possible times for actions. The real time is i/dt
Ntimes = length(times);   
Nstates = (Ntimes+1)*Nactions*2;

% determining the schedule: what is the probability of reward? (for
% variable schedule only -- RI is in the loop)
p_r = [];
if (strcmpi(schedule_type,'variable'))
    startRew = 15*dt+1;
    endRew = 45*dt+1; 
    for tau = 1:Ntimes
        for t_r = 1:Ntimes+1
            if (t_r+tau)<=startRew
                p_r(tau,t_r) = 0;
            else 
                if (t_r+tau)>=endRew
                    p_r(tau,t_r) = 1;
                else 
                    p_r(tau,t_r) = ...
                        (tau+min(t_r-startRew-1,0))/((endRew-startRew)-max(t_r-startRew-1,0));
                end
            end
        end
    end
end
p_non = [0];             % the probability of noncontingent rewards (rate per second)
p_bsr = 0;               % The rate of ongoing 'BSR' (direct) rewards

if ~exist('V')
    V = zeros(Ntimes+1,Nactions,2);
    fprintf ('\nStarting with new Vs')
end
if ~exist('r_avg')
    r_avg = 0;
end
Summary = []; rates = []; bait = []; SessLP = [];
for i = 1:length(Utility)
    fprintf('\n%s %s %3.2f',schedule_type,schedule,interval)
    
    % determining the schedule: what is the probability of reward?
    if (strcmpi(schedule_type,'random') && strcmpi(schedule,'interval'))
        for tau = 1:Ntimes                      % RI: reward probability depends on sum of t_lp and tau
            for t_lp = 1:Ntimes+1
                p_r(tau,t_lp) = 1-exp(-((tau+t_lp-1)/(interval*dt))); 
            end
        end
    end
    
    % Running value iteration to get the optimal values
    fprintf('\nFinding values by value iteration...')
    [V,r_avg] = V_iteration(V,rho,k_v,Utility(i),Temp,dt,times,p_r,p_non,p_bsr,Nactions,Nstates,EatTime,Beta,r_avg,schedule_type);
    
    % Generating data to see what we have got
    fprintf ('\nNow generating data...')
    [Responses,Latencies,Times,R_summary,Consump_summary,Nacts_per_trial,LPs,NPs,Session_lp,CacheQ,CacheIndex,Rewards,L_lp,L_np,Bait_interval,LPrate,OutSummary] = ...
        GenerateData(V,r_avg,rho,k_v,Utility(i),1/interval,times,p_r,p_non,p_bsr,EatTime,Nactions,Nstates,dt,Temp,Beta,Ndata,Nsessions,max_actions,Softmax,figures,Reward_times,schedule,schedule_type);
    fprintf ('\n\n ----------------------------- \n')

%     % Generating data with lower r_avg for comparison
%     fprintf ('\nNow generating data...')
%     [Responses,Latencies,Times,R_summary,Consump_summary,Nacts_per_trial,LPs,NPs,Session_lp,CacheQ,CacheIndex,Rewards,L_lp,L_np,Bait_interval,LPrate,OutSummary] = ...
%         GenerateData(V,r_avg*.5,rho,k_v,Utility,1/interval,times,p_r,p_non(i),p_bsr,EatTime,Nactions,Nstates,dt,Temp,Beta,Ndata,Nsessions,max_actions,Softmax,figures,Reward_times,schedule,schedule_type);
%     fprintf ('\n\n ----------------------------- \n')
    
    rates(i) = r_avg;
    Summary(i,:) = OutSummary;
    bait(i) = Bait_interval;
    % ActualLPRate{i} = LPs;
    SessLP(i) = mean(Session_lp);
    
%     if isempty(Y)
%         Y = [r_avg R_summary(end-1:end)]';
%     else
%         Y(:,end+1) = [r_avg R_summary(end-1:end)]';   % we will run FR10,RR10 and RI10 one after the other to get the difference between obtained and greedy reward for Ch5
%     end
end

% save -V6 intervalU rates Bait ActualLPRate SessLP Summary

% figure
% hf = plot(p_non*60,rates*4,'*-r');
% set(hf,'LineWidth',2)
% hold on
% hf = plot(p_non*60,Summary(:,[1,3]),'*-');
% set(hf,'LineWidth',2)
% set(gca,'FontSize',14)
% xlabel('probability of free reward per minute')
% box off
% legend('r_{avg} (x4)','Mean LP latency','Mean Consumption')
% title ('Optimal responding on RI30')

% V_final = V;