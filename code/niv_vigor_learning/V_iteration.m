function [V,r_avg] = ...
    V_iteration(V,rho,k_v,Utility,Temp,dt,times,p_r,p_non,p_bsr,Nactions,Nstates,EatTime,Beta,r_avg,schedule_type)

%--------------------------------------------------------------------------
% Value Iteration - Generation 4: derive optimal V and Q values 
% when timing within a trial is of no importance, but timing from
% last LP is important, so the state space registers t_lp rather than t_r,
% 
% V - the values to start from (and the end values as output)
% rho - the costs of each unit action
% k_v - the constants for the time-dependent transition costs between actions
% Utility - reward utility
% Temp - temperature for the softmax (used to compute Q values in the end)
% dt - the time resolution (how many timepoints per second)
% times - possible times for actions (states)
% p_r - probability of reward for time/LP count
% p_non - probability of noncontingent rewards (per time)
% p_bsr - rate of ongoing 'BSR' (direct) type of reward
% Nactions - how many actions
% Nstates - how many states
% EatTime - How long it takes to eat a reward (could be 0)
% Beta - time discount factor - if it is 0, use average reward instead
% r_avg - the average reward to start with (and an output) - used only if Beta = 0
% schedule_type - 'random', 'fixed' or 'variable' - the type of reward schedule used
%-------------------------------------------------------------------------

Ntimes = length(times);

% check that we did not change the dimensionality of V since the last run
if (sum(size(V)) ~= sum([Nstates/(2*Nactions),Nactions,2]))    % using sum to execute even if only one dimension doesn't match
    V = zeros(Nstates/(2*Nactions),Nactions,2);
    fprintf ('\nStarting with new Vs')
end

if (Beta > 0)  % use time discounting
    discount = exp(-Beta*times)*ones(1,Nactions); % discount factor for different action delays
else
    discount = [];
end

% Value iteration in order to find the optimal value:
MaxChange = 0.01;
change = MaxChange+1;
Niter = 0;

while (abs(change)>MaxChange)  % this is the criterion for convergence
    Niter = Niter +1;

    % Looping over each state to update V
    V_new = zeros(Nstates/(Nactions*2),Nactions,2);
    
    for i_r = 1:2
        for prev_action = 1:Nactions
            max_t_s = Ntimes+1;     % from 0 and onward
            if (prev_action == 1 && strcmpi(schedule_type,'random'))
                max_t_s = 1;        % as after a LP the time from LP is always 0
            end  
            if strcmpi(schedule_type,'fixed')
                max_t_s = p_r;      % = ratio (counting 0...ratio-1 previous LPs)
            end
            % if (i_r == 2 && prev_action == Nactions) % NP is always the last action! thus these states actually don't exist so there is nothing more to compute
            %     break 
            % end
            for t_s = 1:max_t_s   % the time (or LP count) argument in the state will be, for generality, t_s
    
                % computing the new Qs for this state, which means, the maximum over
                % the values we would get if we started in this state, and took
                % each of the actions (times,actions).
                V_next = compute_Q(t_s,prev_action,i_r,V,Utility,EatTime,times,Ntimes,rho,k_v,p_r,p_non,p_bsr,r_avg,Nactions,Beta,discount,schedule_type);
                
                % and taking the maximum over all these
                V_new(t_s,prev_action,i_r) = max(max(V_next));
                
                % fprintf('The value for time %3.2f, action %d and i_r %d was %3.2f and now is %3.2f',t_s, prev_action, i_r, V(t_s,prev_action,i_r), V_new(t_s,prev_action,i_r))
            end
        end
    end
    
    if (Beta == 0)                    % average reward RL (we should never get here with a FR schedule)
        V_new = V_new - V_new(1,1,2); % values are defined up to an additive constant anyway so we can subtract V(S_0)
        % don't forget to set the dummy values back to 0 after we subtracted the constant! (we may end up using them later...)
        % V_new(:,Nactions,2) = 0;  -- we don't do this any more because with free rewards there is meaning to this value
        if strcmpi(schedule_type,'random')
            V_new(2:end,1,:) = 0; 
        end
        % Updating the average reward based on the new values and the actions
        % we can take from the special state S_0. If we do this with the new
        % states we don't have to renormalize them so that S_0 will be zero, as
        % this comes out by definition
        % S_0 = (1,1,2) (just after a LP which received a reward)
        V_0 = compute_Q(1,1,2,V_new,Utility,EatTime,times,Ntimes,rho,k_v,p_r,p_non,p_bsr,0,Nactions,Beta,discount,schedule_type);
        r_avg = max(max(V_0./(times*ones(1,Nactions)+ones(Ntimes,1)*[zeros(1,Nactions-1),EatTime]))); % don't forget to account fo the extra EatTime if NPing!
    end
    
    change = sum(sum(sum(V(:,1:(Nactions-1),:)-V_new(:,1:(Nactions-1),:)))) + ...
        sum(sum(V(:,Nactions,:)-V_new(:,Nactions,:)));
    
    % fprintf('finished iteration %d ==> r_avg = %3.2f, change = %3.2f',Niter,r_avg,change)
    
    V = V_new;
    
end

fprintf ('\nV iteration ended after %d iterations, last change was %5.4f',Niter,change)
if (Beta == 0)
    fprintf('\n\tThe optimal average reward is %5.4f',r_avg)
end

return
