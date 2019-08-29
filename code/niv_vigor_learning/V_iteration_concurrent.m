function [V,r_avg] = ...
    V_iteration_concurrent(V,rho,k_v,Utility,Temp,dt,times,p_r,Nactions,Nstates,EatTime,Beta,r_avg)

%--------------------------------------------------------------------------
% Value Iteration for concurrent schedules: derive optimal V and Q values 
% when timing from last LP ON EACH LEVER is important, so the state space 
% registers t_lp1 as well as t_lp2
% 
% V - the values to start from (and the end values as output)
% rho - the costs of each unit action
% k_v - the constants for the time-dependent transition costs between actions
% Utility - reward utility
% Temp - temperature for the softmax (used to compute Q values in the end)
% dt - the time resolution (how many timepoints per second)
% times - possible times for actions (states)
% p_r - probability of reward for time/LP count
% Nactions - how many actions
% Nstates - how many states
% EatTime - How long it takes to eat a reward (could be 0)
% Beta - time discount factor - if it is 0, use average reward instead
% r_avg - the average reward to start with (and an output) - used only if Beta = 0
%-------------------------------------------------------------------------

Ntimes = length(times);

% check that we did not change the dimensionality of V since the last run
if (sum(size(V) ~= [Ntimes+1,Ntimes+1,Nactions,2]))    % using sum to execute even if only one dimension doesn't match
    V = zeros(Ntimes+1,Ntimes+1,Nactions,2);
    fprintf ('\nStarting with new Vs')
end

if (Beta > 0)  % use time discounting
    discount = exp(-Beta*times)*ones(1,Nactions); % discount factor for different action delays
else
    discount = [];
end

%----------------------------------------------------
% Value iteration in order to find the optimal value:
%----------------------------------------------------
MaxChange = 0.01;
change = MaxChange+1;
Niter = 0;

while (abs(change)>MaxChange)  % this is the criterion for convergence
    Niter = Niter +1;

    % Looping over each state to update V
    V_new = zeros(Ntimes+1,Ntimes+1,Nactions,2);

    
    for i_r = 1:2
        for prev_action = 1:Nactions
            max_t_lp(1:2) = Ntimes+1;     % from 0 and onward
            if (prev_action < 3)          % previous action was a LP
                max_t_s(prev_action) = 1; % as after a LP the time from LP is always 0
            end  
            if (i_r == 2 && prev_action == Nactions) % NP is always the last action! thus these states actually don't exist so there is nothing more to compute
                break 
            end
            for t_lp1 = 1:max_t_lp(1)      
                for t_lp2 = 1:max_t_lp(2)
                    
                    % computing the new Qs for this state, which means, the maximum over
                    % the values we would get if we started in this state, and took
                    % each of the actions (times,actions).
                    Q = ones(Ntimes,1)*rho - (1./times)*k_v(prev_action,:); % costs
                    t_index1 = min(t_lp1+[1:Ntimes],Ntimes+1);
                    t_index2 = min(t_lp2+[1:Ntimes],Ntimes+1);
                    % LP1 - we get a reward or not, and time passes for LP2 as well
                    Q(:,1) = Q(:,1) + p_r(:,t_lp1,1).*V(1,t_index2,1,2)' + (1-p_r(:,t_lp1,1)).*V(1,t_index2,1,i_r)';
                    % LP2 - we get a reward or not, and time passes for LP1 as well
                    Q(:,2) = Q(:,2) + p_r(:,t_lp2,2).*V(t_index1,1,2,2) + (1-p_r(:,t_lp2,2)).*V(t_index1,1,2,i_r);
                    % Other actions (if there are some) - nothing happens but time progresses for both LPs
                    if (Nactions > 3)
                        for i = 1:Ntimes  % we have to do this in a loop as both first dimentions are variable vectors
                            Q(i,3:(Nactions-1)) = Q(i,3:(Nactions-1)) + V(t_index1(i),t_index2(i),3:(Nactions-1),i_r);
                        end
                    end
                    % NP (always the last action!) - If we are nose-poking and there is an external reward, consume it. 
                    if (i_r==2)
                        t_index1 = min(t_index1+EatTime*dt,Ntimes+1);
                        t_index2 = min(t_index2+EatTime*dt,Ntimes+1);
                        for i = 1:Ntimes
                            Q(i,Nactions) = Q(i,Nactions) + V(t_index1(i),t_index2(i),Nactions,1) + Utility;
                        end
                    else
                        for i = 1:Ntimes
                            Q(i,Nactions) = Q(i,Nactions) + V(t_index1(i),t_index2(i),Nactions,1);
                        end
                    end
              
%                     if (Beta == 0)    % using average rate RL instead of discounting - minus the delay*r_avg
%                         Q = Q - times*r_avg*ones(1,Nactions);               % accounting for average reward lost in the delay
%                         if (i_r == 2)
%                             Q(:,Nactions) = Q(:,Nactions)-r_avg*EatTime*dt; % accounting for reward lost while consuming
%                         end
%                     else
                        Q = discount.*Q;
%                     end
                    
                    % and taking the maximum over all these
                    V_new(t_lp1,t_lp2,prev_action,i_r) = max(max(Q));
                
                    % fprintf('The value for time %3.2f, action %d and i_r %d was %3.2f and now is %3.2f',t_s, prev_action, i_r, V(t_s,prev_action,i_r), V_new(t_s,prev_action,i_r))
                end
            end
        end
    end
    
%     if (Beta == 0)                    % average reward RL - S0 will be defined as what??
%         V_new = V_new - V_new(1,1,2); % values are defined up to an additive constant anyway so we can subtract V(S_0)
%         % don't forget to set the dummy values back to 0 after we subtracted the constant! (we may end up using them later...)
%         V_new(:,Nactions,2) = 0;
%         if strcmpi(schedule_type,'random')
%             V_new(2:end,1,:) = 0; 
%         end
%         % Updating the average reward based on the new values and the actions
%         % we can take from the special state S_0. If we do this with the new
%         % states we don't have to renormalize them so that S_0 will be zero, as
%         % this comes out by definition
%         % S_0 = (1,1,2) (just after a LP which received a reward)
%         V_0 = compute_Q(1,1,2,V_new,Utility,EatTime,times,Ntimes,rho,k_v,p_r,0,Nactions,Beta,discount,schedule_type);
%         r_avg = max(max(V_0./(times*ones(1,Nactions)+ones(Ntimes,1)*[zeros(1,Nactions-1),EatTime*dt]))); % don't forget to account fo the extra EatTime if NPing!
%     end
    
    change = sum(sum(V(1,:,1,:)-V_new(1,:,1,:))) + sum(sum(V(:,1,2,:)-V_new(:,1,2,:))) + ...
        sum(sum(sum(sum(V(:,:,3:(Nactions-1),:)-V_new(:,:,3:(Nactions-1),:))))) + ...
        sum(sum(V(:,:,Nactions,1)-V_new(:,:,Nactions,1)));
    
    fprintf('\n ---> finished iteration %d ==> change = %3.2f',Niter,change)
    
    V = V_new;
    
end

fprintf ('\nV iteration ended after %d iterations, last change was %5.4f',Niter,change)

return
