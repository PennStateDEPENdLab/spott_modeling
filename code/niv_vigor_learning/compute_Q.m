function Q = compute_Q(t_s,prev_action,i_r,V,Utility,EatTime,times,Ntimes,rho,k_v,p_r,p_non,p_bsr,r_avg,Nactions,Beta,discount,schedule_type)

% computing the Q values for this state, which means: the values we would
% get if we started in this state, and took each of the actions (times,actions).
dt = 1/(times(2) - times(1));
% start with the internal reward and cost for each action, and the ongoing
% BSR reward rate (if there is such)
Q = ones(Ntimes,1)*rho - (1./times)*k_v(prev_action,:) + p_bsr*(times*ones(1,Nactions));

% add the state we will end up in after executing the action
% LP - can transition to i_r=2 with probability p_r, always
% transtions to t_lp = 0 in 'random' schedules           
if strcmpi(schedule_type,'fixed')  % all the state transitions are different in this case
    % in the average reward model, in order to prevent the rat from
    % accumulating LPs towards the next reward when he already has one
    % available, LPs are not incremented if i_r == 2
    if (i_r == 1 | Beta > 0)           % if there is no reward or if we are using discounting               
        if (t_s == p_r)                % in FR this is the ratio... (just to confuse us)
            Q(:,1) = Q(:,1) + V(1,1,2);
        else
            Q(:,1) = Q(:,1) + V(t_s+1,1,i_r);
        end
    else  % don't increment t_s even after LP when there is a reward and no discounting
        Q(:,1) = Q(:,1) + V(t_s,1,i_r);
    end
    Q(:,2:(Nactions-1)) = Q(:,2:(Nactions-1)) + ones(Ntimes,1)*V(t_s,2:(Nactions-1),i_r);
    Q(:,Nactions) = Q(:,Nactions) + V(t_s,Nactions,1) + (i_r-1)*Utility;
else
    t_index = min(t_s+[1:Ntimes],Ntimes+1);
    p_non_r = 1-exp(-p_non*times);  % add probability for noncontingent reward in the passing time until any action
    if strcmpi(schedule_type,'random')
        Q(:,1) = Q(:,1) + (p_non_r+p_r(:,t_s)).*V(1,1,2) + (1-p_r(:,t_s)-p_non_r).*V(1,1,i_r);
    else
        Q(:,1) = Q(:,1) + (p_non_r+p_r(:,t_s)).*V(1,1,2) + (1-p_r(:,t_s)-p_non_r).*V(t_index,1,i_r);
    end
    if (Nactions>2)
        Q(:,2:(Nactions-1)) = Q(:,2:(Nactions-1)) + (p_non_r*ones(1,Nactions-2)).*V(t_index,2:(Nactions-1),2) + (1-p_non_r).*V(t_index,2:(Nactions-1),i_r);
    end

    % NP (always the last action!) - If we are nose-poking and there is an external reward, consume it (and gain BSR for the trouble). 
    if (i_r==2)
        t_index = min(t_index+EatTime*dt,Ntimes+1);
        p_non_r = 1-exp(-p_non*(times+EatTime));
        Q(:,Nactions) = Q(:,Nactions) + p_non_r.*V(t_index,Nactions,2) + (1-p_non_r).*V(t_index,Nactions,1) + Utility + p_bsr*EatTime;
    else
        Q(:,Nactions) = Q(:,Nactions) + p_non_r.*V(t_index,Nactions,2) + (1-p_non_r).*V(t_index,Nactions,1);
    end
end

if (Beta == 0)    % using average rate RL instead of discounting - minus the delay*r_avg
    Q = Q - times*r_avg*ones(1,Nactions);               % accounting for average reward lost in the delay
    if (i_r == 2)
        Q(:,Nactions) = Q(:,Nactions) - r_avg*EatTime;  % accounting for reward lost while consuming
    end
else
    Q = discount.*Q;
end
return