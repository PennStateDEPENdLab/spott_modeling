function [V,r_avg] = V_Solve_FR(ratio,k_v,rho,Utility,Nactions,r_avg,EatTime);

% Solve for the optimal values of the FR case, for the average reward model
% if r_avg is not 0 we use it instead (although I can't think of why we
% would want to do that)
V = zeros (ratio,Nactions,2);

% the minimal price for a whole round is making #Ratio LPs and one NP, (ratio-1)
% optimal transitions from LP to LP, one from LP to NP and one from NP to LP
if (r_avg == 0)
    if (EatTime)
        % This pesky EatTime! Now it is unsolvable... so solving numerically for r_avg
        % p (parms) are: [Utility, ratio, rho(LP), rho(NP), k_v(LP,LP), k_v(LP,NP), k_v(NP,LP), EatTime]
        % data is the r_avg we want to solve for
        warning off MATLAB:fzero:UndeterminedSyntax
        p = [Utility,ratio,rho(1),rho(Nactions),k_v(1,1),k_v(1,Nactions),k_v(Nactions,1),EatTime];
        r_solve = inline('(p(1)+p(2)*p(3)+p(4)-(p(2)-1)*(p(5)*x).^.5-(p(7)*x).^.5-(p(6)*x).^.5)./(p(8)+(p(2)-1)*(p(5)./x).^.5+(p(7)./x).^.5+(p(6)./x).^.5)-x','x','p');
        % we'll use the wrong solution from before as a starting point:
        a = EatTime;
        b = 2*((ratio-1)*(k_v(1,1)^.5) + k_v(1,Nactions)^.5 + k_v(Nactions,1)^.5);
        c = -(Utility + ratio*rho(1) + rho(Nactions));
        hint = ((-b + (b^2-4*a*c)^.5)/(2*a))^2;
        r_avg = fzero(r_solve,hint,[],p);
        fprintf('\nWe solved for the average reward numerically and it is: %3.4f',r_avg)
    else
        r_avg = (Utility + ratio*rho(1) + rho(Nactions))/...
            (2*((ratio-1)*(k_v(1,1)^.5) + k_v(1,Nactions)^.5 + k_v(Nactions,1)^.5));
        r_avg = r_avg^2;
    end   
end

K = 2*((r_avg*k_v).^.5);   % we will need this too many times later

% the chain is like this: (assuming two actions, LP and NP)
% (1,2,1) -> LP -> (2,1,1) -> LP -> ... -> LP -> (ratio,1,1) -> LP -> (1,1,2)
% -> NP -> (1,2,1). First we will set the values of these states by solving
% the recursion on the back of an envelope:
V(1,Nactions,1) = 0; % this will be our baseline, for simplicity
V(2,1,1) = K(Nactions,1) - rho(1);
for i = 3:ratio
    V(i,1,1) = K(1,1) - rho(1) + V(i-1,1,1);
end
V(1,1,2) = K(1,1) - rho(1) + V(ratio,1,1);
% As a sanity check we can close the loop and make sure we get a zero for V(1,Nactions,1):
fprintf('\nSanity check: this should be zero: %3.4f',K(1,Nactions) - rho(Nactions) + V(1,1,2) - Utility + r_avg*EatTime)

% now it is trivial to solve for all the rest of the values, as these 
% lead to the above known values:

V(2:ratio-1,Nactions,1) = rho(1) - K(Nactions,1) + V(3:ratio,1,1);
V(ratio,Nactions,1) = rho(1) - K(Nactions,1) + V(1,1,2);
V(:,1,2) = rho(Nactions) - K(1,Nactions) + Utility + V(:,Nactions,1) - r_avg*EatTime;
for a_prev = 2:Nactions-1
    V(1:ratio-1,a_prev,1) = rho(1) - K(a_prev,1) + V(2:ratio,1,1);
    V(ratio,a_prev,1) = rho(1) - K(a_prev,1) + V(1,1,2);
    V(1:ratio,a_prev,2) = rho(Nactions) - K(a_prev,Nactions) + V(1:ratio,Nactions,1) + Utility - r_avg*EatTime;
end

% I think thats it. Normalize by the value of the special state S0
V = V - V(1,1,2);                
V(:,Nactions,2) = 0; V(1,1,1) = 0; % nonexisting states = 0 
% V(2:ratio,1,2) = 0; 

return