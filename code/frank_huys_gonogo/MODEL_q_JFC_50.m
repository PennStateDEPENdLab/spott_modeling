%%%%%%%%%%%%%%%%%
%%% Written by Quentin J. M. Huys, UCL, London 2011
%%% Reference:
%%% Guitart-Masip M, Quentin JM, Fuentemilla LL, Dayan P, Duzel E, Dolan RJ (2012)
%%% Go and no-go learning in reward and punishment: Interaction between affect and effect NeuroImage doi:10.1016/j.neuroimage.2012.04.024

function [l] = MODEL_q_JFC(x,a,r,s,theta,Z,doprior)    
% x=.5*ones(Np,1);
   
rho 	= exp(x(1:2));       % 1-rew, 2-pun differential sensitivity for reward and punishment        
epsilon = 1/(1+exp(-x(3))); 
xi      = 1/(1+exp(-x(4)));
gobias  = x(5);

if doprior
	l = -1/2 * (x-Z.mu)'*Z.nui*(x-Z.mu) - 1/2*log(2*pi/det(Z.nui));   
else
	l=0;
end

% Initialize: rows = 1)Go, 2)NoGo 
Q=zeros(2,4);
thingy=[0,1];

for t=1:length(a)
    % Reinforcement
    rew = any(s(t)==[1 3]) + 1;  % 1 and 3 are punishment trials
    er  = rho(rew) * r(t); % E2 - Select Rew/Pun sensitivity
    
    % MODEL-FREE
	q    = Q(:,s(t));           % E2 - q, expected value, carries over from previous trial
    q(1) = q(1) + gobias;         % E4 - M3 go bias
	
    p0   = exp(q)/sum(exp(q));  % E3 - softmax Probability of action
	pxi  = xi*p0 + (1-xi)/2;    % E3 - M2 irreducible noise 'xi' or 'g': action + noise
    BG   = pxi(a(t));           % 

    finalaction=BG;   
     
    % Likelihoood
    l = l + log(finalaction); 
    
    % Agg
	Q(a(t),s(t)) = Q(a(t),s(t)) + epsilon * (er - Q(a(t),s(t)));  

end
l  = -l ;



