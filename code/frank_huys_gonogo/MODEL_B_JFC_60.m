%%%%%%%%%%%%%%%%%
%%% Written by Quentin J. M. Huys, UCL, London 2011
%%% Reference:
%%% Guitart-Masip M, Quentin JM, Fuentemilla LL, Dayan P, Duzel E, Dolan RJ (2012)
%%% Go and no-go learning in reward and punishment: Interaction between affect and effect NeuroImage doi:10.1016/j.neuroimage.2012.04.024

function [l] = MODEL_B_JFC(x,a,r,s,theta,Z,doprior)    
% x=.5*ones(Np,1);
        
rho 	= exp(x(1:2));          
epsilon = 1/(1+exp(-x(3))); 
xi      = 1/(1+exp(-x(4)));
gobias  = x(5);
pibias  = exp(x(6));

if doprior
	l = -1/2 * (x-Z.mu)'*Z.nui*(x-Z.mu) - 1/2*log(2*pi/det(Z.nui));   
else
	l=0;
end

% Initialize: rows = 1)Go, 2)NoGo 
Q=zeros(2,4);
V=zeros(1,4); 

for t=1:length(a)
    % Reinforcement
    rew = any(s(t)==[1 3]) + 1; 
	er  = rho(rew) * r(t);
    
    % MODEL-FREE
	q    = Q(:,s(t));  
    q(1) = q(1) + pibias * V(s(t)) + gobias;    % add Pavlovian effect 
    
	p0   = exp(q)/sum(exp(q)); 
	pxi  = xi*p0 + (1-xi)/2;
    BG   = pxi(a(t));
    
    finalaction=BG;   
     
    % Likelihoood
    l = l + log(finalaction); 
    
    % Agg
	Q(a(t),s(t)) = Q(a(t),s(t)) + epsilon * (er - Q(a(t),s(t)));  
    V(s(t))      = V(s(t)) + epsilon * (er - V(s(t)));

end
l  = -l ;











