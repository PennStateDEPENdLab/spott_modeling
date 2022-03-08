function  [ gx ] = m_suuvid_obs_exp(Xt, phi, u, inG)
% INPUT
% - x_t : hidden states (weights of basis functions)
% - phi : temperature (1x1)
% - u   : imput vector (not used in observation)
% - inG : multinomial
% OUTPUT
% - gx : p(chosen|x_t)

phi = transform_phi(phi, inG); 
gamma = phi(1); %value-based recovery component
nu = phi(2); %basal recovery component
kappa = phi(3); %softmax temperature
omega = phi(4); %stickiness

tdiff = u(4); %cross-check position in u
active_action = u(5); %cross-check position in u

n_actions = inG.hidden_states;

Qcur = Xt(1:n_actions);
Qtot = sum(Qcur); %total value

%probability of responding at all: pure 2PL
%tdiffR = tdiff/1000;
%if tdiffR>1
%        disp('tdiff reset')
%        tdiffR = 1;
%end
%p_respond = 1/(1 + exp(-gamma * Qtot * (tdiffR - nu)));
%p_respond = 1/(1 + exp(-gamma * Qtot * (tdiff/1000 + nu)));
p_respond = 1-exp(-1*(nu + gamma*Qtot)*tdiff/1000);
%which action to choose

cc = zeros(n_actions,1); %row vector, as with Q
if active_action > 0 %will be zero before an action is chosen in a trial
    cc(active_action) = 1; %populate current action to capture stickiness
end

%Lau and Glimcher 2005
m = kappa*Qcur + omega*cc; % m is 2x1, one is always 0; Qcur is 2x1, current q value between 0-1

m = m - max(m); %rescale for avoiding floating point overflow
p_which = exp(m)/sum(exp(m));

gx = [p_which * p_respond; 1 - p_respond]; %predicted probabilities 3 by 1

end
