function  [ gx ] = suuvid_obs_fixbeta(Xt, phi, u, inG)
% INPUT
% - x_t : hidden states (weights of basis functions)
% - phi : temperature (1x1)
% - u   : imput vector (not used in observation)
% - inG : multinomial
% OUTPUT
% - gx : p(chosen|x_t)

phi = transform_phi(phi, inG);
beta = inG.beta; %motor speed recovery rate
gamma = phi(1); %slope on vigor logistic (sensitivity)
nu = phi(2); %basal vigor
kappa = phi(3); %softmax temperature
cost = phi(4); %stickiness

tdiff = u(4); %cross-check position in u
active_action = u(5); %cross-check position in u

n_actions = inG.hidden_states;
phi_tb = 1 - exp(-tdiff/beta);

Qcur = Xt(1:n_actions);
Qtot = sum(Qcur); %total value

%probability of responding at all
p_respond = phi_tb/(1 + exp(-gamma * (Qtot + nu)));

%which action to choose

cc = zeros(n_actions,1); %row vector, as with Q
if active_action > 0 %will be zero before an action is chosen in a trial
    cc(active_action) = 1; %populate current action to capture stickiness
end

%Lau and Glimcher 2005
m = kappa*Qcur + cost*cc;

m = m - max(m); %rescale for avoiding floating point overflow
p_which = exp(m)/sum(exp(m));

gx = [p_which * p_respond; 1 - p_respond]; %predicted probabilities

end
