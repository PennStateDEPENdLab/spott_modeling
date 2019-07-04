function  [fx] = suuvid_base_evo(Xt, theta, u, inF)
% evolution function of basic suuvid model
%
%
% IN:
%   - Xt : Q values (one per action)
%   - theta : just alpha (learning rate) at present
%   - u 
%        u(1) = new trial 0/1
%        u(2) = prior choice
%        u(3) = prior reinforcement
%        u(4) = time between last response and current bin
%        u(5) = active key (relates to stickiness)
%   - inF : struct of input options (has nbasis and ntimesteps)
% OUT:
%   - fx: evolved basis values/heights (nbasis x 1)

%transform from Gaussian posterior to relevant parameter distributions
theta = transform_theta(theta, inF);
alpha = theta(1);

n_actions = inF.hidden_states;

%update value of chosen action
Qcur = Xt(1:n_actions);

%trial_change = u(1);
chosen_action = u(2);
rew = u(3); %whether the chosen action is reinforced

%carry forward Q values if no action emitted
Qnext = Qcur;

%only update chosen action, if any
if chosen_action ~= 0
    Qchosen = Qcur(chosen_action);
    Q_up = Qchosen + alpha*(rew - Qchosen); %update chosen action
    Qnext(chosen_action) = Q_up;
end

fx = Xt; %allow add-on hidden states
fx(1:length(Qnext)) = Qnext;

end
