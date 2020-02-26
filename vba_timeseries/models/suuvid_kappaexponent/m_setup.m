function [vo] = m_setup(vo)
% Setup model function
% SUUVID Kappa exponent model for response probability

if nargin < 1, vo = struct(); end

%% Model overview
% evolution function
vo.evo_fname = @m_evo_function; %@suuvid_base_evo;

% observation function
vo.obs_fname = @m_obs_function; %@suuvid_obs_kappaexponent;

% number of hidden states
vo.hidden_states = 2; %two-action approach for now

% names of hidden states
vo.state_names = {'Q1', 'Q2'};

% number of model outputs (size of y vector)
vo.n_outputs = 3; %two actions + no response

% names of model outputs
vo.y_names = {'y1', 'y2', 'none'};

% number of free parameters in the evolution function
vo.n_theta = 1;

% names of parameters in the evolution function
vo.theta_names = {'alpha'};

% number of parameters in the observation function
vo.n_phi = 5;

% names of parameters in the observation function
vo.phi_names = {'beta', 'gamma', 'nu', 'kappa', 'cost'};

%% Model priors

priors.muTheta = zeros(vo.n_theta, 1);
priors.SigmaTheta = 1e1*eye(vo.n_theta); %variance of 10 on all

priors.muPhi = [1; ... %beta: exponentiates to ~150
    0; ... %gamma: exponentiates to 1
    0; ... %nu: keep at zero prior
    0; ... %kappa (temperature); Gamma(2,1) transform to 1.67
    0 ]; %cost weight: keep at 0 prior

priors.SigmaPhi = [10,10,10,1,10].*eye(vo.n_phi); %variance of 10 on all excpet kappa (gamma inverse cdf transform)

priors.muX0 = zeros(vo.hidden_states,1); %zero initial values on Q
priors.SigmaX0 = zeros(vo.hidden_states); %no variance or covariance on initial Q values

vo.priors = priors; % populate to the vo structure


end

