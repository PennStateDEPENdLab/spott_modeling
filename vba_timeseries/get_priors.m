function [priors] = get_priors(dim, vo)

if isfield(vo, 'priors')
    priors = vo.priors; %copy across prior setup from vo
else
    priors=[];
end
    
%gamma math:
% mean = alpha / beta
% variance = alpha / beta^2

% precision on state and measurement noise
priors.a_alpha = Inf;   % infinite precision prior on state noist
priors.b_alpha = 0;
priors.a_sigma = 1;     % Jeffrey's prior on measurement noise
priors.b_sigma = 1;     % Jeffrey's prior

%suuvid default
% theta:
%   alpha: learning rate on reinforcement

% phi:
%   beta: motor recovery rate (numerator of prespond)
%   gamma: slope on prespond
%   nu: basal vigor in prespond
%   kappa: softmax temperature in switch probability
%   cost: unit switch cost

% if ismember(vo.model, {'suuvid_base'})
%     priors.muTheta = zeros(dim.n_theta, 1);
%     priors.SigmaTheta = 1e1*eye(dim.n_theta); %variance of 10 on all
%     
%     priors.muPhi = [5; ... %beta: exponentiates to ~150
%         0; ... %gamma: exponentiates to 1
%         0; ... %nu: keep at zero prior
%         0; ... %kappa (temperature); Gamma(2,1) transform to 1.67
%         0 ]; %cost weight: keep at 0 prior
%     
%     priors.SigmaPhi = [10,10,10,1,10].*eye(dim.n_phi); %variance of 10 on all excpet kappa (gamma inverse cdf transform)
%     
% elseif strcmpi(vo.model, 'suuvid_nonu')
%     priors.muTheta = zeros(dim.n_theta, 1);
%     priors.SigmaTheta = 1e1*eye(dim.n_theta); %variance of 10 on all
%     
%     priors.muPhi = [5; ... %beta: exponentiates to ~150
%         0; ... %gamma: exponentiates to 1
%         0; ... %kappa (temperature); Gamma(2,1) transform to 1.67
%         0 ]; %cost weight: keep at 0 prior
%     
%     priors.SigmaPhi = [10,10,1,10].*eye(dim.n_phi); %variance of 10 on all excpet kappa (gamma inverse cdf transform)
% 
% elseif strcmpi(vo.model, 'suuvid_nobeta')
%     priors.muTheta = zeros(dim.n_theta, 1);
%     priors.SigmaTheta = 1e1*eye(dim.n_theta); %variance of 10 on all
%     
%     priors.muPhi = [0; ... %gamma: exponentiates to 1
%         0; ... %nu: keep at zero prior
%         0; ... %kappa (temperature); Gamma(2,1) transform to 1.67
%         0 ]; %cost weight: keep at 0 prior
%     
%     priors.SigmaPhi = [10,10,1,10].*eye(dim.n_phi); %variance of 10 on all excpet kappa (gamma inverse cdf transform)
% 
% elseif strcmpi(vo.model, 'suuvid_fixbeta')
%     priors.muTheta = zeros(dim.n_theta, 1);
%     priors.SigmaTheta = 1e1*eye(dim.n_theta); %variance of 10 on all
%     
%     priors.muPhi = [0; ... %gamma: exponentiates to 1
%         0; ... %nu: keep at zero prior
%         0; ... %kappa (temperature); Gamma(2,1) transform to 1.67
%         0 ]; %cost weight: keep at 0 prior
%     
%     priors.SigmaPhi = [10,10,1,10].*eye(dim.n_phi); %variance of 10 on all excpet kappa (gamma inverse cdf transform)
% 
% elseif strcmpi(vo.model, 'suuvid_minimal')
%     priors.muTheta = zeros(dim.n_theta, 1);
%     priors.SigmaTheta = 1e1*eye(dim.n_theta); %variance of 10 on all
%     
%     priors.muPhi = [0]; %gamma: exponentiates to 1
%     priors.SigmaPhi = [10].*eye(dim.n_phi); 
%     
% elseif strcmpi(vo.model, {'suuvid_kappaexponent'})
%     priors.muTheta = zeros(dim.n_theta, 1);
%     priors.SigmaTheta = 1e1*eye(dim.n_theta); %variance of 10 on all
%     
%     priors.muPhi = [1; ... %beta: exponentiates to ~150
%         0; ... %gamma: exponentiates to 1
%         0; ... %nu: keep at zero prior
%         0; ... %kappa (temperature); Gamma(2,1) transform to 1.67
%         0 ]; %cost weight: keep at 0 prior
%     
%     priors.SigmaPhi = [10,10,10,1,10].*eye(dim.n_phi); %variance of 10 on all excpet kappa (gamma inverse cdf transform)
% end
% 

%0 mean and variance on initial states
%priors.muX0 = zeros(dim.n,1);
%need to add zero(s) onto GVAP

%if ismember(vo.model, {'suuvid_base', 'suuvid_nonu', 'suuvid_fixbeta'})

%default to zero priors on hidden states if not specified upstream in vo
if ~isfield(priors, 'muX0')
    priors.muX0 = zeros(dim.n,1);
end

% else
%     priors.muX0 = [vo.first_y; zeros(dim.n - dim.p,1)]; %provide initial levels as given, zeros elsewhere
%end

%priors.SigmaX0 = zeros(dim.n);
%priors.SigmaX0 = 1e1*eye(dim.n);

%allow small covariances in initial states, too
%if ismember(vo.model, {'suuvid_base', 'suuvid_nonu', 'suuvid_fixbeta'})

%default to zero variance and covariance on hidden states if not specified upstream in vo (i.e., default to no uncertainty)
if ~isfield(priors, 'SigmaX0')
    priors.SigmaX0 = zeros(dim.n);
end
    
    %priors.SigmaX0 = .01*ones(dim.n); %allow small covariances
    %priors.SigmaX0(logical(eye(dim.n))) = 1e1; %variance of 10
%end


end
