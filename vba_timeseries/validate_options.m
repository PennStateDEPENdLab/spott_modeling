function [vo] = validate_options(vo)
% multinomial:  if 1 fits p_chosen from the softmax; continuous RT (multinomial=0) works less well
% multisession: treats runs/conditions as separate, helps fit (do not allow X0 to vary though)
% fixed_params_across_days -- self-explanatory
% fit_propspread -- makes temporal generalization within the eligibility trace a free parameter
% ntimesteps:      number of time bins
% u_aversion:   allow for uncertainty (ambiguity) aversion for UV_sum

if nargin < 1, vo=[]; end

%if user specifies a dataset upstream, don't check environment variable
if isfield(vo, 'dataset')
    fprintf('Using user-specified dataset: %s.\n   Will not check dataset environment variable.\n', vo.dataset);
else
    vo.dataset=getenv('dataset');
    if strcmpi(vo.dataset, '')
        vo.dataset='pandaa'; %currently just PANDAA data
    end
end

%same principle for model variant
if isfield(vo, 'model')
    fprintf('Using user-specified model: %s.\n   Will not check model environment variable.\n', vo.model);
else
    vo.model=getenv('model');
    if strcmpi(vo.model, '')
        vo.model = 'suuvid_base';
    end
end

if ~isfield(vo, 'multisession'), vo.multisession=0; end
%if ~isfield(vo, 'fixed_params_across_days'), vo.fixed_params_across_days=1; end
if ~isfield(vo, 'graphics'), vo.graphics=0; end
if ~isfield(vo, 'model'), vo.model='suuvid_base'; end
if ~isfield(vo, 'saveresults'), vo.saveresults=0; end %used in some places to denote whether to save fitted outputs


%hidden states field used to index hidden state vector inside evolution and observation functions
if strcmpi(vo.model,'suuvid_base')
    vo.obs_fname = @suuvid_obs;

    vo.evo_fname = @suuvid_base_evo;
    vo.hidden_states = 2; %two-action approach for now
    vo.state_names = {'Q1', 'Q2'};
    
    vo.n_outputs = 3; %two actions + no response
    vo.y_names = {'y1', 'y2', 'none'};
    
    vo.n_theta=1;
    vo.theta_names={'alpha'};

    vo.n_phi = 5;
    vo.phi_names = {'beta', 'gamma', 'nu', 'kappa', 'cost'};

elseif strcmpi(vo.model, 'suuvid_fixbeta')
    vo.obs_fname = @suuvid_obs_fixbeta;
    vo.beta = 100; %fix at a sane value

    vo.evo_fname = @suuvid_base_evo;
    vo.hidden_states = 2; %two-action approach for now
    vo.state_names = {'Q1', 'Q2'};
    
    vo.n_outputs = 3; %two actions + no response
    vo.y_names = {'y1', 'y2', 'none'};
    
    vo.n_theta=1;
    vo.theta_names={'alpha'};

    vo.n_phi = 4;
    vo.phi_names = {'gamma', 'nu', 'kappa', 'cost'};

elseif strcmpi(vo.model, 'suuvid_nonu')
    vo.obs_fname = @suuvid_obs_nonu;

    vo.evo_fname = @suuvid_base_evo;
    vo.hidden_states = 2; %two-action approach for now
    vo.state_names = {'Q1', 'Q2'};
    
    vo.n_outputs = 3; %two actions + no response
    vo.y_names = {'y1', 'y2', 'none'};
    
    vo.n_theta=1;
    vo.theta_names={'alpha'};

    vo.n_phi = 4;
    vo.phi_names = {'beta', 'gamma', 'kappa', 'cost'};
end

end
