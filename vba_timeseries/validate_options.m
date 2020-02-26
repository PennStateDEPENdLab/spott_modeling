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
        vo.model = 'time2pl';
    end
end

% setup parallel parameters
if isfield(vo, 'do_parallel') && vo.do_parallel == true
    if ~isfield(vo, 'matlab_cpus')
        fprintf('Using user-specified matlab_cpus = %d for parallel execution. Ignoring matlab_cpus environment variable\n', vo.matlab_cpus);
    else
        
        ncpus=getenv('matlab_cpus');
        if strcmpi(ncpus, '')
            ncpus=4;
            fprintf('defaulting to 40 cpus because matlab_cpus not set\n');
        else
            ncpus=str2double(ncpus);
        end
        
        poolobj=parpool('local',ncpus); %just use shared pool for now since it seems not to matter (no collisions)
    end
end

if ~isfield(vo, 'multisession'), vo.multisession=0; end
if ~isfield(vo, 'graphics'), vo.graphics=0; end
if ~isfield(vo, 'saveresults'), vo.saveresults=0; end %used in some places to denote whether to save fitted outputs


end
