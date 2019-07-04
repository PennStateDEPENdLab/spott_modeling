function [overall, trial_level]=extract_group_statistics(s_array, overall_fname, trial_level_fname)

%this function assumes that s_array is a 1-d cell vector of structs generated by extract_subject_statistics

if nargin < 2, overall_fname='suuvid_global_statistics.csv'; end
if nargin < 3, trial_level_fname='suuvid_trial_outputs.csv'; end

ns = length(s_array); %number of subjects

%validate theta parameters
n_theta = cellfun(@(x) length(x.muTheta), s_array);
if length(unique(n_theta)) ~= 1
  error('muTheta has inconsistent size across subjects in s_array');
end

%validate phi parameters
n_phi = cellfun(@(x) length(x.muPhi), s_array);
if length(unique(n_phi)) ~= 1
  error('muPhi has inconsistent size across subjects in s_array');
end

%validate y output size
n_y = cellfun(@(obj) size(obj.y, 1), s_array);
if length(unique(n_y)) ~= 1
  error('y has inconsistent size across subjects in s_array');
end

%validate state names
state_names = cellfun(@(x) {x.model_settings.state_names{:}}, s_array, 'UniformOutput', false);
if ~isequal(state_names{:})
  error('Hidden state names do not match across subjects in s_array');
else
  state_names = state_names{1}; %just use first element for output table names
end

%validate y names
y_names = cellfun(@(x) {x.model_settings.y_names{:}}, s_array, 'UniformOutput', false);
if ~isequal(y_names{:})
  error('y names do not match across subjects in s_array');
else
  y_names = y_names{1}; %just use first element for output table names
end

%validate parameter names
par_names = cellfun(@(x) {x.model_settings.theta_names{:}, x.model_settings.phi_names{:}}, s_array, 'UniformOutput', false);
if ~isequal(par_names{:})
  error('Parameter names do not match across subjects in s_array');
else
  par_names = par_names{1}; %just use first element for output table names
end

par_names_transformed=strcat(par_names, '_transformed');

params = cellfun(@(x) num2cell([x.muTheta', x.muPhi]), s_array, 'UniformOutput', false);
params_transformed = cellfun(@(x) num2cell([x.transformed.muTheta', x.transformed.muPhi]), s_array, 'UniformOutput', false);

has_ffx = all(cellfun(@(x) isfield(x, 'muTheta_ffx'), s_array));
if has_ffx
  params_ffx = cellfun(@(x) num2cell([x.muTheta_ffx', x.muPhi_ffx]), s_array, 'UniformOutput', false);
  params_transformed_ffx = cellfun(@(x) num2cell([x.transformed.muTheta_ffx', x.transformed.muPhi_ffx]), s_array, 'UniformOutput', false);
end

%validate elements of fit structure
fit_names = cellfun(@(x) fieldnames(x.fit)', s_array, 'UniformOutput', false);
fit_stats = cellfun(@(x) struct2cell(x.fit)', s_array, 'UniformOutput', false);
fit_unique = cellfun(@(x) length(x), fit_names);

if length(unique(fit_unique)) ~= 1
  error('Different number of elements in .fit field across subjects in s_array');
end

if ~isequal(fit_names{:})
  error('Names of .fit fields do not match across subjects in s_array');
else
  fit_names = fit_names{1}; %now that we've established congruence, just keep the first vector for the table output
end

%get descriptive information about the model, dataset, and key settings
model_cols = {'id', 'dataset', 'model', 'evo_fname', 'obs_fname'};
model_info = cellfun(@(x) {x.model_settings.id, x.model_settings.dataset, x.model_settings.model, ...
  char(x.model_settings.evo_fname), char(x.model_settings.obs_fname)}, ...
  s_array, 'UniformOutput', false);

% we shouldn't really enforce this since we could pass in multiple datasets, varying run lengths, etc.
%if ~isequal(model_info{:})
%  error('Contents of model information do not match across subjects in s_array');
%end

%each cell array can be column-wise concatenated across subjects to prepare for overall table
cell_combined=horzcat(vertcat(model_info{:}), vertcat(fit_stats{:}), vertcat(params{:}), vertcat(params_transformed{:}));

output_cols={model_cols{:}, fit_names{:}, par_names{:}, par_names_transformed{:}};

if has_ffx
  ffx_names = strcat(par_names, '_ffx');
  ffx_transformed_names = strcat(par_names, '_transformed_ffx');
  output_cols = {output_cols{:}, ffx_names{:}, ffx_transformed_names{:}};
  cell_combined = horzcat(cell_combined, vertcat(params_ffx{:}), vertcat(params_transformed_ffx{:}));
end

overall = cell2table(cell_combined, 'VariableNames', output_cols);
writetable(overall, overall_fname)

%% Trial-level statistics
trial_stats=cell(1,ns);
for i = 1:ns
  this_subj = s_array{i};
  n_obs = size(this_subj.y,2);

  %use 'asc_trial' to clarify that trial from VBA is the fitted trial number, which may not correspond to csv input numbering if there are skips
  t_tbl = table(repmat(this_subj.id, n_obs, 1), repmat(this_subj.model_settings.dataset, n_obs, 1), ...
    repmat(this_subj.model_settings.model, n_obs, 1), (1:n_obs)', 'VariableNames', {'id', 'dataset', 'model', 'obs_number'});

  if isfield(this_subj, 'hidden_states')
    vcell = array2table(this_subj.hidden_states', 'VariableNames', state_names);
    t_tbl = horzcat(t_tbl, vcell);
  end

  %observed responses
  if isfield(this_subj, 'y')
    vcell = array2table(this_subj.y', 'VariableNames', y_names);
    t_tbl = horzcat(t_tbl, vcell);
  end
  
  %model-predicted responses
  if isfield(this_subj, 'y_pred')
    vcell = array2table(this_subj.y_pred', 'VariableNames', strcat(y_names, '_pred'));
    t_tbl = horzcat(t_tbl, vcell);
  end
  
  if isfield(this_subj, 'u')
    vnames = cellfun(@(x) strcat('u_', num2str(x)), num2cell(1:size(this_subj.u,1)), 'UniformOutput', false);
    vcell = array2table(this_subj.u', 'VariableNames', vnames);
    t_tbl = horzcat(t_tbl, vcell);
  end

  trial_stats{i} = t_tbl;  
end

trial_level = vertcat(trial_stats{:});
writetable(trial_level, trial_level_fname);

end
