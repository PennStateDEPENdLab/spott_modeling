function [posterior,out] = suuvid_vba_fit_subject(data_file, vo)
% fits SUUVID model to single-subject 50ms-binned data using VBA toolbox
% example call:
% [posterior,out]=suuvid_vba_fit_subject('01.csv', vo)
% data_file:    name of CSV file containing raw 50ms-binned from SPOTT instrumental
% vo: struct of options for fitting used in SUUVID model (setup by validate_options)

[~, str] = fileparts(data_file);
id = regexp(str,'[\d]+','match','once'); %use lookahead and lookbehind to make id more flexible (e.g., 128_1)
vo.id = id; %propagate into inF and inG for later extraction in group summaries

%load data from CSV file
[data, y, u] = suuvid_get_data(data_file, vo);

[vba_options, dim] = get_vba_options(data, vo);

[posterior,out] = VBA_NLStateSpaceModel(y, u, vo.evo_fname, vo.obs_fname, dim, vba_options);

posterior = add_transformed_params(posterior, vo); %add transformed phi and mu into output objects

out.diagnostics = VBA_getDiagnostics(posterior, out); %pre-compute diagnostics in batch mode (gives Volterra outputs, param correlations, etc.)

if vo.saveresults
  % save output figure
  % h = figure(1);
  % savefig(h,sprintf('results/%s_%s_multinomial%d_multisession%d_fixedParams%d', data_file,model,multinomial,multisession,fixed_params_across_runs))
  
  if ~isfield(vo, 'output_dir')
    vo.output_dir = '~/Data_Analysis/spott_modeling/outputs/vba_out';
  end
  
  save(sprintf([vo.output_dir, '/suuvid_fit_%s_%s_multisession%d_fixedParams%d'], ...
    id, vo.model, vo.multisession, vo.fixed_params_across_days), ...
    'posterior', 'out');
end
