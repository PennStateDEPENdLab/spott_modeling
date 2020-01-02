%loads in subjects' data and fits SUUVID models using VBA;

close all;
clear;
%curpath = fileparts(mfilename('fullpath'));
addpath(genpath('~/VBA-toolbox-master'));
%% set environment and define file locations
project_repo = 'C:\Users\zzo1\Dropbox\GitLAb\spott_modeling\spott_modeling';
data_source=[project_repo, '\data\vba_input_Updated'];
addpath(genpath('C:\Users\zzo1\Documents\MATLAB\VBA-toolbox-master'));
addpath([project_repo, '\vba_timeseries']);
addpath([project_repo, '\vba_timeseries\obs_functions']);

inputfiles = dir([data_source, '/*spott_20.csv']);

%extract IDs for record keeping
ids = cellfun(@(x) char(regexp(x,'[\d]+','match','once')), {inputfiles.name}, 'UniformOutput', false);

%convert inputs back into full paths
inputfiles = arrayfun(@(x) fullfile(x.folder, x.name), inputfiles, 'UniformOutput', false);

%exclude_ids = {'76'}; %A and P pretty much always at 100
%filter_vec = ~ismember(ids, exclude_ids);
%inputfiles = inputfiles(filter_vec);
%ids = ids(filter_vec);

%% setup parallel parameters
% ncpus=getenv('matlab_cpus');
% if strcmpi(ncpus, '')
%   ncpus=40;
%   fprintf('defaulting to 40 cpus because matlab_cpus not set\n');
% else
%   ncpus=str2double(ncpus);
% end
%

poolobj=parpool('local',6); %just use shared pool for now since it seems not to matter (no collisions)
%% 
%p = ProgressBar(length(inputfiles));
% models = {'ap', 'ap_ravg', 'ap_dayonly', 'ap_dayonly_nest', 'ap_hours', ...
%     'ap_null', 'ap_dynaffect', 'ap_dynaffect_hours', 'ap_dynaffect_hours_scalar', 'ap_dynaffect_homerun'};

% models = {'suuvid_base', 'suuvid_nonu', 'suuvid_fixbeta', 'suuvid_nobeta', 'suuvid_kappaexponent'};
models = {'suuvid_kappaexponent'};

%models = {'suuvid_base'};
% inputfiles = inputfiles(1:6);

for mnum = 1:length(models)
    vo=[]; %vba options structure
    vo.dataset = 'vba_input_Updated';
    %vo.dataset = 'vba_sim_n5_minimal';
    vo.model = models{mnum};
    vo.graphics = 0; %don't display fitting interactively
    vo = validate_options(vo); %initialize and validate suuvid fitting settings
        
    vo.output_dir = [project_repo, '/outputs/vba_out/ffx/', vo.dataset, '/', vo.model];
    if ~exist(vo.output_dir, 'dir'), mkdir(vo.output_dir); end

    % Log evidence matrix
    L = NaN(1,length(inputfiles));
    
    % Subject statistics cell vector
    s_all = cell(1,length(inputfiles));

    parfor sub = 1:length(inputfiles)
        o_file=sprintf('%s/fit_%s_%s_multisession%d', ...
            vo.output_dir, ids{sub}, vo.model, vo.multisession);
        
        fit_subj=1;
        if exist([o_file, '.mat'], 'file')
            m=matfile(o_file, 'writable', false);
            posterior=m.posterior;
            out=m.out;
            m=[]; %clear matfile handle
            fit_subj=0;
            fprintf('Skipping existing file: %s\n', o_file);
        else
            fprintf('Fitting subject %d id: %s \n', sub, ids{sub});
            [posterior, out] = suuvid_vba_fit_subject(inputfiles{sub}, vo);
        end
        
        s_all{sub} = extract_subject_statistics(posterior, out); %extract key statistics for each subject
        
        L(sub) = out.F;
        
        subj_id=ids{sub};
        
        %write subject data to mat file only if just fitted, not loaded. This is necessary because the HDF5 format in
        %recent MATLAB .mat files will append to cell arrays implicitly, making the .mat files grow with every save
        if fit_subj == 1
            %parsave doesn't work in recent MATLAB versions.
            m=matfile(o_file, 'writable',true);
            m.posterior=posterior; m.out=out; m.subj_id=ids{sub}; m.subj_stats=s_all{sub};
        end
    end
    
    %save group outputs for now
    save(sprintf('%s/group_fits_%s_%s', vo.output_dir, vo.model, vo.dataset), 'ids', 'L', 'vo', 's_all');
    
    [group_global, group_trial_level] = extract_group_statistics(s_all, ...
        sprintf('%s/%s_%s_ffx_global_statistics.csv', vo.output_dir, vo.dataset, vo.model), ...
        sprintf('%s/%s_%s_ffx_trial_outputs.csv', vo.output_dir, vo.dataset, vo.model));
    
    %save group outputs for now
    save(sprintf('%s/group_fits_%s_%s', vo.output_dir, vo.model, vo.dataset), 'ids', 'L', 'vo', 's_all', 'group_global', 'group_trial_level');
    
end

%p.stop;
delete(poolobj);


%doesn't work in recent MATLAB

 %parsave(sprintf('%s/fit_%s_%s_multinomial%d_multisession%d_fixedparams%d_uaversion%d', ...
        %		  vo.output_dir, ids{sub}, vo.model, vo.multinomial, vo.multisession, ...
        %    vo.fixed_params_across_runs, vo.u_aversion), posterior, out);%, subj_id);
       
