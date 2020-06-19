%loads in subjects' data and fits SUUVID models using VBA;

close all;
clear;
% set environment and define file locations
project_repo = 'C:\Users\zzo1\Dropbox\GitLAb\spott_modeling\spott_modeling';
%data_source=[project_repo, '\data\vba_input_twoPL16'];
data_source=[project_repo, '\data\pandaa_vba_input'];
addpath(genpath('C:\Users\zzo1\Documents\MATLAB\VBA-toolbox-master'));
addpath([project_repo, '\vba_timeseries']);
addpath([project_repo, '\vba_timeseries\obs_functions']);
addpath('c:\Users\zzo1\Dropbox\GitLAb\spott_modeling\mh_vba\');

%vba_working_dir = fileparts(mfilename('fullpath')); %for paths relative to the vba repo
vba_working_dir = 'C:\Users\zzo1\Dropbox\GitLAb\spott_modeling\spott_modeling\vba_timeseries';
models_dir = [ vba_working_dir, filesep, 'models' ];
addpath(models_dir); %shared functions across models can live in models root

if ~exist(models_dir, 'dir'), error('cannot locate the models directory for setup'); end

inputfiles = dir([data_source, '/*spott_50.csv']);

% Remove interfering dotfiles:
inputfiles(cellfun(@(x)x(1)=='.', {inputfiles.name})) = [];

%inputfiles = dir([data_source, '/*spott_20.csv']);

%extract IDs for record keeping
ids = cellfun(@(x) char(regexp(x,'[\d]+','match','once')), {inputfiles.name}, 'UniformOutput', false);

%convert inputs back into full paths
inputfiles = arrayfun(@(x) fullfile(x.folder, x.name), inputfiles, 'UniformOutput', false);

%exclude_ids = {'76'}; %A and P pretty much always at 100
%filter_vec = ~ismember(ids, exclude_ids);
%inputfiles = inputfiles(filter_vec);
%ids = ids(filter_vec);

%p = ProgressBar(length(inputfiles));
% models = {'ap', 'ap_ravg', 'ap_dayonly', 'ap_dayonly_nest', 'ap_hours', ...
%     'ap_null', 'ap_dynaffect', 'ap_dynaffect_hours', 'ap_dynaffect_hours_scalar', 'ap_dynaffect_homerun'};

%models = {'suuvid_base', 'suuvid_nonu', 'suuvid_fixbeta', 'suuvid_nobeta'};
%models = {'fixbeta', 'nobeta', 'nonu_nobeta', 'nonu', 'zetaexponent'};
models = {'time2pl'};

%inputfiles = inputfiles(23:26);

for mnum = 1:length(models)
    disp(mnum)
    vo=[]; %vba options structure
    vo.dataset = 'pandaa_vba_input';
    %vo.dataset = 'vba_sim_n5_minimal';
    vo.model = models{mnum};
    vo.graphics = 0; %don't display fitting interactively
    vo = validate_options(vo); %initialize and validate suuvid fitting settings
    mdir = [models_dir, filesep, models{mnum}];
    if ~exist(mdir, 'dir'), error('cannot locate model directory: %s', mdir); else, addpath(mdir); end
    vo = m_setup(vo); %setup this model
        
    vo.output_dir = [project_repo, '/outputs/vba_out/ffx/', vo.dataset, '/', vo.model];
    if ~exist(vo.output_dir, 'dir'), mkdir(vo.output_dir); end

    % Log evidence matrix
    L = NaN(1,length(inputfiles));
    
    % Subject statistics cell vector
    s_all = cell(1,length(inputfiles));

    for sub = 1:length(inputfiles)
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
    
    rmpath(mdir); %cleanup model path
    
end

%p.stop;
%delete(poolobj);
