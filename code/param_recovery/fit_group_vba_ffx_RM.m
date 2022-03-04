%
%loads in subjects' data and fits SUUVID models using VBA;

close all;
clear;
%curpath = fileparts(mfilename('fullpath'));

%vo=[];
%[vo, poolobj, behavfiles] = spott_setup_environment(vo);


%note that this function looks for 'dataset' and 'model'
%as environment variables so that this script can be scaled easily for batch processing

%% external settings

subjID = getenv("subj");
subjID = '000123';

%%

% local path
% addpath(genpath('/Users/ruofanma/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling'));
% addpath(genpath('/Users/ruofanma/Documents/GitHub/spott_modeling'));
% addpath(genpath('/Users/ruofanma/Documents/GitHub/mh_vba'));

% LL path
addpath(genpath('/nas/longleaf/home/maruofan/GitHub')); 
addpath(genpath('/proj/mnhallqlab/lab_resources/mh_vba'));

%% set environment and define file locations

% local path
% project_repo = '/Users/ruofanma/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling';
% addpath(genpath('/Users/ruofanma/Documents/MATLAB/VBA-toolbox')); 

% LL path
project_repo = '/proj/mnhallqlab/projects/spott_modeling/par_sim/fix_omega_kappa';
addpath(genpath('/proj/mnhallqlab/lab_resources/VBA-toolbox')); 

%data_source=[project_repo, '/data/vba_input_simulated_n80'];
%data_source=[project_repo, '/data/vba_input_simulated_n5_minimal'];
data_source = [project_repo, '/', subjID];  %CHECK
disp(data_source)
%%
%vba_working_dir = fileparts(mfilename('fullpath')); %for paths relative to the vba repo

% local path
% vba_working_dir = '/Users/ruofanma/Documents/GitHub/spott_modeling/vba_timeseries';
% LL path
vba_working_dir = '/nas/longleaf/home/maruofan/GitHub/spott_modeling/vba_timeseries';

models_dir = [ vba_working_dir, filesep, 'models' ];
addpath(models_dir); %shared functions across models can live in models root

if ~exist(models_dir, 'dir'), error('cannot locate the models directory for setup'); end

%inputfiles = dir([data_source, '/*spott_50.csv']);
inputfiles = dir([data_source, '/*spott_data.csv']); %CHECK

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
%models = {'time2pl', 'notime', 'zetaexponent'};
models = {'time2pl'};  %CHECK

%inputfiles = inputfiles(23:26);

for mnum = 1:length(models)
    vo=[]; %vba options structure
    vo.dataset = subjID; %'vba_input_simX3';  %CHECK
    %vo.dataset = 'vba_sim_n5_minimal';
    vo.model = models{mnum};
    vo.graphics = 0; %don't display fitting interactively
    vo = validate_options(vo); %initialize and validate suuvid fitting settings
    mdir = [models_dir, filesep, models{mnum}];
    if ~exist(mdir, 'dir'), error('cannot locate model directory: %s', mdir); else, addpath(mdir); end
    vo = m_setup(vo); %setup this model
        
    vo.output_dir = ['/proj/mnhallqlab/projects/spott_modeling/par_rec/rec_fix_omega_kappa', '/outputs/vba_out/ffx/', vo.dataset, '/', vo.model];
    %vo.output_dir = [project_repo, '/outputs/vba_out/ffx/', vo.dataset, '/', vo.model];
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
%         if fit_subj == 1
%             %parsave doesn't work in recent MATLAB versions.
%             m=matfile(o_file, 'writable',true);
%             m.posterior=posterior; m.out=out; m.subj_id=ids{sub}; m.subj_stats=s_all{sub};
%         end
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
