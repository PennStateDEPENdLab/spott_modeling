function [vo, poolobj, behavfiles] = spott_setup_environment(vo)
%this function sets up the paths for sceptic, VBA, and expected behavior files based on the user's environment
%it also handles setup of the parpool depending on the user

%os = computer;
[~, me] = system('whoami');
me = strtrim(me);

[~, host]=system('hostname');
host = strtrim(host);

is_aci = contains(host, 'aci.ics.psu.edu');

is_alex = ismember(me, {'Alex', 'dombax'});
is_jiazhouchen = ismember(me, {'jiazhouchen'});
is_mnh = ismember(me, {'mnh5174', 'michael'});

if is_mnh 
    if is_aci
        vo.mh_vba_path='/gpfs/group/mnh5174/default/Michael/mh_vba'; %MH helper scripts
        vo.vba_path = '/storage/home/mnh5174/MATLAB/VBA-toolbox';
    else
        %local mac installation
        vo.mh_vba_path='/Users/mnh5174/Data_Analysis/mh_vba'; %MH helper scripts
        vo.vba_path = '~/Documents/MATLAB/VBA-toolbox';
        vo.ncpus=2;
    end
end

addpath(vo.mh_vba_path); %add MH VBA helpers (e.g., extract_group_statistics)
addpath(genpath_safe(vo.vba_path)); %add VBA functions

if ~isfield(vo, 'do_parallel'), vo.parallel=true; end %default to parallel execution

if vo.do_parallel
    delete(gcp('nocreate')); %stop any pool that is running 
end

ncpus=getenv('matlab_cpus');
if strcmpi(ncpus, '')
    ncpus=40;
    fprintf('defaulting to 40 cpus because matlab_cpus not set\n');
else
    ncpus=str2double(ncpus);
end

poolobj=parpool('local',ncpus); %just use shared pool for now since it seems not to matter (no collisions)


% identify subject behavior files
% group_dir=[sceptic_repo, '/subjects']; %not used for anything at the moment...

if strcmpi(vo.dataset,'mmclock_meg')
  behavfiles = glob([sceptic_repo, '/subjects/mmclock_meg/*.csv']);
elseif strcmpi(vo.dataset,'mmclock_fmri')
  behavfiles = glob([sceptic_repo, '/subjects/mmclock_fmri/*.csv']);
elseif strcmpi(vo.dataset,'specc')
  behavfiles = glob([sceptic_repo, '/subjects/SPECC/*.csv']);
elseif strcmpi(vo.dataset,'explore')
    rootdir = sprintf(fullfile(boxdir,'skinner','data','eprime','clock_reversal'));
    behavfiles = glob([rootdir, '/*/*.mat']);
end

if is_alex
  if strcmp(me,'dombax')'
    vo.output_dir = '/Volumes/bek/Box Sync/skinner/projects_analyses/SCEPTIC/mfx_analyses';
  else
    vo.output_dir = '/Users/localadmin/Box Sync/skinner/projects_analyses/SCEPTIC/fMRI_paper/vba_output';
  end
elseif is_jiazhouchen
  vo.output_dir = fullfile(boxdir,'skinner/projects_analyses/SCEPTIC/fMRI_paper/vba_output');
else
  vo.output_dir = [sceptic_repo, '/vba_fmri/vba_out/', vo.dataset, '/mfx/', vo.model];
  if ~exist(vo.output_dir, 'dir'), mkdir(vo.output_dir); end
end

% setup parallel parameters
if is_alex
  if strcmp(me,'dombax')==1
    ncpus = 12;
    fprintf('defaulting to 12 cpus on Thorndike \n');
  else
    ncpus=10;
    fprintf('defaulting to 10 cpus on iMac Pro \n');
  end
  poolobj = gcp('nocreate');
  if isempty(poolobj)
    poolobj=parpool('local',ncpus); %just use shared pool for now since it seems not to matter (no collisions)
  end
elseif is_jiazhouchen
  ncpus=4;
  poolobj = gcp('nocreate');
  if isempty(poolobj)
    poolobj=parpool('local',ncpus); %just use shared pool for now since it seems not to matter (no collisions)
  end
else
  ncpus=getenv('matlab_cpus');
  if strcmpi(ncpus, '')
    ncpus=40;
    fprintf('defaulting to 40 cpus because matlab_cpus not set\n');
  else
    ncpus=str2double(ncpus);
  end
  
  poolobj=parpool('local',ncpus); %just use shared pool for now since it seems not to matter (no collisions)
end

end
