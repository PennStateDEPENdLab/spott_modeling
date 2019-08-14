%%%%%%%%%%%%%%%%%
%%% Written by Quentin J. M. Huys, UCL, London 2011
%%% Reference:
%%% Guitart-Masip M, Quentin JM, Fuentemilla LL, Dayan P, Duzel E, Dolan RJ (2012)
%%% Go and no-go learning in reward and punishment: Interaction between affect and effect NeuroImage doi:10.1016/j.neuroimage.2012.04.024

clear all;
% Load Shiznit
% ya=ya, a=a, s=s, r=r, rew=rewx, N=N, Qi=Qi, yax=yax, Nsub=Nsub, con="/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav.m"
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav1.0.mat');
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav2.0.mat');
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav3.0.mat');
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav4.0.mat');
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav5.0.mat');
load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav6.0.mat');
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav7.0.mat');
%load('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMdataHCSZ.mat');

ThetaType=4;

% Select Model
whichinf=5;
%ff{3} = 'MODEL_q_JFC_40';
ff{1} = 'MODEL_q_JFC_10';
ff{2} = 'MODEL_q_JFC_30';
ff{3} = 'MODEL_q_JFC_40';
ff{4} = 'MODEL_q_JFC_50';  
ff{5} = 'MODEL_B_JFC_60';  
ff{6} = 'MODEL_B_JFC_70a';
ff{7} = 'MODEL_BT_JFC';
ff{8} = 'MODEL_BTv_JFC';

Npar=[5 5 5 5 6 7];
Notes='_SMB_home';   % Type: ("T_A"), base (Ideo, cOmmon)

options=optimset('display','off');     % ,'DerivativeCheck','on'
%warning('off','optim:fminunc:SwitchingMethod')

ThetaTypes={'T_H','T_L','T_R','T_A','T_P'};
T_Label=ThetaTypes{ThetaType};
%for zi=1:34
%    T{zi}=ZSet{zi}(ThetaType,:)';
%end

% load('/home/eeg/eeg/PIT/JFC Scripts/Behavioral_MORE_JFC.mat','Learner_idx');
% LEARNERS=unique([1:34].*(Learner_idx)); LEARNERS=LEARNERS(2:end);

dosave = 0;
docomp = 1;
docheck = 0;
Nsample = 2000;
Np = Npar(whichinf);

for k=1:length(ff); ld{k} = [ff{k} '_' num2str(Npar(whichinf)) Notes]; end

% re-assign up data
A = a;
R = r;
S = s;
T = theta;
C = confl;
Nsj=size(A,1); % get number of subjects; A = Action array

Z.mu=zeros(Np,1); % Z = 
Z.nui=eye(Np);

emit=0;

E = zeros(Np, Nsub);
V = E;
exx = zeros(Nsub,1);
PL = exx;
LL = exx;
while 1;
    
    emit=emit+1; % sj = subjects
    
    % E step......................................................  % Estimation
    
    for sj=1:Nsub;   % LEARNERS
        
        a=A(sj, :); % action vector for this subj
                    
        % a(a==1)=99; a(a==2)=1; a(a==99)=2; % Now: 1=Go, 2=NoGo???
            
        r=R(sj, :);     % reward vector subj sj
        s=S(sj, :);     % stim vector subj sj
        theta=T(sj, :); % Theta vector subj sj
        confl=C; % Theta vector subj sj
        
        ex=-1;tmp=0;tmp1=0;
        while ex<1;
            init=.1*randn(Np,1);    % Initialize Params
            if docheck;             % Don't have checkgrad, so screw it.  Must have been a quality check
                checkgrad(ff{whichinf}, init, .001, a, r, s, theta, Z, 1),
                lkj
            else
                ex=1;
                str='[est,fval,ex,foo,foo,hess] = fminunc(@(x)';
                str=[str ff{whichinf}  '(x, a, r, s, theta, confl, Z, 1),init,options);'];
                eval(str);
                if ex<0 ; tmp=tmp+1; fprintf('didn''t converge %i times exit status %i\r',tmp,ex); end
            end
        end
        
        exx(sj)=ex;
        E(:,sj)=est;					% Subjets' parameter estimates
        V(:,sj) = diag(inv(full(hess)));	% inverse of Hessian = variance
        PL(sj) = fval;					% posterior likelihood
        eval(['LL(sj)=' ff{whichinf} '(est,a,r,s,theta,confl,Z,0);']);	% likelihood
        
 %        fprintf('Emit=%i subject %i exit status=%i\r',emit,sj,exx(sj))
    end
    
    
    % M step using factorized posterior .................................
    mu = mean(E,2);
    nu = sqrt(sum(E.^2 + V,2)/Nsj - mu .^2);
    Z.mu = mu; Z.nui = inv(diag(nu));
    
    [mu nu]
    
    par(emit,:) = [sum(LL) sum(PL) mu' nu(:)'];						% save stuff
    et(emit,:,:) = E;
    if dosave;
        eval(['save mat/' ld{whichinf} ' mu nu E V LL PL par et Z exx emit ld ff whichinf;']);
    end
    % check convergence ...........................................
    if emit>1;if sum(abs(par(emit,2:end)-par(emit-1,2:end)))<1e-3;fprintf('\n *** converged *** \n');break;end;end
end

%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput2.0.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput3.0.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput4.0.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput5.0.mat', 'E')
save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput6.0.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMdataHCSZOutput60.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMdataHCSZOutput60i.mat', 'E')
%save('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput70a.mat', 'E')


