%quick BMC

vba_repo = '~/Data_Analysis/spott_modeling';
output_dir = [vba_repo, '/outputs/vba_out/ffx/'];

Lmat=[];
models={'suuvid_base', 'suuvid_fixbeta', 'suuvid_nonu', 'suuvid_nobeta'};

for m=1:numel(models)
    mm = matfile([output_dir, models{m}, '/group_fits_', models{m}, '_pandaa.mat'], 'writable', false);
    Lmat=vertcat(Lmat, mm.L);
end

[bmc_posterior, bmc_out]=VBA_groupBMC(Lmat);

% gg = mm.group_global;
% hist(gg{:,'ar_g_transformed'})
% hist(gg{:,'ar_v_transformed'})
% hist(Lmat(1,:))
% hist(Lmat(2,:))
