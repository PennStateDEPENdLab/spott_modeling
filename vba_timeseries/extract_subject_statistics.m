function [s] = extract_subject_statistics(posterior, out)
s=[];

so = out.options.inF; %so elements should have been copied during get_vba_options

posterior=add_transformed_params(posterior, so);

s.id = so.id;

%% hidden states
s.hidden_states = posterior.muX;

%% posterior parameter estimates
s.muPhi = posterior.muPhi;
s.SigmaPhi = posterior.SigmaPhi;
s.muTheta = posterior.muTheta;
s.SigmaTheta = posterior.SigmaTheta;
s.transformed = posterior.transformed;

%populate ffx params from VBA MFX, if available
if isfield(posterior, 'ffx')
  s.muPhi_ffx = posterior.ffx.muPhi;
  s.SigmaPhi_ffx = posterior.ffx.SigmaPhi;
  s.muTheta_ffx = posterior.ffx.muTheta;
  s.SigmaTheta_ffx = posterior.ffx.SigmaTheta;
end

% compute missing statistics if needed (this is slow...)
if ~isfield(out,'diagnostics')
    out.diagnostics = VBA_getDiagnostics(posterior,out);
end

s.parCorr = out.diagnostics.C;

%% fit statistics
s.fit.F = out.F;
s.fit.LL = out.fit.LL;
s.fit.AIC = out.fit.AIC;
s.fit.BIC = out.fit.BIC;
s.fit.R2 = out.fit.R2;

%% key inputs
s.y = out.y;
s.u = out.u;
s.skipf = out.options.skipf;
s.priors = out.options.priors;

%remove large cell arrays of matrices for hidden state covariance (SigmaX),
% state noise precision (iQx) and measurement noise precision (iQy)
s.priors = rmfield(s.priors, {'SigmaX', 'iQx', 'iQy'}); 

%% model settings
s.model_settings = out.options.inF; %should contain key ingredients from so object and fitting

end
