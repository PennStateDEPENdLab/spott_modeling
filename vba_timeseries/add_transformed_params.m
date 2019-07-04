function [posterior] = add_transformed_params(posterior, vo)

posterior.transformed.muPhi = transform_phi(posterior.muPhi, vo);
posterior.transformed.SigmaPhi = transform_phi(posterior.SigmaPhi, vo);

posterior.transformed.muTheta = transform_theta(posterior.muTheta, vo);

posterior.transformed.SigmaTheta = transform_theta(posterior.SigmaTheta, vo);

%in MFX case, populate transformed ffx variants
if isfield(posterior, 'ffx')
  posterior.transformed.muPhi_ffx = transform_phi(posterior.ffx.muPhi, vo);
  posterior.transformed.SigmaPhi_ffx = transform_phi(posterior.ffx.SigmaPhi, vo);
  
  posterior.transformed.muTheta_ffx = transform_theta(posterior.ffx.muTheta, vo);
  posterior.transformed.SigmaTheta_ffx = transform_theta(posterior.ffx.SigmaTheta, vo);
end

end