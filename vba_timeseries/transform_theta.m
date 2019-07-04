function [theta_trans] = transform_theta(theta, inF)

if ismatrix(theta) && ~isvector(theta)
    asymm_check = abs(theta - theta') > 1e-5; %if ~issymmetric(theta) %this objects to tiny floating point imprecision??
    if any(asymm_check), error('Non-symmetric matrix passed to transform_theta'); end
    
    if ismember(inF.model, {'suuvid_base'})
        vars = diag(theta)'; %make sure this is a row vector
        sds_trans = sqrt([VBA_sigmoid(vars(1))]); %transform the parameters to get variances, then compute SDs
        theta_trans = transform_covmat(theta, sds_trans); %compute covariance matrix on rescaled parameters
    else
        theta_trans = theta; %just return untransformed for the moment...
    end
else
    
    if strcmpi(inF.model, 'suuvid_base')
        theta_trans = [VBA_sigmoid(theta(1))]; %sigmoid transform alpha
    else
        error(['unrecognized model in transform_theta: ', inF.model]);
    end
end

end