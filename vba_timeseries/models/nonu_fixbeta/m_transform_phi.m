function [phi_trans] = m_transform_phi(phi)
% Parameter transformation function for parameters in phi (observation function)

phi_trans = [ exp(phi(1)); ... %gamma: positive
            gaminv(fastnormcdf(phi(2)), 2, 1); ... %kappa: Gamma(2,1) transform using inverse CDF approach. Use precompiled std norm cdf code for speed
            phi(3) ... %omega stickiness can be positive or negative -- keep as Gaussian
            ];

end
