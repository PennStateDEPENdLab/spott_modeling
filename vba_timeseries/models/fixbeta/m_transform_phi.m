function [phi_trans] = m_transform_phi(phi, inG)
% Parameter transformation function for parameters in phi (observation function)

phi_trans = [ exp(phi(1)); ... %gamma: positive
    phi(2); ... %nu: basal vigor allowed to be positive or negative -- keep as Gaussian
    gaminv(fastnormcdf(phi(3)), 2, 1); ... %kappa: Gamma(2,1) transform using inverse CDF approach. Use precompiled std norm cdf code for speed
    phi(4) ... %omega stickiness can be positive or negative -- keep as Gaussian
    ];

end
