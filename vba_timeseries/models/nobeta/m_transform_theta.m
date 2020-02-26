function [theta_trans] = m_transform_theta(theta)
% Parameter transformation function for parameters in theta (evolution function)

theta_trans = VBA_sigmoid(theta(1)); %sigmoid transform alpha 0..1

end
