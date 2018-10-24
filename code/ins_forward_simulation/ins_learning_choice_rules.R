#functions for learning and choice rules for SPOTT instrumental

#use 2pl-type approach for computing the probability of emitting a response
p_response <- function(Q, tau=NULL, rtlast=NULL, gamma=2, nu=1, beta=1, eta=1) {
  #Q is a vector of the values of available actions on this trial
  #tau is the current moment in time (or center of the current time bin)
  #rtlast is the time of the last response
  #gamma is vigor sensitivity, essentially scaling the steepness of the response function with changes in value
  #nu is basal vigor (difficulty in IRT terms) that scales the total value in the environment at which p(response) is 0.5 (i.e., Qstar - nu=0)
  #beta is the recovery rate for motor speed. Higher values lead to slower recovery (lower maximum numerator in response function)
  #eta is not currently used because it could compete with nu (future direction)
  
  Qstar <- sum(Q)/eta #total environmental value
  
  phi <- 1 - exp(-(tau - rtlast)/beta) #recovery function (motor speed)
  
  #_tb denotes (t)rial, time (b)in
  p_respond_tb <- phi / (1+exp(-gamma*(Qstar + nu))) #probability of making a response in this bin
  return(p_respond_tb)
}

#probability of switching actions given the value of the:
# unchosen action (Q_u)
# scalar switch (cost)
# chosen action (Q_c)
p_switch <- function(Q_c, Q_u, kappa, cost) { 1 / (1 + exp(-1*kappa*(Q_u - cost - Q_c) )) }

#learning rule
# options for variants:
# - decay of unchosen action (probably not plausible in small state space)
# - asymmetric learning rate for wins and losses
Q_next <- function(Q, action=0, outcome=NULL, alpha=NULL) {
  stopifnot(is.numeric(alpha) && alpha > 0 && alpha < 1)
  #if action is 0, no action was taken (bin elapsed)
  if (action > 0) {
    Q[action] <- Q[action] + alpha*(outcome - Q[action]) #update chosen action -- don't change other values
  }
  return(Q)
}