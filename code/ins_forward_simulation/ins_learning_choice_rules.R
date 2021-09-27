#functions for learning and choice rules for SPOTT instrumental

# Use 2pl-type approach for computing the probability of emitting a response.
# This variant is based on the total Q (value) being the 'latent ability' (theta) in IRT terms,
# \nu (basal vigor) as difficulty, and \gamma (vigor sensitivity) as discriminability.
p_response <- function(Q, tau=NULL, rt_last=NULL, gamma=1, nu=1, beta=1e-10, eta=1) {
  # Q is a vector of the values of available actions on this trial
  # tau is the current moment in time (or center of the current time bin)
  # rt_last is the time of the last response
  # gamma is vigor sensitivity, essentially scaling the steepness of the response function with changes in value
  #   Default value of gamma (1) means that the model simplifies to 1*(Qstar+nu) if we want one less parameter
  # nu is basal vigor (difficulty in IRT terms) that scales the total value in the environment at which 
  #   p(response) is 0.5 (i.e., Qstar - nu = 0)
  # beta is the recovery rate for motor speed. Higher values lead to slower recovery (lower maximum numerator in response function)
  #   Default value of beta (1e-10) means that if beta is not provided, phi will always be 1.0, dropping this out of the
  #   conceptualization of the 2PL since the numerator is always 1, rather than a lower maximum probability due to motor recovery.
  # eta is not currently used because it could compete with nu (future direction)

  Qstar <- sum(Q)/eta #total environmental value

  phi <- 1 - exp(-(tau - rt_last)/beta) #recovery function (motor speed)

  #_tb denotes (t)rial, time (b)in
  p_respond_tb <- phi / (1+exp(-gamma*(Qstar + nu))) #probability of making a response in this bin
  return(p_respond_tb)
}

# This variant of the 2PL response probability uses the time since the last response (tdiff) to scale the
# probability of responding. In this view, time elapsed is 'latent ability' (theta), meaning that higher values
# push the logistic function toward higher probability.
p_response_tdiff <- function(Q, tau=NULL, rt_last=NULL, gamma=1, nu=1, beta=1e-10, eta=1, no_Q=FALSE) {
  # Q is a vector of the values of available actions on this trial
  # tau is the current moment in time (or center of the current time bin)
  # rt_last is the time of the last response
  # gamma is vigor sensitivity, essentially scaling the steepness of the response function with changes in time elapsed
  #   Default value of gamma (1) means that the model simplifies to -Qstar*(tdiff-nu) if we want one less parameter
  # nu is basal vigor (difficulty in IRT terms) that specifies the time elapsed since last response at which
  #   p(response) is 0.5 (i.e., tdiff - nu = 0)
  # beta is the recovery rate for motor speed. Higher values lead to slower recovery (lower maximum numerator in response function)
  #   Default value of beta (1e-10) means that if beta is not provided, phi will always be 1.0, dropping this out of the
  #   conceptualization of the 2PL since the numerator is always 1, rather than a lower maximum probability due to motor recovery.
  # eta is not currently used because it could compete with nu (future direction)
  # no_Q specifies whether to fit a 2PL where Q does not scale the time elapsed, only gamma
  
  if (isTRUE(no_Q)) {
    Qstar <- 1 #drop Q effects out of equation, simplifying to -gamma * (tdiff - nu)
  } else {
    Qstar <- sum(Q)/eta #total environmental value 
  }
  
  phi <- 1 - exp(-(tau - rt_last)/beta) #recovery function (motor speed)
  
  tdiff = (tau - rt_last)/1000 # rescale parameters in seconds (avoid crazy values)
  
  p_respond_tb <- phi / (1 + exp(-gamma*Qstar*(tdiff-nu))) #probability of making a response in this bin
  return(p_respond_tb)
}


# Lau and Glimcher sticky softmax
# Has softmax choice rule where choices reflect value-based influence (exploitation)
# and prior selection influence (choice stickiness)

# Q is vector of action values
# cur_action is the index of the current action
# kappa is the inverse temperature that scales influence of relative value on choice ('outcome sensitivity')
# omega scales the influence of choice repetition/stickiness ('choice sensitivity')
p_sticky_softmax <- function(Q, cur_action, kappa, omega) {
  stopifnot(length(Q) > 1)
  if (cur_action > length(Q)) { stop("cur_action must be in the set of available actions") }
  
  #allocate vector representing currently active option
  cc <- rep(0, length(Q))
  cc[cur_action] <- 1
  
  m <- kappa*Q + omega*cc
  
  m <- m - max(m) # avoid floating point overflow
  
  p_which <- exp(m)/sum(exp(m)) # standard Boltzmann softmax
  return(p_which)
}

# simple delta learning rule
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


## DEPRECATED

#probability of switching actions given the value of the:
# unchosen action (Q_u)
# scalar switch (cost)
# chosen action (Q_c)
# 10/2018 update: convert cost into a 0..1 probability treated outside of the softmax
p_switch <- function(Q_c, Q_u, kappa, cost) { 
  ps <- (1 / (1 + exp(-1*kappa*(Q_u - Q_c) ))) - cost
  if (ps > 1) { ps <- 1 #enforce 0..1 boundaries after accounting for cost
  } else if (ps < 0) { ps <- 0 }
  return(ps)
}