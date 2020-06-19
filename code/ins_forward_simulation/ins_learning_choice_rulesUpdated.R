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
  
  tdiffsec = (tau - rtlast)/1000
  p_respond_tb <- 1 / (1 + exp(-gamma*Qstar*(tdiffsec-nu))) #probability of making a response in this bin
 
  return(p_respond_tb)
}

#probability of switching actions given the value of the:
# unchosen action (Q_u)
# scalar switch (cost)
# chosen action (Q_c)
# 10/2018 update: convert cost into a 0..1 probability treated outside of the softmax
# p_switch <- function(Q_c, Q_u, kappa, cost) {
#   ps <- (1 / (1 + exp(-1*kappa*(Q_u - Q_c) ))) - cost
#   if (ps > 1) { ps <- 1 #enforce 0..1 boundaries after accounting for cost
#   } else if (ps < 0) { ps <- 0 }
#   return(ps)
# }

p_sticky_softmax <- function(Q, cur_action, kappa, cost) {
  stopifnot(length(Q) > 1)
  if (cur_action > length(Q)) { stop("cur_action must be in the set of available actions") }
  
  #allocate vector representing currently active option
  cc <- rep(0, length(Q))
  cc[cur_action] <- 1
  
  m <- kappa*Q + cost*cc
  
  m <- m - max(m) #avoid floating point overflow
  
  p_which <- exp(m)/sum(exp(m))
  return(p_which)
}


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