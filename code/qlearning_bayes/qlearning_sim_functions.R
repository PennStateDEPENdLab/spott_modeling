#function for simulating data from a Qlearning model
get_omat <- function(ntrials, preward) {
  outcomes <- sapply(preward, function(o) { rbinom(ntrials, size=1, prob=o) })  
}

softmax <- function(Q, beta, inverse_temperature=TRUE) {
  if (inverse_temperature) {
    #Multiply by inverse temperature (higher values = more exploitative)
    ( exp((Q - max(Q))*beta) ) / (sum( exp((Q - max(Q))*beta) ))
  } else {
    #Divide by temperature (higher values = more stochastic)
    ( exp((Q - max(Q))/beta) ) / (sum( exp((Q - max(Q))/beta) )) 
  }
}

#simulate data for a subject with parameters alpha and beta
simsubject <- function(alpha, beta, outcomes, ...) {
  ntrials <- nrow(outcomes)
  Q <- matrix(NA, nrow=nrow(outcomes)+1, ncol=ncol(outcomes))
  R <- matrix(NA, nrow=nrow(outcomes), ncol=ncol(outcomes))
  choices <- rep(NA, nrow(outcomes))
  Q[1,] <- 0 #initialize no expectation of reward
  ovec <- 1:ncol(outcomes) #vector of possible choices
  for (i in 1:ntrials) {
    #softmax choice using weighted probabilities
    choice <- choices[i] <- sample(x=ncol(outcomes), size=1, prob=softmax(Q[i,], beta, ...))
    
    #harvest outcome
    R[i,choice] <- outcomes[i,choice]
    
    #learn from it
    Q[i+1,choice] <- Q[i,choice] + alpha*(R[i,choice] - Q[i,choice])
    
    #carry forward other values
    Q[i+1, ovec[!ovec == choice]] <- Q[i, ovec[!ovec == choice]]
  }
  
  return(list(Q=Q, R=R, choices=choices, alpha=alpha, beta=beta, outcomes=outcomes))
}

#preward controls the probability of rewards for each of k options. The length of the vector
#determines the number of actions in the environment
sim_qlearning_group <- function(nsubjects=50, ntrials=50, preward=c(0.9, 0.1),
                                alpha_dist=list(meana = 0.1, mina = 0.02, maxa = 0.4, sda = 0.06),
                                beta_dist=list(meanb = 1, minb = 0.1, maxb = 10, sdb = 0.3), #this is inverse temp
                                quiet=FALSE, seed=1050, ...) {
  
  require(truncnorm)
  set.seed(seed) #keep sims consistent
  
  #simulate subject parameters
  alpha_subj <- rtruncnorm(nsubjects, a=alpha_dist$mina, b=alpha_dist$maxa, mean = alpha_dist$meana, sd = alpha_dist$sda)
  if (is.null(beta_dist$fixed)) {
    beta_subj  <- rtruncnorm(nsubjects, a=beta_dist$minb, b=beta_dist$maxb, mean = beta_dist$meanb, sd = beta_dist$sdb)  
  } else {
    beta_subj  <- rep(beta_dist$fixed, nsubjects)
  }
  
  
  slist <- list()
  for (s in 1:nsubjects) {
    omat <- get_omat(ntrials, preward)
    
    slist[[s]] <- simsubject(alpha_subj[s], beta_subj[s], omat, ...)
    slist[[s]]$rvec <- apply(slist[[s]]$R, 1, function(row) { row[which(!is.na(row))] })
  }
  
  rewards <- do.call(rbind, lapply(slist, function(s) { s$rvec } ))
  choices <- do.call(rbind, lapply(slist, function(s) { s$choices } ))
  
  if (!quiet) {
    cat("Simulated Q-learning data for", nsubjects, "subjects with", ntrials, "trials\n")
    cat("Reward probabilities:", paste(preward, collapse=", "), "\n")
    cat("Learning rate simulated from truncated normal with parameters:\n  min =", 
        alpha_dist$mina, ", mean =", alpha_dist$meana, ", max =", alpha_dist$maxa, ", sd = ", alpha_dist$sda, "\n")
    cat("Summary statistics for simulated alphas:\n")
    print(summary(alpha_subj))
    cat("Beta (inverse temperature) simulated from truncated normal with parameters:\n  min =", 
        beta_dist$minb, ", mean =", beta_dist$meanb, ", max =", beta_dist$maxb, ", sd = ", beta_dist$sdb, "\n")
    cat("Summary statistics for simulated betas:\n")
    print(summary(beta_subj))
  }
  
  return(list(slist=slist, rewards=rewards, choices=choices, alpha_subj=alpha_subj, beta_subj=beta_subj))
}


#    In binary, softmax is: p(choice = A | X) = 1 / (1 + exp(-beta*(Q_A - Q_B))      
#    Then the loglikelihood function = sum(log( p(choice(t) = A | X) )) over all trials t and you use numerical optimization to maximize this... 
#    Or since most optimizers minimize an objective function, you minimize the negative LL.
#
#    More generally, sum the log of choice probabilities for the chosen options (since we want the model to align with data)

fitQ <- function(params, outcomes, choices, priorQ=NULL, optim=TRUE) {
  #priorQ vector must be named Q<choice 1>, Q2<choice 2>, ...
  alpha <- params[1] #learning rate
  beta <- params[2] #softmax temperature
  #choices should be integer-valued actions for each trial 
  ntrials <- length(outcomes)
  stopifnot(length(outcomes) == length(choices))
  nactions <- length(unique(choices))
  #stopifnot(min(choices) == 1)
  action_lookup <- 1:nactions #key value pair. Key (name)
  names(action_lookup) <- as.character(sort(unique(choices)))
  #cat("Choice mapping:\n")
  #print(action_lookup)
  Q <- matrix(NA, nrow=ntrials, ncol=nactions, dimnames=list(trial=1:ntrials, action=paste0("Q", names(action_lookup))))
  pchoice <- matrix(NA, nrow=ntrials, ncol=nactions, dimnames=list(trial=1:ntrials, action=paste0("p", names(action_lookup))))
  if (is.null(priorQ)) {
    Q[1,] <- 0 #initial expected value of zero for all actions        
  } else {
    #stopifnot(length(priorQ) == ncol)
    Q[1,] <- priorQ[dimnames(Q)$action] #pull just the correct elements out of priorQ
  }
  
  
  for (i in 2:ntrials) {
    whichcol <- action_lookup[names(action_lookup)==as.character(choices[i])]
    #learning rule (chosen option)
    Q[i,whichcol] <- Q[i-1,whichcol] + alpha*(outcomes[i] - Q[i-1,whichcol])
    
    #carry forward other columns (note that one could just keep a scalar value for each choice
    #more parsimonious, but not as complete in terms of seeing under the hood of Q
    Q[i,action_lookup[!whichcol==action_lookup]] <- Q[i - 1,action_lookup[!whichcol==action_lookup]] 
    vchoice <- exp(beta*Q[i,])
    pchoice[i,] <- vchoice/sum(vchoice)
  }
  
  #log likelihood is the log sum of the probability of choosing the chosen action
  ll = 0
  for (a in 1:nactions) {
    ll = ll + sum(log(na.omit(pchoice[ choices==as.numeric(names(action_lookup)[a]), a ])))
  }
  
  if (optim) {
    return(-ll) #only return negative LL for optimization  
  } else {
    return(list(ll=-ll, Q=Q, pchoice=pchoice, action_lookup=action_lookup, 
                alldf=data.frame(trial=1:ntrials, Q, pchoice, rewards=outcomes, choices=choices)))
  }
  
}