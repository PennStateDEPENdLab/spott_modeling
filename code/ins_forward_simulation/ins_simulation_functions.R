#functions to estimate ins model performance given a task environment and set of parameters

#cost function is total points earned given the contingency
#this function finds optimal parameters for the model given the current environment
simulate_ins_performance <- function(initial_params, fixed=NULL, task_environment, optimizer="nlminb", profile=FALSE) {
  #track optimization
  if (profile) { 
    prof_file <- tempfile(pattern="Rprof", tmpdir=tempdir(), fileext=".out")
    Rprof(prof_file)
  }
  
  if (optimizer=="optim") {
    #call predict using optimizer
    if (length(initial_params$value) == 1L) { method="Brent"
    } else { method="L-BFGS-B" }
    
    elapsed_time <- system.time(optResult <- optim(par=initial_params$value, fn=ins_wins, method=method,
                                                   lower=initial_params$lower, upper=initial_params$upper,
                                                   task_environment=task_environment, fixed=fixed,
                                                   control=list(parscale=initial_params$par_scale)))
  } else if (optimizer=="nlminb") {
    #this is the most sensible, and corresponds to optim above (and is somewhat faster)
    elapsed_time <- system.time(optResult <- nlminb(start=initial_params$value, objective=ins_wins, 
                                                    lower=initial_params$lower, upper=initial_params$upper, 
                                                    task_environment=task_environment, fixed=fixed,
                                                    scale=1/initial_params$par_scale, control=list(eval.max=500, iter.max=500)))
    
  }
  
  optResult$timing <- elapsed_time
  return(optResult)
  
}

#helper function to divide parameters to optimize versus fix
split_free_fixed <- function(params, fixed=NULL) {
  if (is.null(fixed)) {
    return(list(free=params, fixed=NULL))
  } else {
    fpars <- fixed
    
    #if NA is passed for a free parameter, copy the value across
    napars <- names(fpars)[which(is.na(fpars))]
    fpars[napars] <- params$value[napars]

    free <- lapply(params, function(el) {
      el[!names(el) %in% names(fixed)]
    })
    return(list(free=free, fixed=fpars))
  }
}

#this is the main worker to simulate model earnings and performance in a given environment
#if optimize=TRUE, only the (negative) earnings are returned as the objective function
#if optimize=FALSE, the model outputs at the current parameters are provided in a list
ins_wins <- function(params, fixed=NULL, task_environment=NULL, optimize=TRUE) {
  params <- c(params, fixed) #add any fixed parameters to named vector
  prew <- task_environment$prew
  n_trials <- task_environment$n_trials
  trial_length <- task_environment$trial_length
  bin_size <- task_environment$bin_size
  time_resolution <- task_environment$time_resolution
  outcomes <- task_environment$outcomes
  n_timesteps <- task_environment$n_timesteps #I have confounded thinking between bins and timesteps...
  
  choices <- matrix(0, nrow=n_trials, ncol=n_timesteps)
  rewards <- matrix(NA_real_, nrow=n_trials, ncol=n_timesteps)
  time_vec <- seq(0, trial_length, by=time_resolution)
  
  #allow for trial-varying prew: trials x choices
  if (is.vector(prew)) {
    all_choices <- 1:length(prew)
    prew <- pracma::repmat(prew, n_trials, 1) #if we get a vector, replicate onto rows (trials)
  } else {
    stopifnot(nrow(prew) == n_trials)
    all_choices <- 1:ncol(prew) #assume we have an ntrials x nchoices reward matrix
  } #handle within trial variation?
  
  current_choices <- sample(all_choices, 2) #first element is chosen, second is unchosen. there has to be an initial chosen action (even if not emitted)
  #inactive_choice <- all_choices[all_choices != active_choice]
  Q_tba = array(NA_real_, dim=c(n_trials, n_timesteps, length(all_choices))) #zero priors on first trial + timestep
  Q_tba[1,,] <- 0
  
  #loop over trials and timesteps
  for (i in 1:n_trials) {
    rt_last <- 0 #reset trial start
    for (j in 1:n_timesteps) {
      emit_response <- p_response(Q_tba[i,j,], tau = time_vec[j], rtlast = rt_last, gamma=params["gamma"], nu=params["nu"], beta=params["beta"])
      
      #decide whether to emit a response
      if (emit_response > outcomes[i,j,1]) {
        c_ij <- p_switch(Q_c=Q_tba[i,j,current_choices[1]], Q_u=Q_tba[i,j,current_choices[2]], kappa=params["kappa"], cost=params["cost"])
        if (c_ij > outcomes[i,j,2]) {
          #switch actions
          #active_choice <- all_choices[all_choices != active_choice]
          #inactive_choice <- all_choices[all_choices != active_choice]
          current_choices <- rev(current_choices) #for binary choice, just swap vector (first position is active choice, second is inactive)
        }
        
        choices[i,j] <- current_choices[1] #choose the current action      
      }

      #harvest outcome if response is emitted
      if (choices[i,j] != 0) {
        rewards[i,j] <- as.numeric(outcomes[i,j,3] < prew[i,current_choices[1]]) #harvest reward on action
        rt_last <- time_vec[j] #update the last response time
      }
      
      #evolve Q vector
      if (j < n_timesteps) {
        #update value for next timestep
        Q_tba[i,j+1,] <- Q_next(Q_tba[i,j,], action=choices[i,j], outcome=rewards[i,j], alpha=params["alpha"])
      } else if (i < n_trials) {
        #carry learning to first timestep of next trial (assumption)
        Q_tba[i+1,1,] <- Q_next(Q_tba[i,j,], action=choices[i,j], outcome=rewards[i,j], alpha=params["alpha"])
      }
      
    }
  }
  
  if (optimize) {
    return(-1*sum(rewards, na.rm=TRUE)) #only return earnings to optimizer. negate earnings since optimizer minimizes the objective
  } else {
    return(list(Q_tba=Q_tba, choices=choices, rewards=rewards)) #return full structure
  }
  
}

#gaussian random walk with reflecting boundaries
grwalk <- function(len, start=0.5, step_sd=0.025, max_p=0.8, min_p=0.2)  {
  stopifnot(start > min_p && start < max_p) #not sure what we'd do otherwise...
  probs <- rep(NA, len)
  rvec <- rnorm(len, mean=0, sd=step_sd)
  probs[1] <- start
  for (i in 2:len) {
    test <- probs[i-1] + rvec[i]
    if (test > max_p || test < min_p) {
      incr <- -1*rvec[i]
    } else {
      incr <- rvec[i]
    }
    
    probs[i] <- probs[i-1] + incr
  }
  
  return(probs)
}
