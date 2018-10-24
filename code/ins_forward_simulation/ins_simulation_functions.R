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


myrollfunc <- function(vec, win=20) {
  require(zoo)
  rollmean(vec, win, align="right", fill=NA)
}

##return key summary statistics from run
get_sim_stats <- function(ins_results, task_environment) {
  #accepts the output of ins_wins as the ins_results input
  
  Q_df <- reshape2::melt(ins_results$Q_tba, varnames=c("trial", "timestep", "action")) %>% 
    mutate(action=factor(action, levels=c(1,2), labels=c("Q_1", "Q_2")))
  
  choices_df <- reshape2::melt(ins_results$choices, varnames=c("trial", "timestep"), value.name="choice")
  rewards_df <- reshape2::melt(ins_results$rewards, varnames=c("trial", "timestep"), value.name="outcome")
  
  trial_plot <- ggplot(Q_df %>% filter(trial < 6), aes(x=timestep, y=value, color=action)) + geom_line() + facet_wrap(~trial, ncol=1)
  
  #Q_tba is Q values trials x timesteps x actions
  #choices_df: action 0 means no action, whereas 1 or 2 denote their respective actions
  
  #this has columnns for Q_1 and Q_2
  
  #don't compute Q ratio if either is 0
  all_df <- Q_df %>% spread(action, value) %>% full_join(choices_df) %>% full_join(rewards_df) %>% 
    arrange(trial, timestep) %>% 
    mutate(outcome=factor(outcome, levels=c(0,1), labels=c("omission", "reward")),
           Q_sum=Q_1 + Q_2, 
           Q_ratio=if_else(Q_1 > 0 & Q_2 > 0, Q_1/Q_2, NA_real_),
           response=as.numeric(choice > 0),  #any response
           reward=if_else(outcome=="reward", 1, 0, missing=0)) %>% #recode 1/0 for reward/omission and make NAs (no response) 0s
    #group_by(trial) %>% #group by trial to get rolling mean to respect each trial
    mutate(response_rate=myrollfunc(response), #a bin is 30 milliseconds, so this is responses per 450ms
           reward_rate=myrollfunc(reward),
           Q_1_roll=myrollfunc(Q_1), Q_2_roll=myrollfunc(Q_2),
           n_1=myrollfunc(choice==1),
           n_2=myrollfunc(choice==2),
           n_1_pref=if_else(n_1 > 0 & n_2 > 0, n_1/n_2, NA_real_),
           Q_sum_roll=myrollfunc(Q_sum), Q_ratio_roll=myrollfunc(Q_ratio)) %>% 
    ungroup() %>%
    mutate(resp_Q_y = case_when(choice==1 ~ Q_1_roll, choice==2 ~ Q_2_roll, TRUE ~ NA_real_))
  
  #Q_ratio_roll, 
  #response_rate, reward_rate
  
  sum_df <- all_df %>% group_by(trial) %>% 
    summarize(nresp=sum(choice > 0), avg_value=sum(Q_1, Q_2),
              n_1=sum(choice==1), n_2=sum(choice==2),
              Q_1=sum(Q_1), Q_2=sum(Q_2),
              Q1_Q2=sum(Q_1) / sum(Q_2),
              Q1_m_Q2=sum(Q_1) - sum(Q_2),
              n1_n2=sum(choice==1) / sum(choice==2),
              n1_m_n2=sum(choice==1) - sum(choice==2))
  
  prew_df <- data.frame(task_environment$prew) %>% setNames(c("p_1", "p_2")) %>% mutate(trial=1:n()) %>%
    mutate(p1_p2=p_1/p_2)
  
  sum_df <- sum_df %>% left_join(prew_df) %>%
    mutate(log_n1_n2 = log(n1_n2), log_p1_p2=log(p1_p2))
  
  return(list(all_df=all_df, sum_df=sum_df))
}

repeat_forward_simulation <- function(params, task_environment, n=100) {
  all_df_outputs <- list()
  sum_df_outputs <- list()
  
  for (i in 1:n) {
    #regenerate outcomes matrix and GRWs
    task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))
    task_environment$outcomes <- with(task_environment, array(runif(n_bins*n_trials*3), dim=c(n_trials, n_timesteps, 3)))  
    
    results <- ins_wins(params, fixed=NULL, task_environment, optimize=FALSE)
    summaries <- get_sim_stats(results, task_environment)
    sum_df <- summaries$sum_df
    all_df <- summaries$all_df
    sum_df$replication <- i
    all_df$replication <- i
    all_df_outputs[[i]] <- all_df
    sum_df_outputs[[i]] <- sum_df
  }
  
  all_df <- bind_rows(all_df_outputs)
  sum_df <- bind_rows(sum_df_outputs)
  return(list(all_df=all_df, sum_df=sum_df))
}

##function to convert simulated data to format the matches Stan model

