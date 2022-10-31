#functions to estimate ins model performance given a task environment and set of parameters

# helper function to setup a task environment consisting of actions, reward probabilities,
# trials, time duration of trials, and time bins
setup_task_environment <- function(model=NULL, prew=list(0.3, 0.3), n_trials=200, trial_ms=6000, bin_ms=50, schedule="VR") {
  # prew <- lapply(prew, eval.parent, n=1) # evaluate all prew expressions
  # prew <- lapply(prew, eval, envir = parent.frame()) # RM for 6/21/22 edited from the line above so it runs within foreach()
  # prew <- lapply(prew, eval, envir=parent.frame(n=2)) # RM: this still doesn't work
  # prew <- lapply(prew, function(x) { browser()  }) 
  prew <- lapply(prew, function(e_i, n_trials_inner) { eval(e_i)  }, n_trials_inner=n_trials) 
  lens <- sapply(prew, length)
  stopifnot(length(unique(lens)) == 1L)
  prew <- do.call(cbind, prew) #make into a matrix
  
  task_environment <- list(
    prew=prew, # evaluate all prew expressions
    n_actions = length(prew),
    n_trials = n_trials,
    trial_ms = trial_ms, #6 seconds
    bin_ms = bin_ms, # ms
    n_timesteps = trial_ms/bin_ms,
    model=model,
    schedule = schedule #VI: default is VR for variable ratio; can input VI for variable interval
  )
  
  # These random numbers need to be constant in optimization so that the cost function is on the same scale across iterations
  # random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
  
  # Need a unique RNG seed for each trial x timestep to make sure that sample() call in sticky softmax is
  # reproducible across iteration in optimization.
  task_environment$rand_p_which <- with(task_environment, array(sample.int(n=n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
  task_environment$rand_p_respond <- with(task_environment, array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
  
  if (schedule == "VR"){
    #task_environment$rand_p_respond <- with(task_environment, array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
    task_environment$rand_p_reward <-  with(task_environment, array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
  } else if (schedule == "VI"){ #for VI, rand_p_reward is "deterministic" following the VI set up
    times <- seq(0, trial_ms, by = 50)/1000 #starting from 0, used to count time past from the last reward
    
    # Using n_timesteps here rather than length(times) because n_timesteps is the number of time intervals (in which a response can occur)
    # Transposed to match the dimension in VR: 1 trial x n_timesteps
    #task_environment$rand_p_respond <- t(rbinom(task_environment$n_timesteps, size = 1, prob=0.5)) 
    
    # Initializing variables x and rewarded
    # x has size n_timesteps x ncol(prew); 
    # programmed intervals (for VI) to be waited before the next reward; max # needed is n_timesteps (i.e., max # of responses possible)
    # Though it seems like using length(times) vs. n_timesteps won't make a difference because we look at each interval at a time point
    # E.g., Responses during 0-0.05 interval is marked as occuring at time 0.05
    x<- matrix(rep(NA, task_environment$n_timesteps*2), ncol = ncol(prew))
    
    # rewarded will contain the reward of each interval for each choice; size n_timesteps x ncol(prew)
    rewarded <- matrix(rep(NA, task_environment$n_timesteps*2), ncol = ncol(prew))
    for (k in 1:ncol(prew)){
      x[,k] <- rgamma(task_environment$n_timesteps, rate=prew[k], shape = 4) # VI schedule for choice k
      
      i <- 1 # interval that's being sampled/used
      last_rew <- 0 #last rewarded time
      time_i <- x[i,k]
      for (t in 1:task_environment$n_timesteps) { #what's happening during each time interval #if using length(times), should -1, because there are only length(times)-1 possible response intervals
        if (task_environment$rand_p_respond[t] == 0) {
          rewarded[t,k] <- 0
        } else{
          
          # t+1 because, for e.g., response during the first interval 0-0.05 is recorded at the 2nd time point 0.05; that is, 0.05 time has elapsed since the start
          t_elapsed <- times[t+1] - last_rew # max t is length(times)-1, so should be all within bounds
          if (t_elapsed >= time_i) {
            rewarded[t,k] <- 1
            last_rew <- times[t+1]
            i <- i+1
            time_i <- x[i,k]
          } else {
            rewarded[t,k] <- 0
          }
        }
      }
      
    }
    task_environment$rand_p_reward <- rewarded # For "VI," this is not a probability, but using this name for now
  }
  
  return(task_environment)
  
}


# Cost function is total points earned given the contingency
# This function finds optimal parameters for the model given the current environment
# In computational RL terms, this should find maximum likelihood parameters for one subject
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
ins_wins <- function(params, fixed=NULL, task_environment=NULL, optimize=TRUE, prior_Q=0) {
  
  params <- c(params, fixed) #add any fixed parameters to named vector

  model <- task_environment$model
  checkmate::assert_string(model, null.ok = FALSE)
  
  prew <- task_environment$prew #VI: not relevant
  n_trials <- task_environment$n_trials #VI: default to 1
  trial_ms <- task_environment$trial_ms #VI: default to 200*6000
  bin_ms <- task_environment$bin_ms
  outcomes <- task_environment$outcomes
  rand_p_which <- task_environment$rand_p_which
  rand_p_reward <- task_environment$rand_p_reward #VI: not relevant
  rand_p_respond <- task_environment$rand_p_respond  
  n_timesteps <- task_environment$n_timesteps #I have confounded thinking between bins and timesteps...
  schedule <- task_environment$schedule
  choices <- matrix(0, nrow=n_trials, ncol=n_timesteps)
  rewards <- matrix(NA_real_, nrow=n_trials, ncol=n_timesteps)
  time_vec <- seq(0, trial_ms, by=bin_ms)
  
  if (model == "nonu" && params["nu"] != 0) {
    warning("nonu model has non-zero nu")
    params["nu"] <- 0
  } else if (model == "nobeta" && params["beta"] != 1.0) {
    warning("nobeta model has beta != 1.0")
    params["beta"] <- 1
  }
  
  #allow for trial-varying prew: trials x choices
  if (is.vector(prew)) {
    all_choices <- 1:length(prew)
    prew <- pracma::repmat(prew, n_trials, 1) #if we get a vector, replicate onto rows (trials)
  } else {
    stopifnot(nrow(prew) == n_trials)
    all_choices <- 1:ncol(prew) #assume we have an ntrials x nchoices reward matrix
  } #handle within trial variation?
  
  active_action <- sample(all_choices, 1) #first element is chosen, second is unchosen. there has to be an initial chosen action (even if not emitted)
  #inactive_choice <- all_choices[all_choices != active_choice]
  Q_tba = array(NA_real_, dim=c(n_trials, n_timesteps, length(all_choices))) #zero priors on first trial + timestep
  Q_tba[1,,] <- prior_Q
  
  # setup local function for p_response based on model
  if (model %in% c("main", "value2pl", "nobeta", "nonu")) {
    loc_presp <- function(Q, tau, rt_last) {
      p_response(Q, tau = tau, rt_last = rt_last,
                 gamma=params["gamma"], nu=params["nu"], beta=params["beta"])  
    }
  } else if (model %in% c("time2pl")) {
    # at present time2pl variants do not have a beta parameter (numerator is always 1.0)
    loc_presp <- function(Q, tau, rt_last) {
      p_response_tdiff(
        Q, tau = tau, rt_last = rt_last, 
        gamma=params["gamma"], nu=params["nu"], no_Q=FALSE) # beta=params["beta"], 
    }
  } else if (model %in% c("exp")) {
    loc_presp <- function(Q, tau, rt_last) {
      p_response_exp(
        Q, tau=tau, rt_last = rt_last, 
        gamma=params["gamma"], nu=params["nu"])  
    }
  } else if (model == "time2pl_noQ") {
    loc_presp <- function(Q, tau, rt_last) {
      p_response_tdiff(
        Q, tau=time_vec[j], rt_last = rt_last, 
        gamma=params["gamma"], nu=params["nu"], no_Q=TRUE) # beta=params["beta"], 
    }
  } else if (model == "notime") {
    stop("Not implemented")
  }
  
  #loop over trials and timesteps
  for (i in 1:n_trials) {
    rt_last <- 0 #reset trial start
    for (j in 1:n_timesteps) {
      emit_response <- loc_presp(Q_tba[i,j,], tau = time_vec[j], rt_last = rt_last)
      
      #decide whether to emit a response
      if (emit_response > rand_p_respond[i,j]) {
        p_ij <- p_sticky_softmax(Q_tba[i,j,], cur_action=active_action, kappa=params["kappa"], omega=params["omega"])
        set.seed(rand_p_which[i,j])
        c_ij <- sample(all_choices, size=1, prob=p_ij)
        
        if (c_ij != active_action) {
          # new action chosen
          active_action <- c_ij
        }
        
        choices[i,j] <- active_action #choose the current action
      }
      
      #harvest outcome if response is emitted
      if (choices[i,j] != 0) {
        if (schedule == "VI"){
          rewards[i,j] <- rand_p_reward[j, active_action]
        } else{ #VR
          rewards[i,j] <- as.numeric(rand_p_reward[i,j] < prew[i,active_action]) #harvest reward on action
        }
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

# gaussian random walk with reflecting boundaries -- useful for simulations
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

# simple rolling average
myrollfunc <- function(vec, win=20) {
  require(zoo)
  rollmean(vec, win, align="right", fill=NA)
}

# return key summary statistics from run
get_sim_stats <- function(ins_results, task_environment, get_rolling_stats = FALSE, make_trial_plot=FALSE) {
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  #accepts the output of ins_wins as the ins_results input
  
  n_actions <- dim(ins_results$Q_tba)[3]
  Q_df <- reshape2::melt(ins_results$Q_tba, varnames=c("trial", "timestep", "action")) %>%
    dplyr::mutate(action=factor(action, levels=1:n_actions, labels=paste0("Q_", 1:n_actions)))
  
  choices_df <- reshape2::melt(ins_results$choices, varnames=c("trial", "timestep"), value.name="choice")
  rewards_df <- reshape2::melt(ins_results$rewards, varnames=c("trial", "timestep"), value.name="outcome")
  
  if (isTRUE(make_trial_plot)) {
    trial_plot <- ggplot(Q_df %>% filter(trial < 8), aes(x=timestep, y=value, color=action)) + geom_line() + facet_wrap(~trial, ncol=1)  
  } else {
    trial_plot <- NULL
  }
  
  #Q_tba is Q values trials x timesteps x actions
  #choices_df: action 0 means no action, whereas 1 or 2 denote their respective actions
  
  #this has columnns for Q_1 and Q_2
  
  #N.B. These mutate calls are only written for the 2-choice case (not yet generalized)
  #don't compute Q ratio if either is 0
  all_df <- Q_df %>% spread(action, value) %>% full_join(choices_df, by=c("trial", "timestep")) %>% full_join(rewards_df, by=c("trial", "timestep")) %>% 
    arrange(trial, timestep) %>% 
    mutate(outcome=factor(outcome, levels=c(0,1), labels=c("omission", "reward")),
           Q_sum=Q_1 + Q_2, 
           Q_ratio=if_else(Q_1 > 0 & Q_2 > 0, Q_1/Q_2, NA_real_),
           response=as.numeric(choice > 0),  #any response
           reward=if_else(outcome=="reward", 1, 0, missing=0)) #recode 1/0 for reward/omission and make NAs (no response) 0s
  
  # rolling response rates, Q_1, etc. in a 30-bin window (this is slow, so off by default)
  if (isTRUE(get_rolling_stats)) {
    all_df <- all_df %>%
      group_by(trial) %>% #group by trial to get rolling mean to respect each trial
      mutate(response_rate=myrollfunc(response), #a bin is 30 milliseconds, so this is responses per 450ms
             reward_rate=myrollfunc(reward),
             Q_1_roll=myrollfunc(Q_1), Q_2_roll=myrollfunc(Q_2),
             n_1=myrollfunc(choice==1),
             n_2=myrollfunc(choice==2),
             n_1_pref=if_else(n_1 > 0 & n_2 > 0, n_1/n_2, NA_real_),
             Q_sum_roll=myrollfunc(Q_sum), Q_ratio_roll=myrollfunc(Q_ratio)) %>%
      ungroup() %>%
      mutate(resp_Q_y = case_when(choice==1 ~ Q_1_roll, choice==2 ~ Q_2_roll, TRUE ~ NA_real_))
  }
  
  #N.B. These summaries are only written for the 2-choice case.
  sum_df <- all_df %>% group_by(trial) %>% 
    summarize(nresp=sum(choice > 0), avg_value=sum(Q_1, Q_2),
              n_1=sum(choice==1), n_2=sum(choice==2),
              Q_1=sum(Q_1), Q_2=sum(Q_2),
              Q1_Q2=sum(Q_1) / sum(Q_2),
              Q1_m_Q2=sum(Q_1) - sum(Q_2),
              n1_n2=sum(choice==1) / sum(choice==2),
              n1_m_n2=sum(choice==1) - sum(choice==2))
  
  #allow for trial-varying prew: trials x choices
  if (is.vector(task_environment$prew)) {
    #if we get a vector, replicate onto rows (trials)
    task_environment$prew <- pracma::repmat(task_environment$prew, task_environment$n_trials, 1)
  } else {
    stopifnot(nrow(task_environment$prew) == task_environment$n_trials)
  }
  
  if (task_environment$schedule == "VI") {
    
    prew_df <- data.frame(choices = t(as.matrix(ins_results$choices)), rewards = t(as.matrix(ins_results$rewards))) %>% 
      summarize(p_1 = sum(rewards[choices==1]), p_2 = sum(rewards[choices==2])) %>%
      mutate(trial=1:n()) %>%
      mutate(p1_p2=p_1/p_2)
    
  } else {#VR
    prew_df <- data.frame(task_environment$prew) %>% setNames(paste0("p_", 1:n_actions)) %>% mutate(trial=1:n()) %>%
      mutate(p1_p2=p_1/p_2)

  }
  
  sum_df <- sum_df %>% left_join(prew_df, by="trial") %>%
    mutate(log_n1_n2 = log(n1_n2), log_p1_p2=log(p1_p2))
  
  return(list(all_df=all_df, sum_df=sum_df, trial_plot=trial_plot))
}

repeat_forward_simulation <- function(params, task_environment, n=100) {
  all_df_outputs <- list()
  sum_df_outputs <- list()
  
  if (task_environment$schedule == "VI"){
    for (i in 1:n) {
      if (is.null(task_environment$prew)) {
        message("Using default VI where intervals are sampled from rgamma with shape parameters 1.5 qnd 3")
        task_environment$prew <- list(1.5, 3)
      }
      
      # get new random numbers
      task_environment <- setup_task_environment(model = task_environment$model, 
                                                 prew = task_environment$prew, 
                                                 n_trials = task_environment$n_trials,
                                                 task_environment$trial_ms,
                                                 task_environment$bin_ms,
                                                 schedule = "VI")

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
  } else{
    
    for (i in 1:n) {
      #regenerate outcomes matrix and GRWs
      if (is.null(task_environment$prew)) {
        message("Using default GRW task environment with initial p_rew = .5 and SD = .08")
        task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))
      }
      
      # get new random numbers for each replication dataset so that outcomes vary from one to the next
      task_environment$rand_p_which <-   with(task_environment, array(sample.int(n=n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
      task_environment$rand_p_respond <- with(task_environment, array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
      task_environment$rand_p_reward <-  with(task_environment, array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps)))
      
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
}


# function to convert simulated data to a format that matches Stan or VBA model
# wrapper around repeat_forward simulation that converts to same format as empirical data
sim_data_for_stan <- function(params, task_environment, n=100) {
  results <- repeat_forward_simulation(params, task_environment, n)
  subj_data <- results$all_df
  bin_boundaries <- seq(0, task_environment$trial_ms, by=task_environment$bin_ms)
  timestep_vector <- sort(unique(subj_data$timestep))
  time_vec <- seq(0, task_environment$trial_ms, by=task_environment$bin_ms)
  bin_centers <- time_vec[-1] - task_environment$bin_ms/2
  time_levels <- Hmisc::cut2(bin_centers, time_vec)
  
  subj_data <- subj_data %>% 
    dplyr::rename(instrial=trial, id=replication, key=choice, nreward=reward, npress=response) %>%
    mutate(bincenter = plyr::mapvalues(timestep, from=timestep_vector, to=bin_centers),
           binmedian = if_else(npress > 0, bincenter, NA_real_),
           latency_bin = factor(subj_data$timestep, 
                                levels=timestep_vector,
                                labels=time_levels)
    ) %>%
    group_by(id, instrial) %>% do({
      df <- . #for some reason, using . throughout results in returning the same data.frame repeatedly (loses grouping structure)
      df$curkey <- NA_integer_
      df$tdiff <- NA_real_
      df$switch <- 0
      
      curkey <- 0
      lastrt <- 0 #the start of the trial (time = 0) is treated as last rt. thus, tdiff accumulates up to first button press
      firstkey <- df$key[df$key > 0][1] #give the model the first press (i.e., give the model a bit of clairvoyance wrt the subject's first choice)
      curkey <- firstkey
      for (i in 1:nrow(df)) {
        if (df$key[i] != 0 && df$key[i] != curkey ) {
          curkey <- df$key[i]
          df$switch[i] <- 1
        }
        
        if (!is.na(df$binmedian[i])) {
          rt_k <- lastrt
          lastrt <- df$binmedian[i] #now update the running tracker of last response
          tau <- lastrt #use the last rt versus this rt as the tdiff in a response bin
        } else {
          tau <- df$bincenter[i]
          rt_k <- lastrt
        }
        
        df$curkey[i] <- curkey
        df$tdiff[i] <- tau - rt_k
      }
      
      df
    }) %>% ungroup() %>%
    select(id, instrial, latency_bin, key, binmedian, npress, nreward, switch, bincenter, curkey, tdiff) 
  
  return(subj_data)
}

# use method of moments to approximate gamma rate and shape based on mean and sd
# mu is mean of desired mean, sigma is standard deviation
gamma_params_from_moments <- function(mu, sigma) {
  checkmate::assert_number(mu, lower=0)
  checkmate::assert_number(sigma, lower = 0)

  s2 <- sigma^2
  scale_hat <- s2 / mu
  rate_hat <- 1 / scale_hat
  shape_hat <- mu / scale_hat
  
  return(c(shape = shape_hat, rate = rate_hat, scale = scale_hat))
}

rgamma_moments <- function(n, mean, sd) {
  moments <- gamma_params_from_moments(mean, sd)
  rgamma(n, shape = moments["shape"], scale = moments["scale"])
}

# function to simulate free operant behavior in SPOTT for multiple subjects, allowing for
# between-subject variation in the sample parameter distribution
sim_spott_free_operant_group <- function(
  nsubjects=50, parameters=list(), task_environment,
  quiet=FALSE, seed=1050, ...) {
  
  pacman::p_load(truncnorm)
  
  # removing parameter defaults for now to force clear specification upstream
  # param_defaults <- list(
  #   model="value2pl",
  #   alpha=expression(rtruncnorm(nsubjects, a=0.01, b=0.99, mean=0.2, sd=0.2)),
  #   gamma=expression(rgamma(nsubjects, shape=3, rate=1)),
  #   nu=expression(rnorm(nsubjects, mean=0, sd=0)), #deprecated parameter
  #   # beta=expression(rgamma(nsubjects, shape=4, rate=1/100)), #motor recovery
  #   beta=expression(rgamma(nsubjects, shape=50, rate=1)), #motor recovery
  #   omega=expression(rnorm(nsubjects, mean=1, sd=2)), #switch omega/stickiness
  #   kappa=expression(rgamma(nsubjects, shape=3, rate=1)) #(inverse) temperature on value-guided component of choice
  # )
  
  # #fill in defaults for any parameters not passed in
  # for (pname in names(param_defaults)) {
  #   if (is.null(parameters[[pname]])) {
  #     parameters[[pname]] <- param_defaults[[pname]]
  #   }
  # }
  
  set.seed(seed) #keep sims consistent
  
  # models that don't use beta (like time2pl) should fix beta to 1e-10 to make numerator 1.0
  if (is.null(parameters$beta)) {
    parameters$beta <- 1e-10
  }

  #simulate subject parameters
  # note that the use of parent.frame() should work in general if the list is setup in the environment
  # in which sim_spott_free_operant_group was called. We may have to adjust for other cases
  alpha_subj <- eval(parameters$alpha, envir = parent.frame())
  gamma_subj <- eval(parameters$gamma, envir = parent.frame())
  nu_subj <- eval(parameters$nu, envir = parent.frame())
  beta_subj <- eval(parameters$beta, envir = parent.frame())
  omega_subj <- eval(parameters$omega, envir = parent.frame())
  kappa_subj <- eval(parameters$kappa, envir = parent.frame())
  
  parmat <- cbind(alpha=alpha_subj, gamma=gamma_subj, nu=nu_subj, beta=beta_subj, omega=omega_subj, kappa=kappa_subj)
  
  dlist <- list()
  
  for (i in 1:nrow(parmat)) {
    subj_data <- sim_data_for_stan(parmat[i,], task_environment, n=1)
    subj_data$id <- i
    subj_data <- cbind(subj_data, as.list(parmat[i,]))
    dlist[[i]] <- subj_data
  }
  
  dlist <- dplyr::bind_rows(dlist)
  
  return(dlist)    
}

