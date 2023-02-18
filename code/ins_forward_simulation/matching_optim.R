# This script finds an the combination of exp parameters so that 
# "a" calculated from matching is the closest to 0.8

library(dplyr)
library(ggplot2)

repo_dir <- "~/Documents/GitHub/spott_modeling/code/ins_forward_simulation"
setwd(repo_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")


# Global variables --------------------------------------------------------
a_target = 0.8

task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)

# Cost function -----------------------------------------------------------

# fn for optim()
# fn is the cost function: A function to be minimized (or maximized), with first 
# argument the vector of parameters over which minimization is to take place. 
# It should return a scalar result.

SimFit_a <- function(params, task_environment, a=a_target){
  # Repplicate simulation 20 times
  # Each replication has 100 trials
  xx <- repeat_forward_simulation(params, task_environment, n=20) #default n is 100 replications
  res <- xx$sum_df
  res_combined <- bind_rows(res)
  
  # Regression: log_n1_n2 ~ log(b) + a*log_p1_p2
  # Get estimates of a and b; there should be 20 estimates, one from each replication
  log_lm <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(replication) %>% do({
    df <- .
    df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
    m <- lm(log_n1_n2 ~ log_p1_p2, df)
    broom::tidy(m)
  })
  
  log_lm <- log_lm %>% ungroup() %>% pivot_wider(names_from = term, values_from = estimate) %>% rename(a = log_p1_p2, b="(Intercept)") %>% filter(is.na(b))
  
  # Calculate cost as sum of squared difference between estimated a and target a
  
  cost <- sum((log_lm["a"]-a_target)^2)
  print(cost)
  #if (is.infinite(cost) || is.na(cost)) {browser()}
  if (is.infinite(cost) || is.na(cost)) {
    cost <- 1e8
  }
  return(cost)
}
  

# Optimizer ---------------------------------------------------------------

# initial_params <- c(alpha=0.1, gamma=2, nu=0.5, omega=0, kappa = 2) # taken from previous simulations
# PANDAA_params <- c(alpha=0.1259690, gamma=3.2626238, nu=0.5724897, omega=3.4277531, kappa = 2.1928352) # recovered from PANDAA using VBA
starting_vals <- c(alpha=0.1, gamma=2, nu=0.5, omega=0.5, kappa = 2)

lower_bounds <- c(alpha=0.05, gamma=0.05, nu=0.2, omega=0.5, kappa=1)
upper_bounds <- c(alpha=0.9, gamma=10, nu=2.5, omega=5, kappa=6)
  
relativeScale <- c(alpha=0.1, gamma=0.5, nu=0.1, omega=0.5, kappa=1)

# Need to change after writing the input function to fn; 
# lower and upper are needed for the "L-BFGS-B" method
# For the "SANN" method it specifies a function to generate a new candidate point. If it is NULL a default Gaussian Markov kernel is used.
# control: parscale is a vector of scaling values for the parameters. Optimization 
#         is performed on par/parscale and these should be comparable in the sense 
#         that a unit change in any element produces about a unit change in the scaled value. 

optimizedParams <- optim(starting_vals, fn=SimFit_a, method="L-BFGS-B", 
                        lower=lower_bounds, upper=upper_bounds,
                        control=list(parscale=relativeScale),
                        task_environment = task_environment, a=a_target)


# Getting multiple optim results ------------------------------------------
options(error = recover) 

optimizedParams <- starting_vals
for (i in 1:10){
  new_optimizedParams <- optim(starting_vals, fn=SimFit_a, method="L-BFGS-B", 
                 lower=lower_bounds, upper=upper_bounds,
                 control=list(parscale=relativeScale),
                 task_environment = task_environment, a=a_target)
  optimizedParams <- rbind(optimizedParams, t(new_optimizedParams$par))
}

