#simulate performance of model in solving instrumental task
library(tidyverse)
source("code/ins_forward_simulation/ins_simulation_functions.R")
source("code/ins_forward_simulation/ins_learning_choice_rules.R")

# possible models
# 'value2pl': base model in which Qstar scales the probability of responding
#   free params:
#     - alpha: learning rate
#     - kappa: value-sensitivity in sticky softmax
#     - omega: choice-sensitivity (repeat) in sticky softmax
#     - beta: recovery rate for motor refractory period
#     - gamma: vigor sensitivity (slope of p_respond)
#     - nu: basal vigor (equilibrium point in p_respond)
# 'nonu': value2pl with nu = 0
# 'nobeta': value2pl with beta = 0


# 'time2pl': 
# 'notime'


get_params_list <- function()
  
  
  initial_params <- list(
    value=c(     alpha=0.1,   gamma=2,   nu=-1,   beta=200, omega=0, kappa=8), #make it more decisive/exploitative
    lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  omega=-10,    kappa=0.001),
    upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, omega=10,    kappa=10),
    par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, omega=1e-1, kappa=1e-1)
  )

# prew is a list of expressions that are evaluated inside the function to generate reward probabilities
# for each action. The length of prew determines the number of actions used in the simulations.

setup_task_environment <- function(prew=list(0.3, 0.3), n_trials=200, trial_ms=6000, bin_ms=50) {
  
  task_environment <- list(
    prew=lapply(prew, eval), # evaluate all prew expressions
    n_actions = length(prew),
    n_trials = n_trials,
    trial_ms = trial_ms, #6 seconds
    bin_ms = bin_ms, # ms
    n_timesteps = trial_ms/bin_ms,
  )
  
  # Initialize an array of random uniform numbers (trials x timesteps x actions) that are used in the
  # softmax choice rule to decide which action to take
  
  # Need a unique RNG seed for each trial x timestep to make sure that sample() call in sticky softmax is
  # reproducible across iteration in optimization.
  task_environment$rand_p_which <- array(sample.int(n=n_trials*n_timesteps), dim=c(n_trials, n_timesteps))
  task_environment$rand_p_respond <- array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps))
  task_environment$rand_p_reward <- array(runif(n_trials*n_timesteps), dim=c(n_trials, n_timesteps))

  
}


setup_task_environment(
  prew=list(
    expression(grwalk(n_trials, start=0.5, 0.08)),
    expression(grwalk(n_trials, start=0.5, 0.08)),
    expression(grwalk(n_trials, start=0.3, 0.1))
  ),
  n_trials
)

task_environment$n_actions

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_timesteps <- with(task_environment, trial_length/bin_size)
task_environment$outcomes <- with(task_environment, array(runif(n_timesteps*n_trials*3), dim=c(n_trials, n_timesteps, 3)))

# to generalize to > 2 actions, not clear if the pre-allocated outcome approach is right
# in sticky softmax, we want a probabilistic samples based on Q: 
#table(replicate(10000, sample(1:3, 1, prob=c(.5, .1, .4))))
task_environment$p_which <- array(runif(n_timesteps*n_trials*2), dim=c(n_trials, n_timesteps, 3)))

#run the model at these parameter settings.
# ins_results <- ins_wins(initial_params$value, fixed=NULL, task_environment, optimize=FALSE)

#get summary statistics
# sstats <- get_sim_stats(ins_results, task_environment)
# sum_df <- sstats$sum_df
# all_df <- sstats$all_df

#simulate data for stan fitting
#test_stan_sim <- sim_data_for_stan (initial_params$value, task_environment, n=100)

#simulate data using a population distribution on the parameters
stan_population <- sim_spott_free_operant_group(nsubjects=20, task_environment = task_environment,
                                                parameters=list( #use this list to provide expressions that are evaluated to generate group parameter distributions
                                                  kappa=expression(rnorm(nsubjects, 10, 1)),
                                                  beta=expression(rnorm(nsubjects, 1, .1)),
                                                  omega=expression(rnorm(nsubjects, 1.5, .3)),
                                                  gamma=expression(rnorm(nsubjects, 1, .3)),
                                                  nu=expression(rnorm(nsubjects, .12, .03)))
)

parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, beta, omega, kappa), mean)

out_dir <- "/Users/mnh5174/Data_Analysis/spott_modeling/data/vba_input_simulated_n80"
#out_dir <- "/Users/mnh5174/Data_Analysis/spott_modeling/data/vba_input_simulated_n5_minimal"
if (!dir.exists(out_dir)) { dir.create(out_dir) }
dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -beta, -omega, -kappa)

dsplit <- split(dsplit, dsplit$id)
sapply(1:length(dsplit), function(d) {
  id <- names(dsplit)[d]
  data <- dsplit[[d]]
  write.csv(data, file=file.path(out_dir, sprintf("%03s_spott_50.csv", id)), row.names=F)
})

#readr::write_csv(stan_population, path="data/stan_population_demo_trialdata.csv.gz")
write.csv(parmat, file=file.path(out_dir, "stan_population_demo_parameters.csv"), row.names=F)


## Older approach using switch cost approach

#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - cost: switch cost (should be relative to Qs)
# - kappa: inverse temperature on softmax choice (switch probability)

initial_params <- list(
  value=c(     alpha=0.1,   gamma=2,   nu=-1,   beta=200, omega=0.01, kappa=3), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  omega=0,    kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, omega=5,    kappa=5),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, omega=1e-1, kappa=1e-1)
)

task_environment <- list(
  #prew=c(0.5,0.3),
  n_trials=60,
  trial_length=6000, #6 seconds
  bin_size=50
)

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_timesteps <- with(task_environment, trial_length/bin_size)
task_environment$outcomes <- with(task_environment, array(runif(n_timesteps*n_trials*3), dim=c(n_trials, n_timesteps, 3)))

set.seed(100)
#run the model at these parameter settings.
ins_results <- ins_wins(c(alpha=0.1, gamma=8, nu=0, beta=200, omega=0, kappa=2), fixed=NULL, task_environment, optimize=FALSE)

#get summary statistics
sstats <- get_sim_stats(ins_results, task_environment)
sum_df <- sstats$sum_df
all_df <- sstats$all_df

toplot <- all_df %>% select(trial, timestep, Q_1, Q_2, choice, outcome) %>% gather(key=action, value=Q, Q_1, Q_2)
ggplot(toplot %>% filter(trial <= 10 & choice !=0), aes(x=timestep, y=Q, color=factor(action))) + 
  geom_line() + geom_point(aes(x=timestep, y=2-choice, color=outcome)) + facet_wrap(~trial)

#simulate data for stan fitting
test_stan_sim <- sim_data_for_stan (initial_params$value, task_environment, n=100)

#simulate data using a population distribution on the parameters
stan_population <- sim_spott_free_operant_group(nsubjects=50, task_environment = task_environment)

#if you want just the parameters
parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, beta, omega, kappa), mean)

readr::write_csv(stan_population, path="data/stan_population_demo_trialdata.csv.gz")
write.csv(parmat, file="data/stan_population_demo_parameters.csv", row.names=F)

#plot diagnosis
str(stan_population)


#historical archive

##July 2019: UPDATED example using sticky softmax instead of p_switch
#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - omega: temperature on sticky-guided part of softmax choice
# - kappa: temperature on value-guided part of softmax choice
