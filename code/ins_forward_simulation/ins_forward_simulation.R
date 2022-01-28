#simulate performance of model in solving instrumental task
library(tidyverse)
repo_dir <- "~/Data_Analysis/spott_modeling"
setwd(repo_dir)
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
# 'nobeta': value2pl with beta = 1 (divide by 1)
# 'norecovery': value2pl with beta = 1e-10, makes phi term (motor recovery) == 1.0
# 'time2pl': model in which tdiff scales the probability of responding
# 'time2pl_noQ': variant of time2pl where Qstar does not enter logistic
# 'notime'

# general settings for main model  
value2pl_params <- list(
  value=c(     alpha=0.1,   gamma=2,   nu=-1,   beta=200, omega=0, kappa=8), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  omega=-10,    kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, omega=10,    kappa=10),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, omega=1e-1, kappa=1e-1)
)

# general settings for time2pl model -- haven't spent time on this yet, but probably not grossly wrong (Sep2021)
# time2pl does not have a beta parameter (1.0 in numerator)
time2pl_params <- list(
  value=c(     alpha=0.1,   gamma=2,   nu=-1,  omega=0,   kappa=8), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   omega=-10,  kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   omega=10,   kappa=10),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, omega=1e-1, kappa=1e-1)
)

# prew is a list of expressions that are evaluated inside the function to generate reward probabilities
# for each action. The length of prew determines the number of actions used in the simulations.

task_environment <- setup_task_environment(
  prew=list(
    expression(grwalk(n_trials, start=0.7, 0.08)),
    expression(grwalk(n_trials, start=0.5, 0.08))#,
    #expression(grwalk(n_trials, start=0.3, 0.1)) #need to delete one line to have 2 options
  ),
  n_trials=200,
  model="time2pl" #note that the $model element can be edited and then passed back into a simulation function
)

#run the value2pl model at these parameter settings.
task_environment$model <- "value2pl"
ins_results_value2pl <- ins_wins(params=value2pl_params$value, fixed=NULL, task_environment, optimize=FALSE)

#run the time2pl model at these parameter settings.
task_environment$model <- "time2pl"
ins_results_time2pl <- ins_wins(params=time2pl_params$value, fixed=NULL, task_environment, optimize=FALSE)

#get summary statistics
sstats_value2pl <- get_sim_stats(ins_results_value2pl, task_environment)
sstats_time2pl <- get_sim_stats(ins_results_time2pl, task_environment)
# sum_df <- sstats$sum_df
# all_df <- sstats$all_df

#simulate data for stan fitting
#test_stan_sim <- sim_data_for_stan (value2pl_params$value, task_environment, n=100)

#simulate data using a population distribution on the parameters -- takes a few minutes
stan_population <- sim_spott_free_operant_group(
  nsubjects=20, task_environment = task_environment,
  parameters=list( 
    # use this list to provide expressions that are evaluated to generate group parameter distributions
    # note that there are internal defaults in the function that fill in unspecified parameters
    model="value2pl",
    kappa=expression(rnorm(nsubjects, 10, 1)),
    beta=expression(rnorm(nsubjects, 1, .1)),
    omega=expression(rnorm(nsubjects, 1.5, .3)),
    gamma=expression(rnorm(nsubjects, 1, .3)),
    nu=expression(rnorm(nsubjects, .12, .03)))
)

#distill subject parameters
parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, beta, omega, kappa), mean)

#name the path for simulation outputs
out_dir <- file.path(repo_dir, "data", "vba_input_simX")
if (!dir.exists(out_dir)) { dir.create(out_dir) }
dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -beta, -omega, -kappa)

dsplit <- split(dsplit, dsplit$id)
for (d in seq_along(dsplit)) {
  id <- names(dsplit)[d]
  data <- dsplit[[d]]
  readr::write_csv(data, file=file.path(out_dir, sprintf("%03s_spott_50.csv", id)))
}

#this writes the combined data for all subjects for multi-subject/hierarchical fitting
#readr::write_csv(stan_population, path=file.path(out_dir, "stan_population_demo_trialdata.csv.gz"))
write.csv(parmat, file=file.path(out_dir, "stan_population_demo_parameters.csv"), row.names=F)

#------
#historical archive

# toplot <- all_df %>% select(trial, timestep, Q_1, Q_2, choice, outcome) %>% gather(key=action, value=Q, Q_1, Q_2)
# ggplot(toplot %>% filter(trial <= 10 & choice !=0), aes(x=timestep, y=Q, color=factor(action))) + 
#   geom_line() + geom_point(aes(x=timestep, y=2-choice, color=outcome)) + facet_wrap(~trial)


##July 2019: UPDATED example using sticky softmax instead of p_switch
#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - omega: temperature on sticky-guided part of softmax choice
# - kappa: temperature on value-guided part of softmax choice


#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - cost: switch cost (should be relative to Qs)
# - kappa: inverse temperature on softmax choice (switch probability)
