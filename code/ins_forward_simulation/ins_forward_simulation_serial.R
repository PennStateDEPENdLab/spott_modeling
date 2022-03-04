rm(list=ls())

library(tidyverse)

repo_dir <- "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling"
setwd(repo_dir)
source("ins_forward_simulation/ins_simulation_functions.R")
source("ins_forward_simulation/ins_learning_choice_rules.R")

sim_grid <- expand.grid(alpha=seq(0.001, 0.9, by=0.1), # increment 0.1 <- 0.01
                        gamma=seq(0.1, 100, by = 3),  # increment 1 <- 0.1
                        nu=seq(-5, 5, by=1),
                        #beta=seq(25, 500, by=25), # increment 25 <- 1
                        omega=seq(-5, 5, by=1), # increment 1 <- 0.1
                        kappa=seq(0.001, 10, by=1)) #increment 1 <- 0.1

# for (i in seq_len(nrow(sim_grid))) {
#   stan_population <- sim_spott_free_operant_group(, nsubjects=20, task_environment = task_environment, sim_grid[i,])
#   do.call(sim_group_something, as.list(sim_grid[i,]))
# }

task_environment <- setup_task_environment(
  prew=list(
    expression(grwalk(n_trials, start=0.7, 0.08)),
    expression(grwalk(n_trials, start=0.5, 0.08))#,
    #expression(grwalk(n_trials, start=0.3, 0.1)) #need to delete one line to have 2 options
  ),
  n_trials=200,
  model="time2pl" #note that the $model element can be edited and then passed back into a simulation function
)

#run the time2pl model at these parameter settings.
task_environment$model <- "time2pl"

for (i in c(11:15, 1011:1015, 10011:10015, 100011:100015)){
  
  these_params <- list(
    alpha=expression(rtruncnorm(nsubjects, a=0.01, b=0.99, mean=sim_grid$alpha[i], sd=0.2)),
    gamma=expression(rgamma(nsubjects, shape=sim_grid$gamma[i], rate=1)),
    nu=expression(rnorm(nsubjects, mean=sim_grid$nu[i], sd=0)), #deprecated parameter
    # beta=expression(rgamma(nsubjects, shape=4, rate=1/100)), #motor recovery
    # beta=expression(rgamma(nsubjects, shape=sim_grid$beta[i], rate=1)), #motor recovery
    omega=expression(rnorm(nsubjects, mean=sim_grid$omega[i], sd=2)), #switch omega/stickiness
    kappa=expression(rgamma(nsubjects, shape=sim_grid$kappa[i], rate=1)) #(inverse) temperature on value-guided component of choice
  )
  
  #simulate data using a population distribution on the parameters -- takes a few minutes
  stan_population <- sim_spott_free_operant_group(
    nsubjects=20, task_environment = task_environment,
    these_params)
  
  #distill subject parameters
  parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, beta, omega, kappa), mean)
  
  #name the path for simulation outputs
  out_dir <- file.path(repo_dir, "data", paste0("vba_input_simX", as.character(i)))
  if (!dir.exists(out_dir)) { dir.create(out_dir) }
  dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -beta, -omega, -kappa)
  
  dsplit <- split(dsplit, dsplit$id)
  for (d in seq_along(dsplit)) {
    id <- names(dsplit)[d]
    data <- dsplit[[d]]
    readr::write_csv(data, file=file.path(out_dir, sprintf("%03s_spott_20.csv", id)))
  }
  
  #this writes the combined data for all subjects for multi-subject/hierarchical fitting
  write.csv(parmat, file=file.path(out_dir, "stan_population_demo_parameters.csv"), row.names=F)
}

