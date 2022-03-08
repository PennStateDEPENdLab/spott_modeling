library(doFuture)
library(doRNG)
library(foreach)
library(future.batchtools)
library(iterators)
library(tidyverse)

repo_dir <- "/proj/mnhallqlab/projects/spott_modeling"
code_dir <- file.path(repo_dir, "code", "ins_forward_simulation")
out_dir <- file.path(repo_dir, "par_sim_exp/fix_omega_kappa")
nsubjects <- 50
model <- "exp"

if (!dir.exists(file.path(repo_dir, "par_sim_exp"))) {
  dir.create(file.path(repo_dir, "par_sim_exp"))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

setwd(code_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

# learning rate (alpha) follows rtruncnorm
# vigor sensitivity (gamma) follows gamma
# basal vigor (nu) follows normal
# motor recovery (beta) is unused in exp [EXAMINE]
# choice stickiness (omega) follows normal
# inverse temperature (kappa) follows gamma

# means of gamma
gvals <- 1.1^(seq(from=-30, to=25, length.out=20)) # prioritize low values. spans .057 -- 10.83

# means of kappa
kvals <- c(1, 4, 8) #1.14^(seq(-10, 20, 2)) # prioritize low values. spans .27 -- 13.74

sim_grid <- expand.grid(
  model = model,
  alpha_mean = seq(0.1, 0.9, by = 0.1), # increment 0.1 <- 0.01
  alpha_sd = 0.1, # fixed for now
  alpha_min = 0.02, # set constraints on learning rates
  alpha_max = 0.98,
  gamma_mean = gvals,
  nu_mean = seq(-5, 5, by = 1),
  nu_sd = 1,
  omega_mean = 0, #seq(-5, 5, by = 1), # increment by 1
  omega_sd = 1,
  kappa_mean = kvals
)

sim_grid$gamma_sd <- sqrt(gvals) # make b/w variation proportionate to mean, don't cross with gamma_mean in design
sim_grid$kappa_sd <- sqrt(kvals) # make b/w variation proportionate to mean, don't cross with kappa_mean in design
sim_grid$cond_id <- 1:nrow(sim_grid)

# subset for testing
# sim_grid <- sim_grid[c(11:15, 1011:1015, 10011:10015, 100011:100015),]
# sim_grid <- sim_grid[c(16:20, 1016:1020, 10016:10020, 100016:100020),]
sim_grid <- sim_grid[c(11:12),]

# sets how many items from the dataframe are sent to each parallel execution of the loop
# if you set it too low (e.g. 3), you'll get a *lot* of separate, brief jobs on the scheduler,
# which can delay completion of the queue or lead the scheduler to reject your later jobs.
# If you set it too high (100), then the total compute time may be high because simulations 
# are completed serially within each chunk (so 100 iterations would have to run per job)
chunk_size <- 10
registerDoFuture() # tell dopar to use future compute mechanism

# future set up: for using slurm as the backend of future
# https://tdhock.github.io/blog/2019/future-batchtools/
future::plan(
  future.batchtools::batchtools_slurm,
  template = "slurm-simple",
  resources = list(
    walltime = 10 * 60 * chunk_size, # 10 minutes per condition
    memory = 2000, # 2 GB
    ncpus = 1, # just needs one CPU within each chunk
    chunks.as.arrayjobs = FALSE
  )
)

#future.debug = TRUE

# example environment with 2 actions that start at .7 and .3 reward probability, with .08 step size
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 50,
  model = model # note that the $model element can be edited and then passed back into a simulation function
)

#registerDoSEQ() # uncomment for in-session debugging

res <- foreach(
  cond = iter(sim_grid, by = "row"), .options.future = list(chunk.size = chunk_size),
  .packages = c("readr", "dplyr", "truncnorm"), 
  .export = c("rgamma_moments", "gamma_params_from_moments") # not picked up automatically by code analyzer inside expression
) %dorng% {
  these_params <- list(
    model = "exp",
    alpha = expression(rtruncnorm(nsubjects, a=cond$alpha_min, b=cond$alpha_max, mean=cond$alpha_mean, sd=cond$alpha_sd)),
    gamma = expression(rgamma_moments(nsubjects, mean = cond$gamma_mean, sd = cond$gamma_sd)),
    nu = expression(rnorm(nsubjects, mean = cond$nu_mean, sd = cond$nu_sd)),
    omega = expression(rnorm(nsubjects, mean = cond$omega_mean, cond$omega_sd)), # switch omega/stickiness
    kappa = expression(rgamma_moments(nsubjects, mean = cond$kappa_mean, sd = cond$kappa_sd)) # (inverse) temperature on value-guided component of choice
  )

  # setup output folder for this sim
  cond_out_dir <- file.path(out_dir, sprintf("cond%06d", cond$cond_id))
  if (!dir.exists(cond_out_dir)) {
    dir.create(cond_out_dir)
  } else {
    # may want to support a global overwrite = TRUE/FALSE setting
    cat(sprintf("Sim directory %s already exists. Skipping it for now.\n", cond_out_dir))
    return(invisible(NULL)) # drop out before we do anything
  }

  # simulate data using a population distribution on the parameters -- takes a few minutes
  stan_population <- sim_spott_free_operant_group(
    nsubjects = nsubjects, task_environment = task_environment, parameters = these_params
  )

  # distill subject parameters
  parmat <- stan_population %>%
    group_by(id) %>%
    summarize_at(vars(alpha, gamma, nu, beta, omega, kappa), mean)

  # write groundtruth parameters for each subject
  write.csv(parmat, file = file.path(cond_out_dir, paste0("stan_population_parameters_", cond$cond_id, ".csv")), row.names = F)

  dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -beta, -omega, -kappa)

  dsplit <- split(dsplit, dsplit$id)
  for (d in seq_along(dsplit)) {
    id <- names(dsplit)[d]
    data <- dsplit[[d]]
    readr::write_csv(data, file = file.path(cond_out_dir, sprintf("%03d_spott_data.csv", as.integer(id))))
  }

  # this writes the combined data for all subjects for multi-subject/hierarchical fitting
  readr::write_csv(stan_population, file = file.path(cond_out_dir, "stan_population_trialdata.csv.gz"))
  
  return(parmat)

}


#######
#######
# RUOFAN CODE

# future.apply::future_apply(sim_grid_tr, c(1), future.chunk.size=chunk_size,  function(sim_grid_row){
#   print(sim_grid_row)
#   })


# future.apply::future_apply(sim_grid_tr, c(1), future.chunk.size=chunk_size,  function(sim_grid_row){
#   print(sim_grid_row)
#   these_params <- list(
#     # alpha=expression(rnorm(nsubjects, mean=sim_grid_row$alpha, sd=0.2)), #rtruncnorm --> rnorm
#     #alpha=expression(rtruncnorm(nsubjects, a=0.01, b=0.99, mean=sim_grid_row$alpha, sd=0.2)),
#     gamma=expression(rgamma(nsubjects, shape=sim_grid_row$gamma, rate=1)),
#     nu=expression(rnorm(nsubjects, mean=sim_grid_row$nu, sd=0)), #deprecated parameter
#     omega=expression(rnorm(nsubjects, mean=sim_grid_row$omega, sd=2)), #switch omega/stickiness
#     kappa=expression(rgamma(nsubjects, shape=sim_grid_row$kappa, rate=1)) #(inverse) temperature on value-guided component of choice
#   )
# 
#   #simulate data using a population distribution on the parameters -- takes a few minutes
#   stan_population <- sim_spott_free_operant_group(
#     nsubjects=20, task_environment = task_environment,
#     these_params)
# 
#   #distill subject parameters
#   parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, omega, kappa), mean)
# 
#   out_dir <- "/proj/mnhallqlab/users/ruofan/ins_forward_simulation"
#   row_index <- rownames(sim_grid_row)
# 
#   #this writes the combined data for all subjects for multi-subject/hierarchical fitting
#   write.csv(parmat, file=file.path(out_dir, paste0("stan_population_demo_parameters_", row_index)), row.names=F)
# 
# })


# loop set up to iterate over the dataframe in parallel
# each interation handles "chunk.size" number of elements
# %dorng% sets the loop to run in parallel
# res <- foreach(
#   df = iter(sim_grid), .packages=c("brms", "tidybayes", "emmeans", "glue", "dplyr", "ggdist", "data.table"),
#   .options.future = list(chunk.size = chunk_size)) %dorng% {
    
#     print(head(df))
    
#     }



#########
#########


# # par_list contains values of alpha, gamma, nu, omega, kappa; e.g., par_list <- sim_grid[1,]
# # par_list is one element in sim_grid, so the function run_sim goes through one iteration in the original for loop
# run_sim <- function(par_list){
#   these_params <- list(
#     alpha=expression(rtruncnorm(nsubjects, a=0.01, b=0.99, mean=par_list$alpha, sd=0.2)),
#     gamma=expression(rgamma(nsubjects, shape=par_list$gamma, rate=1)),
#     nu=expression(rnorm(nsubjects, mean=par_list$nu, sd=0)), #deprecated parameter
#     omega=expression(rnorm(nsubjects, mean=par_list$omega, sd=2)), #switch omega/stickiness
#     kappa=expression(rgamma(nsubjects, shape=par_list$kappa, rate=1)) #(inverse) temperature on value-guided component of choice
#   )
# 
#   #simulate data using a population distribution on the parameters -- takes a few minutes
#   stan_population <- sim_spott_free_operant_group(
#     nsubjects=20, task_environment = task_environment,
#     these_params)
# 
#   #distill subject parameters
#   parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, omega, kappa), mean)
# 
#   #name the path for simulation outputs
#   out_dir <- "/proj/mnhallqlab/users/ruofan/ins_forward_simulation"
#   # out_dir <- file.path(repo_dir, "data", paste0("vba_input_simX", as.character(i))) # Now how do I name the new document?
#   # if (!dir.exists(out_dir)) { dir.create(out_dir) }
#   # dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -omega, -kappa)
#   # 
#   # dsplit <- split(dsplit, dsplit$id)
#   # for (d in seq_along(dsplit)) {
#   #   id <- names(dsplit)[d]
#   #   data <- dsplit[[d]]
#   #   readr::write_csv(data, file=file.path(out_dir, sprintf("%03s_spott_20.csv", id)))
#   # }
# 
#   #this writes the combined data for all subjects for multi-subject/hierarchical fitting
#   write.csv(parmat, file=file.path(out_dir, paste0("stan_population_demo_parameters_")), row.names=F)
# }