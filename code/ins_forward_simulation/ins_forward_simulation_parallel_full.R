library(doFuture)
library(doRNG)
library(foreach)
library(future.batchtools)
library(iterators)
library(tidyverse)

repo_dir <- "/proj/mnhallqlab/projects/spott_modeling"

code_dir <- "/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/ins_forward_simulation"
out_dir <- file.path(repo_dir, "outputs/par_sim_exp_full")
nsubjects <- 50
model <- "exp"

# if (!dir.exists(file.path(repo_dir, "par_sim_exp"))) {
#   dir.create(file.path(repo_dir, "par_sim_exp"))
# }

if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

setwd(code_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

# alpha <- seq(0.1, 0.8, 0.1)
# gamma <- 1.1^(seq(from=-16, to=15, length.out=10))
# nu <- seq(0, 2, 0.25)
# kappa <- seq(2, 6, 1)
# omega <- seq(0, 4, 0.5)

# learning rate (alpha) follows rtruncnorm
# vigor sensitivity (gamma) follows gamma
# basal vigor (nu) follows rtruncnorm
# motor recovery (beta) is unused in exp
# choice stickiness (omega) follows rtruncnorm
# inverse temperature (kappa) follows gamma

gvals <- 1.1^(seq(from=-16, to=15, length.out=10))
kvals <- seq(2, 6, 1)

sim_grid <- expand.grid(
  model = model,
  alpha_mean = seq(0.1, 0.8, by = 0.1),
  alpha_sd = 0.1, # fixed for now
  alpha_min = 0.02, # set constraints on learning rates
  alpha_max = 0.98,
  gamma_mean = gvals,
  nu_mean = seq(0, 2, 0.25), # Keeping nu positive
  nu_sd = 0.3,
  nu_min = 0,
  nu_max = 4,
  omega_mean = seq(0, 4, 0.5),
  omega_sd = 1,
  omega_min = 0,
  omega_max = 6,
  kappa_mean = kvals
)

sim_grid$gamma_sd <- sqrt(gvals) # make b/w variation proportionate to mean, don't cross with gamma_mean in design
sim_grid$kappa_sd <- sqrt(kvals) # make b/w variation proportionate to mean, don't cross with kappa_mean in design
sim_grid$cond_id <- 1:nrow(sim_grid)

# sim_grid <- sim_grid[9684,]

chunk_size <- 10
registerDoFuture()

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

task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 30,
  model = model # note that the $model element can be edited and then passed back into a simulation function
)

res <- foreach(
  cond = iter(sim_grid, by = "row"), .options.future = list(chunk.size = chunk_size),
  .packages = c("readr", "dplyr", "truncnorm"), 
  .export = c("rgamma_moments", "gamma_params_from_moments") # not picked up automatically by code analyzer inside expression
) %dorng% {
  these_params <- list(
    model = "exp",
    alpha = expression(rtruncnorm(nsubjects, a=cond$alpha_min, b=cond$alpha_max, mean=cond$alpha_mean, sd=cond$alpha_sd)),
    gamma = expression(rgamma_moments(nsubjects, mean = cond$gamma_mean, sd = cond$gamma_sd)),
    nu=expression(rtruncnorm(nsubjects,  a=cond$nu_min, b=cond$nu_max, mean=cond$nu_mean, sd=cond$nu_sd)), 
    omega = expression(rtruncnorm(nsubjects, a=cond$omega_min, b=cond$omega_max, mean = cond$omega_mean, cond$omega_sd)), # switch omega/stickiness
    kappa = expression(rgamma_moments(nsubjects, mean = cond$kappa_mean, sd = cond$kappa_sd)) # (inverse) temperature on value-guided component of choice
  )
  
  # setup output folder for this sim
  cond_out_dir <- file.path(out_dir, sprintf("cond%05d", cond$cond_id))
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
    summarize_at(vars(alpha, gamma, nu, omega, kappa), mean)
  
  # write ground truth parameters for each subject
  write.csv(parmat, file = file.path(cond_out_dir, paste0("stan_population_parameters_", cond$cond_id, ".csv")), row.names = F)
  
  dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -omega, -kappa)
  
  dsplit <- split(dsplit, dsplit$id)
  for (d in seq_along(dsplit)) {
    id <- names(dsplit)[d]
    data <- dsplit[[d]]
    readr::write_csv(data, file = file.path(cond_out_dir, sprintf("%02d_spott_data.csv", as.integer(id))))
  }
  
  # this writes the combined data for all subjects for multi-subject/hierarchical fitting
  readr::write_csv(stan_population, file = file.path(cond_out_dir, "stan_population_trialdata.csv.gz"))
  
  return(parmat)
  
}
  