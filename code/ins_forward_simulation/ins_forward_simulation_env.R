
# repo_dir <- "/proj/mnhallqlab/projects/spott_modeling"
# code_dir <- "/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/ins_forward_simulation"
# out_dir <- file.path(repo_dir, "outputs/par_sim_exp_full")

# setwd(code_dir)
# source("ins_simulation_functions.R")
# source("ins_learning_choice_rules.R")

# Define parameters using the "middle values" or "optimal values" from recovery
# parameters <- 

nsubjects <- 50
model <- "exp"

# GRW
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 30,
  model = model
)

# Reversal (less volatile)
task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.7, 10), rep(0.3, 10), rep(0.7, 10)),
    c(rep(0.3, 10), rep(0.7, 10), rep(0.3, 10))
  ),
  n_trials = 30,
  model = model
)

# Reversal (more volatile)
task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.7, 3), rep(0.3, 5), rep(0.7, 5), rep(0.7, 3), rep(0.3, 4), rep(0.7, 6), rep(0.3, 4)),
    c(rep(0.3, 3), rep(0.7, 5), rep(0.3, 5), rep(0.3, 3), rep(0.7, 4), rep(0.3, 6), rep(0.7, 4))
  ),
  n_trials = 30,
  model = model
)

# Varying relative Q
task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.8, 30)),
    c(rep(0.2, 30))
  ),
  n_trials = 30,
  model = model
)

task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.7, 30)),
    c(rep(0.3, 30))
  ),
  n_trials = 30,
  model = model
)

task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.6, 30)),
    c(rep(0.4, 30))
  ),
  n_trials = 30,
  model = model
)

task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.5, 30)),
    c(rep(0.5, 30))
  ),
  n_trials = 30,
  model = model
)

# Varying total Q
task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.2, 30)),
    c(rep(0.4, 30))
  ),
  n_trials = 30,
  model = model
)

task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.3, 30)),
    c(rep(0.6, 30))
  ),
  n_trials = 30,
  model = model
)

task_environment <- setup_task_environment(
  prew = list(
    c(rep(0.4, 30)),
    c(rep(0.8, 30))
  ),
  n_trials = 30,
  model = model
)


