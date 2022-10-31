repo_dir <- "/Users/maruofan/Documents/GitHub/spott_modeling/code/ins_forward_simulation"
#repo_dir <- "/Users/ruofanma/Documents/GitHub/spott_modeling/code/ins_forward_simulation" #laptop
setwd(repo_dir)


source("ins_simulation_functions_vi.R")
source("ins_learning_choice_rules.R")

task_environment <- setup_task_environment(
  prew = list(150, 300), #VI: use this as the input to rate in rgamma
  n_trials = 1,
  trial_ms=200*6000,
  model = "exp", #model # note that the $model element can be edited and then passed back into a simulation function
  schedule = "VI"
)

params <- c(alpha=0.1259690, gamma=3.2626238, nu=0.5724897, omega=3.4277531, kappa = 2.1928352)
xx <- repeat_forward_simulation(params, task_environment, n=20) #default n is 100 replications
res <- xx$sum_df
# res_combined <- bind_rows(res)

save(res, file ="/Users/maruofan/Documents/GitHub/spott_modeling/data/Testing_VI_sim.RData")

# VR: 
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)
