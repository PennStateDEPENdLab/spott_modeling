library(dplyr)
library(ggplot2)

# Using parameter values recovered from PANDAA as initial parameter values
setwd("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/outputs/vba_out/ffx/pandaa_vba_input/exp/")
PANDAAparas <- read.csv("pandaa_vba_input_exp_ffx_global_statistics.csv")
initial_params <- c(alpha=mean(PANDAAparas$alpha_transformed), # 0.1259690
                    gamma=mean(PANDAAparas$gamma_transformed), # 3.2626238
                    nu=mean(PANDAAparas$nu_transformed), # 0.5724897
                    omega=mean(PANDAAparas$omega_transformed), # 3.4277531
                    kappa = mean(PANDAAparas$kappa_transformed)) # 2.1928352

repo_dir <- "~/Documents/GitHub/spott_modeling/code/ins_forward_simulation"
setwd(repo_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

# example environment with 2 actions that start at .7 and .3 reward probability, with .08 step size
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)

xx <- repeat_forward_simulation(initial_params, task_environment) 
sdf <- xx$sum_df

sdf_filtered <- sdf %>% filter(log_p1_p2 > -1 & log_n1_n2 < Inf & log_n1_n2 > -Inf)

cor(sdf_filtered$log_n1_n2,sdf_filtered$log_p1_p2)
summary(lm(log_n1_n2 ~ log_p1_p2, sdf_filtered))

