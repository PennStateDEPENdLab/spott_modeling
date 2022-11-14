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
xx_4 <- repeat_forward_simulation(params, task_environment, n=20) #default n is 100 replications
res_4 <- xx_4$sum_df
# res_combined <- bind_rows(res)

res_15_30 <- res
res_15_30_2 <- res_2
res_1.5_3 <- res_3
save(res_15_30, res_15_30_2, res_1.5_3, file ="/Users/maruofan/Documents/GitHub/spott_modeling/data/Testing_VI_sim.RData")

load("/Users/maruofan/Documents/GitHub/spott_modeling/data/Testing_VI_sim.RData")

summary(lm(log_n1_n2~log_p1_p2, res_3))

# Pick one of the data set generated using prew = list(15, 30), because that now gives "a" close to 0.8
# pending investigation in how the parameters (which go into rgamma()) affect a
all_df <- xx_2$all_df

# Get the programmed time stamps for rewards, for both choices.
# Then calculate programmed_interval as the time interval that each reward has to wait, for both choices
df <- data.frame(task_environment$rand_p_reward, time_programmed = seq(0.05,task_environment$n_timesteps,0.05)*1000)
df_1 <- df %>% filter(X1 == 1) %>% mutate(programmed_interval = (time_programmed - lag(time_programmed)))
df_2 <- df %>% filter(X2 == 1) %>% mutate(programmed_interval = (time_programmed - lag(time_programmed)))

# For each choice, get the actual rewarded times (how long has it been since the last reward)
# and put the programmed intervals on the side 
choice_1 <- all_df %>% filter(choice ==1 & reward ==1) %>% group_by(replication) %>% mutate(reward_interval = (timestep - lag(timestep))*50) %>% mutate(programmed_interval = head(df_1$programmed_interval, tally(cur_data())))
hist(choice_1$reward_interval)
choice_2 <- all_df %>% filter(choice ==2 & reward ==1) %>% group_by(replication) %>% mutate(reward_interval = (timestep - lag(timestep))*50) %>% mutate(programmed_interval = head(df_2$programmed_interval, tally(cur_data())))
hist(choice_2$reward_interval)

cor(na.omit(choice_1$reward_interval), na.omit(choice_1$programmed_interval))

hist(choice_1$reward_interval)

hist(rgamma(200*6000/50, 15, 4))




# VR: 
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)
