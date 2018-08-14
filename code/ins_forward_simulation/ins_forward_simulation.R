#simulate performance of model in solving instrumental task
library(tidyverse)
source("code/ins_forward_simulation/ins_simulation_functions.R")
source("code/ins_forward_simulation/ins_learning_choice_rules.R")



#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - cost: switch cost (should be relative to Qs)
# - kappa: inverse temperature on softmax choice (switch probability)

initial_params <- list(
  value=c(alpha=0.1, gamma=2, nu=1, beta=100, cost=0.01, kappa=1),
  lower=c(alpha=0.001, gamma=0.1, nu=0, beta=25, cost=0, kappa=0.001),
  upper=c(alpha=0.99, gamma=100, nu=5, beta=500, cost=5, kappa=5),
  par_scale=c(alpha=1e-1, gamma=1e1, nu=1e0, beta=1e0, cost=1e-1, kappa=1e-1)
)

task_environment <- list(
  prew=c(0.5,0.3),
  n_trials=60,
  trial_length=6000,
  bin_size=60,
  time_resolution=30
)

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_bins <- with(task_environment, trial_length/bin_size)
task_environment$n_timesteps <- with(task_environment, trial_length/time_resolution + 1)
task_environment$outcomes <- with(task_environment, array(runif(n_bins*n_trials*3), dim=c(n_trials, n_timesteps, 3)))

#just check that the model does sensible things at priors
xx <- ins_wins(initial_params$value, task_environment, optimize=FALSE)


Q_df <- reshape2::melt(xx$Q_tba, varnames=c("trial", "timestep", "action")) %>% mutate(action=factor(action, levels=c(1,2), labels=c("Q_1", "Q_2")))
trial_plot <- ggplot(Q_df %>% filter(trial < 6), aes(x=timestep, y=value, color=action)) + geom_line() + facet_wrap(~trial, ncol=1)
choices_df <- reshape2::melt(xx$choices, varnames=c("trial", "timestep"), value.name="choice")
rewards_df <- reshape2::melt(xx$rewards, varnames=c("trial", "timestep"), value.name="outcome")

#all_df <- Q_df %>% spread(action, value) %>% full_join(choices_df) %>% full_join(rewards_df) %>% arrange(trial, timestep)
all_df <- reshape2::melt(xx$Q_tba, varnames=c("trial", "timestep", "action")) %>% full_join(choices_df) %>% full_join(rewards_df) %>% arrange(trial, timestep)


sumdf <- all_df %>% group_by(trial) %>% summarize(nresp=sum(choice > 0), avg_value=sum(Q_1, Q_2))
resp_plot <- ggplot(sumdf, aes(x=trial, y=nresp)) + geom_line()
cor.test(~nresp + avg_value, sumdf) #wow, highly correlated! So, higher average value within a trial correlates with responses on that trial
with(sumdf, ccf(nresp, avg_value))
ggplot(sumdf %>% gather(key="variable", value="value", nresp, avg_value), aes(x=trial, y=value, color=variable)) + geom_line()


ggplot(all_df %>% filter(trial < 6), aes(x=timestep, y=value, color=action)) + geom_line() + facet_wrap(~trial, ncol=1)


library(cowplot)
plot_grid(trial_plot, resp_plot, align="h", ncol=1)

##Now switch over to parameter optimization in this environment
optpars <- simulate_ins_performance(initial_params = initial_params, task_environment = task_environment, optimizer = "optim")

xx <- ins_wins(optpars$par, task_environment = task_environment, optimize=FALSE)
sum(xx$rewards, na.rm=TRUE) == -1*optpars$value

