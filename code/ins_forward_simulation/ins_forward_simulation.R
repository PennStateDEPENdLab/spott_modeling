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
  value=c(     alpha=0.1,   gamma=2,   nu=1,   beta=200, cost=0.01, kappa=3), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  cost=0,    kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, cost=5,    kappa=5),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, cost=1e-1, kappa=1e-1)
)


#test function
vv <- grwalk(1000)


task_environment <- list(
  #prew=c(0.5,0.3),
  n_trials=60,
  trial_length=6000, #6 seconds
  bin_size=60,
  time_resolution=30
)

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_bins <- with(task_environment, trial_length/bin_size)
task_environment$n_timesteps <- with(task_environment, trial_length/time_resolution + 1)
task_environment$outcomes <- with(task_environment, array(runif(n_bins*n_trials*3), dim=c(n_trials, n_timesteps, 3)))


### TEST AT PRIORS
#just check that the model does sensible things at priors
ins_results <- ins_wins(initial_params$value, fixed=NULL, task_environment, optimize=FALSE)

Q_df <- reshape2::melt(ins_results$Q_tba, varnames=c("trial", "timestep", "action")) %>% 
  mutate(action=factor(action, levels=c(1,2), labels=c("Q_1", "Q_2")))
trial_plot <- ggplot(Q_df %>% filter(trial < 6), aes(x=timestep, y=value, color=action)) + geom_line() + facet_wrap(~trial, ncol=1)
choices_df <- reshape2::melt(ins_results$choices, varnames=c("trial", "timestep"), value.name="choice")
rewards_df <- reshape2::melt(ins_results$rewards, varnames=c("trial", "timestep"), value.name="outcome")

#Q_tba is Q values trials x timesteps x actions
#choices_df: action 0 means no action, whereas 1 or 2 denote their respective actions

#this has columnns for Q_1 and Q_2
library(zoo)
# rollwin <- 20
myrollfunc <- function(vec, win=20) {
  require(zoo)
  rollmean(vec, win, align="right", fill=NA)
}
#don't compute Q ratio if either is 0
all_df <- Q_df %>% spread(action, value) %>% full_join(choices_df) %>% full_join(rewards_df) %>% 
  arrange(trial, timestep) %>% 
  mutate(outcome=factor(outcome, levels=c(0,1), labels=c("omission", "reward")),
         Q_sum=Q_1 + Q_2, 
         Q_ratio=if_else(Q_1 > 0 & Q_2 > 0, Q_1/Q_2, NA_real_),
         response=as.numeric(choice > 0),  #any response
         reward=if_else(outcome=="reward", 1, 0, missing=0)) %>% #recode 1/0 for reward/omission and make NAs (no response) 0s
  #group_by(trial) %>% #group by trial to get rolling mean to respect each trial
  mutate(response_rate=myrollfunc(response), #a bin is 30 milliseconds, so this is responses per 450ms
         reward_rate=myrollfunc(reward),
         Q_1_roll=myrollfunc(Q_1), Q_2_roll=myrollfunc(Q_2),
         n_1=myrollfunc(choice==1),
         n_2=myrollfunc(choice==2),
         n_1_pref=if_else(n_1 > 0 & n_2 > 0, n_1/n_2, NA_real_),
         Q_sum_roll=myrollfunc(Q_sum), Q_ratio_roll=myrollfunc(Q_ratio)) %>% 
  ungroup() %>%
  mutate(resp_Q_y = case_when(choice==1 ~ Q_1_roll, choice==2 ~ Q_2_roll, TRUE ~ NA_real_))

#Q_ratio_roll, 
#response_rate, reward_rate
all_df_gather <- all_df %>% gather(key=rate_type, value=rate, Q_sum_roll, Q_1_roll, Q_2_roll, n_1, n_2) %>% filter(trial < 6)

ggplot(all_df_gather, aes(x=timestep, y=rate, color=rate_type)) + geom_line() + facet_wrap(~trial) +
  geom_point(data=filter(all_df_gather, response == 1), aes(x=timestep, y=resp_Q_y, shape=outcome), color="black")


cor.test(all_df$n_1, all_df$Q_1_roll)
cor.test(all_df$n_2, all_df$Q_2_roll)
plot(all_df$n_2, all_df$Q_2_roll)

cor.test(all_df$Q_ratio_roll, all_df$n_1)
cor.test(all_df$Q_ratio_roll, all_df$n_2)
cor.test(all_df$Q_ratio_roll, all_df$n_1_pref)
plot(all_df$Q_ratio_roll, all_df$n_1_pref)

cor.test(all_df$n_2, all_df$Q_2_roll)

cor.test(all_df$response_rate, all_df$reward_rate)
cor.test(all_df$response_rate, all_df$Q_sum)
cor.test(all_df$reward_rate, all_df$Q_sum)
ccf(na.omit(all_df$response_rate), na.omit(all_df$reward_rate))

summary(lm(response_rate ~ Q_1 + Q_2, all_df))

cor.test(all_df$response_rate, all_df$Q_sum_roll)

summary(lm(response_rate ~ Q_1_roll + Q_2_roll, all_df))



#Look at bidirectional relationships between response rate and reward rate
xx <- vars::VAR(na.omit(all_df[,c("response_rate", "reward_rate")]), p=5)
summary(xx)

#scale Qs in terms of rewards per timestep

#proportion matching


sumdf <- all_df %>% group_by(trial) %>% 
  summarize(nresp=sum(choice > 0), avg_value=sum(Q_1, Q_2),
            n_1=sum(choice==1), n_2=sum(choice==2),
            Q_1=sum(Q_1), Q_2=sum(Q_2),
            Q1_Q2=sum(Q_1) / sum(Q_2),
            Q1_m_Q2=sum(Q_1) - sum(Q_2),
            n1_n2=sum(choice==1) / sum(choice==2),
            n1_m_n2=sum(choice==1) - sum(choice==2))
resp_plot <- ggplot(sumdf, aes(x=trial, y=nresp)) + geom_line()
resp_plot

#merge back reward schedule

#orthogonalize overall vigor versus proportion of 1 vs. 2?
#so in richer environments, we see faster responding, but the ratios are preserved

prew_df <- data.frame(task_environment$prew) %>% setNames(c("p_1", "p_2")) %>% mutate(trial=1:n()) %>%
  mutate(p1_p2=p_1/p_2)

sumdf <- sumdf %>% left_join(prew_df)

cor.test(~nresp + avg_value, sumdf) #wow, highly correlated! So, higher average value within a trial correlates with responses on that trial
ccf_resp_value <- with(sumdf, ccf(nresp, avg_value))
ggplot(sumdf %>% gather(key="variable", value="value", nresp, avg_value) %>% group_by(variable) %>% mutate(value=scale(value)), aes(x=trial, y=value, color=variable)) + geom_line()

ggplot(sumdf, aes(x=avg_value, y=nresp)) + geom_point()

cor.test(~n_1 + Q_1, sumdf) #strong correlation of Q_1 value with number of times it is chosen
cor.test(~n_2 + Q_2, sumdf) #similar for Q_2

#specificity
summary(lm(n_1 ~ Q_1 + Q_2, sumdf))
summary(lm(n_2 ~ Q_1 + Q_2, sumdf))

lm.beta(lm(n_1 ~ Q_1 + Q_2, sumdf))
lm.beta(lm(n_2 ~ Q_1 + Q_2, sumdf))

summary(lm(nresp ~ p_1 + p_2, sumdf))
lm.beta(lm(nresp ~ p_1 + p_2, sumdf))
summary(lm(nresp ~ Q_1 * Q_2, sumdf)) #vigor effect

#what about proportion matching?
cor.test(~n1_n2 + Q1_Q2, filter(sumdf, n1_n2 < 15)) #ratio
cor.test(~n1_m_n2 + Q1_m_Q2, sumdf) #difference

cor.test(~n1_n2 + p1_p2, filter(sumdf, n1_n2 < 15))

# ggplot(sumdf, aes(x=n1_n2, y=Q1_Q2)) + geom_point() + xlim(0,15) + stat_smooth()
# ggplot(sumdf, aes(x=n1_n2, y=p1_p2)) + geom_point() + xlim(0,15) + stat_smooth()

#log matching
sumdf <- sumdf %>% mutate(log_n1_n2 = log(n1_n2), log_p1_p2=log(p1_p2))
summary(lm(log_n1_n2 ~ log_p1_p2, filter(sumdf, log_p1_p2 > -1 & log_n1_n2 < 1.9)))

ggplot(filter(sumdf, log_p1_p2 > -1 & log_n1_n2 < 1.9), aes(x=log_n1_n2, y=log_p1_p2)) + geom_point() + stat_smooth(method="lm")

#if we want to plot q traces, we need to gather up Q_1 and Q_2
# all_df <- reshape2::melt(ins_results$Q_tba, varnames=c("trial", "timestep", "action")) %>% 
#   full_join(choices_df) %>% full_join(rewards_df) %>% arrange(trial, timestep)

for_plotting <- all_df %>% gather(key=action, value=value, Q_1, Q_2)
ggplot(for_plotting %>% filter(trial < 12 & trial > 6), aes(x=timestep, y=value, color=factor(action))) + geom_line() + facet_wrap(~trial, ncol=1)

#look at relative proportions of choices based on recent values






library(cowplot)
plot_grid(trial_plot, resp_plot, align="h", ncol=1)

##Now switch over to parameter optimization in the same environment
#when nu is greater than Qstar, then the p_response function becomes reversed...

pars <- split_free_fixed(initial_params, c(beta=NA, nu=0, cost=NA, gamma=25))


optpars <- simulate_ins_performance(initial_params = pars$free, fixed=pars$fixed, 
                                    task_environment = task_environment, optimizer = "nlminb")

ins_results <- ins_wins(optpars$par, fixed=pars$fixed, task_environment = task_environment, optimize=FALSE)
sum(ins_results$rewards, na.rm=TRUE) == -1*optpars$objective
sum(ins_results$rewards, na.rm=TRUE) == -1*optpars$value #optim


Q_df <- reshape2::melt(ins_results$Q_tba, varnames=c("trial", "timestep", "action")) %>% mutate(action=factor(action, levels=c(1,2), labels=c("Q_1", "Q_2")))
trial_plot <- ggplot(Q_df %>% filter(trial < 6), aes(x=timestep, y=value, color=action)) + geom_line() + facet_wrap(~trial, ncol=1)
choices_df <- reshape2::melt(ins_results$choices, varnames=c("trial", "timestep"), value.name="choice")
rewards_df <- reshape2::melt(ins_results$rewards, varnames=c("trial", "timestep"), value.name="outcome")

all_df <- Q_df %>% spread(action, value) %>% full_join(choices_df) %>% full_join(rewards_df) %>% arrange(trial, timestep)

sumdf <- all_df %>% group_by(trial) %>% summarize(nresp=sum(choice > 0), avg_value=sum(Q_1, Q_2))
resp_plot <- ggplot(sumdf, aes(x=trial, y=nresp)) + geom_line()
cor.test(~nresp + avg_value, sumdf) #wow, highly correlated! So, higher average value within a trial correlates with responses on that trial
ccf_resp_value <- with(sumdf, ccf(nresp, avg_value))
ggplot(sumdf %>% gather(key="variable", value="value", nresp, avg_value) %>% group_by(variable) %>% mutate(value=scale(value)), aes(x=trial, y=value, color=variable)) + geom_line()

