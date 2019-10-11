#simulate performance of model in solving instrumental task
library(tidyverse)
source("code/ins_forward_simulation/ins_simulation_functions.R")
source("code/ins_forward_simulation/ins_learning_choice_rules.R")


##July 2019: UPDATED example using sticky softmax instead of p_switch
#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - cost: temperature on sticky-guided part of softmax choice
# - kappa: temperature on value-guided part of softmax choice

initial_params <- list(
  value=c(     alpha=0.1,   gamma=2,   nu=-1,   beta=200, cost=0, kappa=8), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  cost=-10,    kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, cost=10,    kappa=10),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, cost=1e-1, kappa=1e-1)
)

task_environment <- list(
  #prew=c(0.8,0.2), #for constant probs
  n_trials=60,
  trial_length=6000, #6 seconds
  bin_size=50,
  sticky_softmax=TRUE
)

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_timesteps <- with(task_environment, trial_length/bin_size)
task_environment$outcomes <- with(task_environment, array(runif(n_timesteps*n_trials*3), dim=c(n_trials, n_timesteps, 3)))

#run the model at these parameter settings.
# ins_results <- ins_wins(initial_params$value, fixed=NULL, task_environment, optimize=FALSE)

#get summary statistics
# sstats <- get_sim_stats(ins_results, task_environment)
# sum_df <- sstats$sum_df
# all_df <- sstats$all_df

#simulate data for stan fitting
#test_stan_sim <- sim_data_for_stan (initial_params$value, task_environment, n=100)

#simulate data using a population distribution on the parameters
stan_population <- sim_spott_free_operant_group(nsubjects=80, task_environment = task_environment,
                                                parameters=list( #use this list to provide expressions that are evaluated to generate group parameter distributions
                                                  kappa=expression(rep(8, nsubjects)),
                                                  beta=expression(rep(300, nsubjects)),
                                                  cost=expression(rep(0, nsubjects)))
)

parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, beta, cost, kappa), mean)

out_dir <- "/Users/mnh5174/Data_Analysis/spott_modeling/data/vba_input_simulated_n80"
#out_dir <- "/Users/mnh5174/Data_Analysis/spott_modeling/data/vba_input_simulated_n5_minimal"
if (!dir.exists(out_dir)) { dir.create(out_dir) }
dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -beta, -cost, -kappa)

dsplit <- split(dsplit, dsplit$id)
sapply(1:length(dsplit), function(d) {
  id <- names(dsplit)[d]
  data <- dsplit[[d]]
  write.csv(data, file=file.path(out_dir, sprintf("%03s_spott_50.csv", id)), row.names=F)
})

#readr::write_csv(stan_population, path="data/stan_population_demo_trialdata.csv.gz")
write.csv(parmat, file=file.path(out_dir, "stan_population_demo_parameters.csv"), row.names=F)


## Older approach using switch cost approach

#free parameters:
# - alpha: learning rate
# - gamma: vigor sensitivity
# - nu: basal vigor
# - beta: motor refractory speed in p_respond
# - cost: switch cost (should be relative to Qs)
# - kappa: inverse temperature on softmax choice (switch probability)

initial_params <- list(
  value=c(     alpha=0.1,   gamma=2,   nu=-1,   beta=200, cost=0.01, kappa=3), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  cost=0,    kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, cost=5,    kappa=5),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, cost=1e-1, kappa=1e-1)
)

task_environment <- list(
  #prew=c(0.5,0.3),
  n_trials=60,
  trial_length=6000, #6 seconds
  bin_size=50
)

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_timesteps <- with(task_environment, trial_length/bin_size)
task_environment$outcomes <- with(task_environment, array(runif(n_timesteps*n_trials*3), dim=c(n_trials, n_timesteps, 3)))

set.seed(100)
#run the model at these parameter settings.
ins_results <- ins_wins(c(alpha=0.1, gamma=8, nu=0, beta=200, cost=0, kappa=2), fixed=NULL, task_environment, optimize=FALSE)

#get summary statistics
sstats <- get_sim_stats(ins_results, task_environment)
sum_df <- sstats$sum_df
all_df <- sstats$all_df

toplot <- all_df %>% select(trial, timestep, Q_1, Q_2, choice, outcome) %>% gather(key=action, value=Q, Q_1, Q_2)
ggplot(toplot %>% filter(trial <= 10 & choice !=0), aes(x=timestep, y=Q, color=factor(action))) + 
  geom_line() + geom_point(aes(x=timestep, y=2-choice, color=outcome)) + facet_wrap(~trial)

#simulate data for stan fitting
test_stan_sim <- sim_data_for_stan (initial_params$value, task_environment, n=100)

#simulate data using a population distribution on the parameters
stan_population <- sim_spott_free_operant_group(nsubjects=50, task_environment = task_environment)

#if you want just the parameters
parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, beta, cost, kappa), mean)

readr::write_csv(stan_population, path="data/stan_population_demo_trialdata.csv.gz")
write.csv(parmat, file="data/stan_population_demo_parameters.csv", row.names=F)

#plot diagnosis
str(stan_population)


#repeat the same model, but in 100 environments (regenerating GRWs)
library(doMC)
registerDoMC(6)
kappas <- seq(0.5, 4, by=0.5)
res <- foreach(i=seq_along(kappas)) %dopar% {
  pars <- initial_params$value
  pars["kappa"] <- kappas[i]
  xx <- repeat_forward_simulation(pars, task_environment)
  sdf <- xx$sum_df
  sdf$kappa <- kappas[i]
  return(sdf)
}


# res <- lapply(1:length(res), function(x) {
#   res[[x]]$kappa <- kappas[x]
#   return(res[[x]])
# })

res_combined <- bind_rows(res)
cor_stuff <- res_combined %>% group_by(kappa, replication) %>% summarize(vigor_value=cor(avg_value, nresp))

ex_plot <- ggplot(res_combined %>% filter(kappa==2 & replication==6), aes(x=avg_value, y=nresp))  + geom_point() + stat_smooth(method="lm") +
  theme_bw(base_size=21) + ggtitle("Association between total value\nand response vigor") + xlab("Total value of actions within a trial (a.u.)") +
  ylab("Number of responses per trial")

ggsave(ex_plot, file="figures/vigor_value_scatter.pdf", width=6, height=7)

overall_presses <- res_combined %>% group_by(kappa, replication) %>% do({
  df <- . 
  df <- df %>% select(nresp, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(nresp ~ Q_1 + Q_2, df)
  broom::tidy(m)
}) %>% ungroup()

overall_presses <- overall_presses %>% select(kappa, replication, term, estimate, p.value) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)

ss <- overall_presses %>% group_by(kappa, term) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))

g <- ggplot(ss, aes(x=kappa, y=mest, ymin=lo, ymax=hi, color=term)) +
  geom_pointrange(size=1.2, position=position_dodge(width=0.25)) + geom_hline(yintercept=0) +
  ylab("Std. coef. of value predicting button presses") +
  xlab("Switch/stay softmax temperature (kappa)") + 
  ggtitle("Number of presses equally associated with\nQ(1) and Q(2) values") +
  scale_color_discrete("Predictor") 

ggsave(g, file="figures/vigor_value_by_kappa.pdf", width=6, height=6)


#changing temperature in softmax does not alter correlation of vigor and avg value
#this makes sense because we are correlating with average value in environment. Changing choice stochasticity
ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")


#what about selectivity based on current value?
summary_presses <- res_combined %>% group_by(kappa, replication) %>% do({
  df <- .
  df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::glance(m)
}) %>% ungroup()

detailed_presses <- res_combined %>% group_by(kappa, replication) %>% do({
  df <- .
  df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::tidy(m)
}) %>% ungroup()


test2 <- detailed_presses %>% select(kappa, replication, term, estimate, p.value) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)

ss <- test2 %>% group_by(kappa, term) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))
g <- ggplot(ss, aes(x=kappa, y=mest, ymin=lo, ymax=hi, color=term)) +
  geom_pointrange(size=1.2) + geom_line(size=1.2) + geom_hline(yintercept=0) +
  ylab("Std. coef. of value predicting action 1 presses") +
  xlab("Switch/stay softmax temperature (kappa)") + 
  ggtitle("Number of presses for action 1 positively associated with\nQ(1) and negatively associated with Q(2)") +
  scale_color_discrete("Predictor")

ggsave(g, file="figures/explore_exploit_kappa_regweights.pdf", width=6, height=6)

ggplot(test2, aes(x=kappa, y=p.value, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange")

#exploration rate (exploit/explore) strongly controls the tendency of Q1 to drive 
ggplot(test2, aes(x=kappa, y=estimate, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  stat_smooth()


ss <- summary_presses %>% group_by(kappa) %>% summarize(mr2=mean(r.squared), hi=mr2 + sd(r.squared), lo=mr2 - sd(r.squared))
g <- ggplot(ss, aes(x=kappa, y=mr2, ymin=lo, ymax=hi)) + geom_pointrange(size=1.2) +
  xlab("Switch/stay softmax temperature (kappa)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
  ggtitle("Association between action value and\npreferential number of presses increases with kappa") + geom_line(size=1.2)


# g <- ggplot(summary_presses, aes(x=kappa, y=r.squared)) +
#   stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
#   xlab("Switch/stay softmax temperature (kappa)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
#   ggtitle("Association between value and\nnumber of presses increases with kappa") + stat_smooth(se=FALSE)

ggsave(g, file="figures/explore_exploit_kappa_r2.pdf", width=6, height=6)




#these are the complements from the single-simulation setting
summary(lm(n_1 ~ Q_1 + Q_2, sum_df))
summary(lm(n_2 ~ Q_1 + Q_2, sum_df))


#gives a sense of variability across replications
cor_stuff <- xx$sum_df %>% group_by(replication) %>% summarize(vigor_value=cor(avg_value, nresp))
ggplot(cor_stuff, aes(x=1, y=vigor_value)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")


## Work on effect of switch cost

costs <- seq(0, 0.5, by=0.05)
res_cost <- foreach(i=seq_along(costs)) %dopar% {
  pars <- initial_params$value
  pars["cost"] <- costs[i]
  xx <- repeat_forward_simulation(pars, task_environment)
  sdf <- xx$sum_df
  sdf$cost <- costs[i]
  return(sdf)
}

res_cost_combined <- bind_rows(res_cost)
cor_stuff <- res_cost_combined %>% group_by(cost, replication) %>% summarize(vigor_value=cor(avg_value, nresp))

ggplot(cor_stuff, aes(x=cost, y=vigor_value)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")

ggplot(res_cost_combined, aes(x=cost, y=nresp)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")


#what about selectivity based on current value?
summary_presses <- res_cost_combined %>% group_by(cost, replication) %>% do({
  df <- .
  df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::glance(m)
}) %>% ungroup()

detailed_presses <- res_cost_combined %>% group_by(cost, replication) %>% do({
  df <- .
  df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::tidy(m)
}) %>% ungroup()


test2 <- detailed_presses %>% select(cost, replication, term, estimate, p.value) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)

ss <- test2 %>% group_by(cost, term) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))
g <- ggplot(ss, aes(x=cost, y=mest, ymin=lo, ymax=hi, color=term)) +
  geom_pointrange(size=1.2) + geom_line(size=1.2) + geom_hline(yintercept=0) +
  ylab("Std. coef. of value predicting action 1 presses") +
  xlab("Switch/stay softmax temperature (cost)") + 
  ggtitle("Number of presses for action 1 positively associated with\nQ(1) and negatively associated with Q(2)") +
  scale_color_discrete("Predictor")

ggsave(g, file="figures/explore_exploit_cost_regweights.pdf", width=6, height=6)

ggplot(test2, aes(x=cost, y=p.value, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange")

#exploration rate (exploit/explore) strongly controls the tendency of Q1 to drive 
ggplot(test2, aes(x=cost, y=estimate, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  stat_smooth()


ss <- summary_presses %>% group_by(cost) %>% summarize(mr2=mean(r.squared), hi=mr2 + sd(r.squared), lo=mr2 - sd(r.squared))
g <- ggplot(ss, aes(x=cost, y=mr2, ymin=lo, ymax=hi)) + geom_pointrange(size=1.2) +
  xlab("Switch/stay softmax temperature (cost)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
  ggtitle("Association between action value and\npreferential number of presses increases with cost") + geom_line(size=1.2)


# g <- ggplot(summary_presses, aes(x=cost, y=r.squared)) +
#   stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
#   xlab("Switch/stay softmax temperature (cost)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
#   ggtitle("Association between value and\nnumber of presses increases with cost") + stat_smooth(se=FALSE)

ggsave(g, file="figures/explore_exploit_cost_r2.pdf", width=6, height=6)


##what about gamma (sensitivity to reward)

gammas <- seq(0.5, 4, by=0.5)
res_gamma <- foreach(i=seq_along(gammas)) %dopar% {
  pars <- initial_params$value
  pars["gamma"] <- gammas[i]
  xx <- repeat_forward_simulation(pars, task_environment)
  sdf <- xx$sum_df
  sdf$gamma <- gammas[i]
  return(sdf)
}

res_gamma_combined <- bind_rows(res_gamma)
ggplot(res_gamma_combined, aes(x=gamma, y=nresp)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")

#what about selectivity based on current value?
summary_presses <- res_gamma_combined %>% group_by(gamma, replication) %>% do({
  df <- .
  df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::glance(m)
}) %>% ungroup()

detailed_presses <- res_gamma_combined %>% group_by(gamma, replication) %>% do({
  df <- .
  df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::tidy(m)
}) %>% ungroup()


test2 <- detailed_presses %>% select(gamma, replication, term, estimate, p.value) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)

ss <- test2 %>% group_by(gamma, term) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))
g <- ggplot(ss, aes(x=gamma, y=mest, ymin=lo, ymax=hi, color=term)) +
  geom_pointrange(size=1.2) + geom_line(size=1.2) + geom_hline(yintercept=0) +
  ylab("Std. coef. of value predicting action 1 presses") +
  xlab("Switch/stay softmax temperature (gamma)") + 
  ggtitle("Number of presses for action 1 positively associated with\nQ(1) and negatively associated with Q(2)") +
  scale_color_discrete("Predictor")

ggsave(g, file="figures/explore_exploit_gamma_regweights.pdf", width=6, height=6)

ggplot(test2, aes(x=gamma, y=p.value, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange")

#exploration rate (exploit/explore) strongly controls the tendency of Q1 to drive 
ggplot(test2, aes(x=gamma, y=estimate, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  stat_smooth()


ss <- summary_presses %>% group_by(gamma) %>% summarize(mr2=mean(r.squared), hi=mr2 + sd(r.squared), lo=mr2 - sd(r.squared))
g <- ggplot(ss, aes(x=gamma, y=mr2, ymin=lo, ymax=hi)) + geom_pointrange(size=1.2) +
  xlab("Switch/stay softmax temperature (gamma)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
  ggtitle("Association between action value and\npreferential number of presses increases with gamma") + geom_line(size=1.2)


# g <- ggplot(summary_presses, aes(x=gamma, y=r.squared)) +
#   stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
#   xlab("Switch/stay softmax temperature (gamma)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
#   ggtitle("Association between value and\nnumber of presses increases with gamma") + stat_smooth(se=FALSE)

ggsave(g, file="figures/explore_exploit_gamma_r2.pdf", width=6, height=6)




##Proportion matching:


#what about proportion matching?
cor.test(~n1_n2 + Q1_Q2, filter(sum_df, n1_n2 < 15)) #ratio
cor.test(~n1_m_n2 + Q1_m_Q2, sum_df) #difference

cor.test(~n1_n2 + p1_p2, filter(sum_df, n1_n2 < 15))

# ggplot(sum_df, aes(x=n1_n2, y=Q1_Q2)) + geom_point() + xlim(0,15) + stat_smooth()
# ggplot(sum_df, aes(x=n1_n2, y=p1_p2)) + geom_point() + xlim(0,15) + stat_smooth()

#log matching
summary(lm(log_n1_n2 ~ log_p1_p2, filter(sum_df, log_p1_p2 > -1 & log_n1_n2 < 1.9)))

ggplot(filter(sum_df, log_p1_p2 > -1 & log_n1_n2 < 1.9), aes(x=log_n1_n2, y=log_p1_p2)) + geom_point() + stat_smooth(method="lm")

cor_stuff <- res_combined %>% filter(log_p1_p2 > -1 & log_n1_n2 < 1.9) %>% group_by(kappa, replication) %>% summarize(vigor_value=cor(log_n1_n2, log_p1_p2))
g <- ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() + 
  stat_smooth(method="loess",se=FALSE, color="black") +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  ylab("Log-linear association between log\nresponse ratio and log probability ratio") +
  xlab("Switch/stay softmax temperature (kappa)") +
  ggtitle("Probability matching depends\non explore/exploit tradeoff") +
  theme_bw(base_size=21)

ggsave(g, file="figures/log_lin_explore_exploit.pdf", width=7, height=7)

ex_plot <- ggplot(res_combined %>% filter(kappa==4 & replication==6), aes(x=log_p1_p2, y=log_n1_n2))  + geom_point() + stat_smooth(method="lm") +
  theme_bw(base_size=21) + 
  ggtitle("Log response ratios \nvs. log reward probabilities") + xlab("log(p1/p2)") +
  ylab("log(n1/n2)") + annotate(geom="text", x=-0.5, y=3, label="kappa = 4", size=8, hjust=0)

summary(lm(log_n1_n2 ~ log_p1_p2, res_combined %>% filter(kappa==4 & replication==6)))

ggsave(ex_plot, file="figures/log_matching_scatter.pdf", width=7, height=6)


#stat_smooth(method="lm", formula=y ~ I(1/x), se=FALSE)



#if we want to plot q traces, we need to gather up Q_1 and Q_2
# all_df <- reshape2::melt(ins_results$Q_tba, varnames=c("trial", "timestep", "action")) %>% 
#   full_join(choices_df) %>% full_join(rewards_df) %>% arrange(trial, timestep)

for_plotting <- all_df %>% gather(key=action, value=value, Q_1, Q_2)
ggplot(for_plotting %>% filter(trial < 12 & trial > 6), aes(x=timestep, y=value, color=factor(action))) + geom_line() + facet_wrap(~trial, ncol=1)




#other diagnostic plots and analyses



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


resp_plot <- ggplot(sum_df, aes(x=trial, y=nresp)) + geom_line()
resp_plot

#merge back reward schedule

#orthogonalize overall vigor versus proportion of 1 vs. 2?
#so in richer environments, we see faster responding, but the ratios are preserved

cor.test(~nresp + avg_value, sum_df) #wow, highly correlated! So, higher average value within a trial correlates with responses on that trial
ccf_resp_value <- with(sum_df, ccf(nresp, avg_value))
ggplot(sum_df %>% gather(key="variable", value="value", nresp, avg_value) %>% group_by(variable) %>% mutate(value=scale(value)), aes(x=trial, y=value, color=variable)) + geom_line()

ggplot(sum_df, aes(x=avg_value, y=nresp)) + geom_point()

cor.test(~n_1 + Q_1, sum_df) #strong correlation of Q_1 value with number of times it is chosen
cor.test(~n_2 + Q_2, sum_df) #similar for Q_2

#specificity
summary(lm(n_1 ~ Q_1 + Q_2, sum_df))
summary(lm(n_2 ~ Q_1 + Q_2, sum_df))
library(lm.beta)
lm.beta(lm(n_1 ~ Q_1 + Q_2, sum_df))
lm.beta(lm(n_2 ~ Q_1 + Q_2, sum_df))

summary(lm(nresp ~ p_1 + p_2, sum_df))
lm.beta(lm(nresp ~ p_1 + p_2, sum_df))
summary(lm(nresp ~ Q_1 * Q_2, sum_df)) #vigor effect
summary(lm(nresp ~ Q_1 + Q_2, sum_df)) #vigor effect

#good, the GRWs are uncorrelated
cor.test(~p_1 + p_2, sum_df)

#what about proportion matching?
cor.test(~n1_n2 + Q1_Q2, filter(sum_df, n1_n2 < 15)) #ratio
cor.test(~n1_m_n2 + Q1_m_Q2, sum_df) #difference

cor.test(~n1_n2 + p1_p2, filter(sum_df, n1_n2 < 15))

# ggplot(sum_df, aes(x=n1_n2, y=Q1_Q2)) + geom_point() + xlim(0,15) + stat_smooth()
# ggplot(sum_df, aes(x=n1_n2, y=p1_p2)) + geom_point() + xlim(0,15) + stat_smooth()

#log matching
summary(lm(log_n1_n2 ~ log_p1_p2, filter(sum_df, log_p1_p2 > -1 & log_n1_n2 < 1.9)))

ggplot(filter(sum_df, log_p1_p2 > -1 & log_n1_n2 < 1.9), aes(x=log_n1_n2, y=log_p1_p2)) + geom_point() + stat_smooth(method="lm")

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

sum_df <- all_df %>% group_by(trial) %>% summarize(nresp=sum(choice > 0), avg_value=sum(Q_1, Q_2))
resp_plot <- ggplot(sum_df, aes(x=trial, y=nresp)) + geom_line()
cor.test(~nresp + avg_value, sum_df) #wow, highly correlated! So, higher average value within a trial correlates with responses on that trial
ccf_resp_value <- with(sum_df, ccf(nresp, avg_value))
ggplot(sum_df %>% gather(key="variable", value="value", nresp, avg_value) %>% group_by(variable) %>% mutate(value=scale(value)), aes(x=trial, y=value, color=variable)) + geom_line()

