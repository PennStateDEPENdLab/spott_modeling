# this script is modified from MNH's sne_2019_simulations.R, for the exp model

library(dplyr)
library(ggplot2)

repo_dir <- "~/Documents/GitHub/spott_modeling/code/ins_forward_simulation"
# repo_dir <- "/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/ins_forward_simulation"
setwd(repo_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

initial_params <- c(alpha=0.1, gamma=2, nu=0.5, omega=0, kappa = 4)

# example environment with 2 actions that start at .7 and .3 reward probability, with .08 step size
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)

# task_environment$model <- "exp"

#repeat the same model, but in 100 environments (regenerating GRWs)
library(doMC)
registerDoMC(6)
# RM: We used #kvals <- c(1, 4, 8), or to prioritize low kappas kvals <- 1.14^(seq(-10, 20, 2))
#     in previous simulation. I'm keeping the kappa values here unchanged from the sne 2019 script,
#     because we want to see how matching is followed at low/high kappa
kappas <- seq(0.5, 8, by=0.5) 
res <- foreach(i=seq_along(kappas)) %dopar% {
  pars <- initial_params #$value
  pars["kappa"] <- kappas[i]
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$kappa <- kappas[i]
  return(sdf)
}

write.csv(res, file = "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data/matching_sample_data.csv")

output_dir <- "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/ins_forward_simulation/figures"
setwd(output_dir)

res_combined <- bind_rows(res)
cor_stuff <- res_combined %>% group_by(kappa, replication) %>% summarize(vigor_value=cor(avg_value, nresp))

# str(res_combined)
# ggplot(res_combined, aes(x=p_1, y=p_2, color=nresp)) + geom_point() + facet_wrap(~kappa)
# 
# res_combined %>% filter(replication == 1) %>% head()
# res_combined %>% filter(replication == 2) %>% head()
# 
# ggplot(res_combined, aes(x=p_1, y=p_2, color=n_2)) + geom_point() + facet_wrap(~kappa)


ex_plot <- ggplot(res_combined %>% filter(kappa==2 & replication==6), aes(x=avg_value, y=nresp))  + geom_point() + stat_smooth(method="lm") +
  theme_bw(base_size=21) + ggtitle("Association between total value\nand response vigor") + xlab("Total value of actions within a trial (a.u.)") +
  ylab("Number of responses per trial")

# plot(ex_plot)
ggsave(ex_plot, file="exp/vigor_value_scatter_exp.pdf", width=6, height=7)


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

ggsave(g, file="exp/vigor_value_by_kappa_exp.pdf", width=6, height=6)

# summary(aov(mest~term, data = ss))

#changing temperature in softmax does not alter correlation of vigor and avg value
#this makes sense because we are correlating with average value in environment. Changing choice stochasticity
# RM: Not sure right now about what will happen at even higher kappas
ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")


# what about selectivity based on current value? --------------------------

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

#plot(g)
ggsave(g, file="exp/explore_exploit_kappa_regweights_exp.pdf", width=6, height=6)

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

ggsave(g, file="exp/explore_exploit_kappa_r2_exp.pdf", width=6, height=6)


## Skipping the "complements from the single-simulation setting"


# Work on effect of switch cost -------------------------------------------

costs <- seq(0, 0.5, by=0.05)
res_cost <- foreach(i=seq_along(costs)) %dopar% {
  pars <- initial_params #$value
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


# what about selectivity based on current value? --------------------------

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

ggsave(g, file="exp/explore_exploit_cost_regweights_exp.pdf", width=6, height=6)

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

ggsave(g, file="exp/explore_exploit_cost_r2_exp.pdf", width=6, height=6)


# what about gamma (sensitivity to reward) --------------------------------

gammas <- seq(0.5, 4, by=0.5)
res_gamma <- foreach(i=seq_along(gammas)) %dopar% {
  pars <- initial_params #$value
  pars["gamma"] <- gammas[i]
  xx <- repeat_forward_simulation(pars, task_environment)
  sdf <- xx$sum_df
  sdf$gamma <- gammas[i]
  return(sdf)
}

res_gamma_combined <- bind_rows(res_gamma)
ggplot(res_gamma_combined, aes(x=gamma, y=nresp)) + #geom_boxplot() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")


# what about selectivity based on current value? --------------------------

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

ggsave(g, file="exp/explore_exploit_gamma_regweights_exp.pdf", width=6, height=6)

ggplot(test2, aes(x=gamma, y=p.value, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange")

ggplot(test2, aes(x=gamma, y=estimate, color=term)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  stat_smooth()

ss <- summary_presses %>% group_by(gamma) %>% summarize(mr2=mean(r.squared), hi=mr2 + sd(r.squared), lo=mr2 - sd(r.squared))
g <- ggplot(ss, aes(x=gamma, y=mr2, ymin=lo, ymax=hi)) + geom_pointrange(size=1.2) +
  xlab("Switch/stay softmax temperature (gamma)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
  ggtitle("Association between action value and\npreferential number of presses increases with gamma") + geom_line(size=1.2)

ggsave(g, file="exp/explore_exploit_gamma_r2_exp.pdf", width=6, height=6)


# Proportion matching: ----------------------------------------------------

#what about proportion matching?
sum_df <- res[[8]] #RM: Just using kappa = 4

cor.test(~n1_n2 + Q1_Q2, filter(sum_df, n1_n2 < 15)) #ratio #RM seems like there are NaN's, but leaving this for now
cor.test(~n1_m_n2 + Q1_m_Q2, sum_df) #difference

cor.test(~n1_n2 + p1_p2, filter(sum_df, n1_n2 < 15))

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

ggsave(g, file="exp/log_lin_explore_exploit_exp.pdf", width=7, height=7)

ex_plot <- ggplot(res_combined %>% filter(kappa==4 & replication==6), aes(x=log_p1_p2, y=log_n1_n2))  + geom_point() + stat_smooth(method="lm") +
  theme_bw(base_size=21) + 
  ggtitle("Log response ratios \nvs. log reward probabilities") + xlab("log(p1/p2)") +
  ylab("log(n1/n2)") + annotate(geom="text", x=-0.5, y=3, label="kappa = 4", size=8, hjust=0)

summary(lm(log_n1_n2 ~ log_p1_p2, res_combined %>% filter(kappa==4 & replication==6)))

ggsave(ex_plot, file="exp/log_matching_scatter_exp.pdf", width=7, height=6)

# RM: skipping the script below Line 322 in the original script, for now
