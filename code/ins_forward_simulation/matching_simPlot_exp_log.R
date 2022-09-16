library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data")
load("matching_sim_exp.RData")
res_combined <- matching_sim

output_dir <- "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/ins_forward_simulation/figures"
setwd(output_dir)

# Regress log_n1_n2 on log_p1_p2
# The plot from cor_stuff above used correlation between log_n1_n2 and log_p1_p2
log_lm_reversal <- res_combined_reversal %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(kappa) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
  m <- lm(log_n1_n2 ~ log_p1_p2, df)
  broom::tidy(m)
}) %>% ungroup()

log_lm2_reversal <- log_lm_reversal %>% pivot_wider(names_from = term, values_from = estimate)
# log_lm3 <- log_lm %>% filter(term == "log_p1_p2")
plot(log_lm2_reversal$kappa, log_lm2_reversal$log_p1_p2) # a values increase as kappa increases; a is around 0.8 when kappa ~2.5
g<- ggplot(data = log_lm2_reversal) +
  geom_point(mapping = aes(x=kappa, y = log_p1_p2), color = "blue") + 
  ylab("a values") + 
  xlab("kappa") +
  geom_hline(yintercept=0.8, linetype="dashed")

plot(g)

plot(log_lm2$kappa, log_lm2$"(Intercept)") # log(b) values are mostly around 0 (i.e., b~1) unless kappa is large


# Calculate correlations between  log_n1_n2 and log_p1_p2, instead of regression coefficients
log_cor <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(kappa) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
  m <- cor(df$log_n1_n2,df$log_p1_p2)
  broom::tidy(m)
}) %>% ungroup()

g<- ggplot(data = log_cor) +
  geom_point(mapping = aes(x=kappa, y = x), color = "blue") + 
  ylab("log_log_correlations") + 
  xlab("kappa") +
  geom_hline(yintercept=0.8, linetype="dashed")

plot(g)


# ---- 8/15 trouble shoot

# p1, p2 for the RW condition
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)

# p1, p2 for the reversal condition
task_environment <- setup_task_environment(
  prew = list(
    expression(c(rep(0.7, 35), rep(0.3,15), rep(0.7, 35), rep(0.3,15))),
    expression(1- c(rep(0.7, 35), rep(0.3,15), rep(0.7, 35), rep(0.3,15)))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)

# Data simulation; the simulationt takes a while now because of the number of kappas (8->16) and number of trials (20->100)
library(doMC)
registerDoMC(6)
kappas <- seq(0.5, 8, by=0.5) 
res <- foreach(i=seq_along(kappas)) %dopar% {
  pars <- initial_params #$value
  pars["kappa"] <- kappas[i]
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$kappa <- kappas[i]
  return(sdf)
}

res_combined <- bind_rows(res)


# Plots using regress coefficients of log_n1_n2 on log_p1_p2
log_lm <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(kappa) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
  m <- lm(log_n1_n2 ~ log_p1_p2, df)
  broom::tidy(m)
}) %>% ungroup()

log_lm2 <- log_lm %>% pivot_wider(names_from = term, values_from = estimate)
# log_lm3 <- log_lm %>% filter(term == "log_p1_p2")
plot(log_lm2_reversal$kappa, log_lm2_reversal$log_p1_p2) # a values increase as kappa increases; a is around 0.8 when kappa ~2.5
g<- ggplot(data = log_lm2_reversal) +
  geom_point(mapping = aes(x=kappa, y = log_p1_p2), color = "blue") + 
  ylab("a values") + 
  xlab("kappa") +
  geom_hline(yintercept=0.8, linetype="dashed")

plot(g)

# Plots using correlations between log_n1_n2 and log_p1_p2
cor_stuff <- res_combined %>% filter(log_p1_p2 > -1 & log_n1_n2 < 1.9) %>% group_by(kappa, replication) %>% summarize(vigor_value=cor(log_n1_n2, log_p1_p2))
g <- ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() +
  stat_smooth(method="loess",se=FALSE, color="black") +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  ylab("Log-linear association between log\nresponse ratio and log probability ratio") +
  xlab("Switch/stay softmax temperature (kappa)") +
  ggtitle("Probability matching depends\non explore/exploit tradeoff") +
  theme_bw(base_size=21)

plot(g)

# # This does the same thing as above but the plots are not as fancy
# # Calculate correlations between  log_n1_n2 and log_p1_p2, instead of regression coefficients
# log_cor <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(kappa) %>% do({
#   df <- .
#   df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
#   m <- cor(df$log_n1_n2,df$log_p1_p2)
#   broom::tidy(m)
# }) %>% ungroup()
# 
# g<- ggplot(data = log_cor) +
#   geom_point(mapping = aes(x=kappa, y = x), color = "blue") + 
#   ylab("log_log_correlations") + 
#   xlab("kappa") +
#   geom_hline(yintercept=0.8, linetype="dashed")
# 
# plot(g)

