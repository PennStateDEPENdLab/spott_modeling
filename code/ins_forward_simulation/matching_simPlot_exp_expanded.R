library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data")
load("matching_sim_exp.RData")
res_combined <- matching_sim

output_dir <- "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/ins_forward_simulation/figures"
setwd(output_dir)

ex_plot <- ggplot(res_combined %>% filter(kappa==4 & replication==6), aes(x=avg_value, y=nresp, color=p_1+p_2))  + geom_point() + stat_smooth(method="lm") +
  theme_bw(base_size=21) + ggtitle("Association between total value\nand response vigor") + xlab("Total value of actions within a trial (a.u.)") +
  ylab("Number of responses per trial") #+
  #facet_grid(p_1~p_2)

ggsave(ex_plot, file="exp/vigor_value_scatter_exp_expanded.pdf", width=10, height=8)

ex_plot2 <- ggplot(res_combined %>% filter(kappa==2 & replication==6), aes(x=avg_value, y=nresp, color=p_1+p_2, group=p_1+p_2))  + geom_point() + stat_smooth(method="lm", se=FALSE) +
  theme_bw(base_size=21) + ggtitle("Association between total value\nand response vigor") + xlab("Total value of actions within a trial (a.u.)") +
  ylab("Number of responses per trial")


# scale_if <- function(x) { if (sd(x) > 0) scale(x) else x }
overall_presses <- res_combined %>% group_by(kappa, replication, p_1, p_2) %>% do({
  # options(error=browser)
  df <- . 
  df_scale <- df %>% select(nresp, Q_1, Q_2) %>% mutate(across(where(~sd(.x) > 0), scale))
  n_counts <- df %>% summarize(across(c(n_1, n_2), sum))
  m <- lm(nresp ~ Q_1 + Q_2, df_scale)
  broom::tidy(m) %>% bind_cols(n_counts)
}) %>% ungroup()

# QUESTION: I suppose this does the job, 
# but why did I always get errors when I tried to use
# mutate(across(where())))

# I assume we are not touching the t- and p-values?
overall_presses2 <- overall_presses %>% 
  filter(term != "(Intercept)") %>%
  mutate(estimate = case_when(
    is.na(estimate) & n_1 !=0 & n_2 !=0 ~ 0,
    TRUE~estimate
  ))
  
# overall_presses2 <- overall_presses %>% select(kappa, replication, term, estimate, p.value, p_1, p_2) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)

ss <- overall_presses2 %>% group_by(kappa, term, p_1, p_2) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))

g <- ggplot(ss, aes(x=kappa, y=mest, ymin=lo, ymax=hi, color=term)) +
  geom_pointrange(size=1.2, position=position_dodge(width=0.25)) + geom_hline(yintercept=0) +
  ylab("Std. coef. of value predicting button presses") +
  xlab("Switch/stay softmax temperature (kappa)") + 
  ggtitle("Number of presses equally associated with\nQ(1) and Q(2) values") +
  scale_color_discrete("Predictor")+
  facet_grid(p_1~p_2)

ggsave(g, file="vigor_value_by_kappa_exp_expanded.pdf", width=49, height=49)

# cor_stuff <- res_combined %>% group_by(kappa, replication, p_1, p_2) %>% summarize(vigor_value=cor(avg_value, nresp))
# 
# g <- ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() + 
#   stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange")+
#   facet_grid(p_1~p_2)

# TO DO - modify the plot at the top to see linear/non-linear pattern

# what about selectivity based on current value? --------------------------

summary_presses <- res_combined %>% group_by(kappa, replication, p_1, p_2) %>% do({
  options(error=browser)
  df <- .
  df_scale <- df %>% select(nresp, Q_1, Q_2) %>% mutate(across(where(~sd(.x) > 0), scale))
  n_counts <- df %>% summarize(across(c(n_1, n_2), sum))
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::glance(m) %>% bind_cols(n_counts)
}) %>% ungroup()

# QUESTION [looking into late]: this gave 20 warnings of
# In summary.lm(x) : essentially perfect fit: summary may be unreliable

detailed_presses <- res_combined %>% group_by(kappa, replication, p_1, p_2) %>% do({
  options(error=browser)
  df <- .
  df_scale <- df %>% select(nresp, Q_1, Q_2) %>% mutate(across(where(~sd(.x) > 0), scale))
  n_counts <- df %>% summarize(across(c(n_1, n_2), sum))
  m <- lm(n_1 ~ Q_1 + Q_2, df)
  broom::tidy(m) %>% bind_cols(n_counts)
}) %>% ungroup()

# same warning messages

options(error=NULL)
detailed_presses2 <- detailed_presses %>% 
  filter(term != "(Intercept)") %>%
  mutate(estimate = case_when(
    is.na(estimate) & n_1 !=0 & n_2 !=0 ~ 0,
    TRUE~estimate
  ))

test2 <- detailed_presses2 %>% select(kappa, replication, p_1, p_2, term, estimate, p.value) #%>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)

ss <- test2 %>% group_by(kappa, term, p_1, p_2) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))
g <- ggplot(ss, aes(x=kappa, y=mest, ymin=lo, ymax=hi, color=term)) +
  geom_pointrange(size=1.2) + geom_line(size=1.2) + geom_hline(yintercept=0) +
  ylab("Std. coef. of value predicting action 1 presses") +
  xlab("Switch/stay softmax temperature (kappa)") + 
  ggtitle("Number of presses for action 1 positively associated with\nQ(1) and negatively associated with Q(2)") +
  scale_color_discrete("Predictor")+
  facet_grid(p_1~p_2)

ggsave(g, file="exp/explore_exploit_kappa_regweights_exp_expanded.pdf", width=49, height=49)

# #exploration rate (exploit/explore) strongly controls the tendency of Q1 to drive 
# g<- ggplot(test2, aes(x=kappa, y=estimate, color=term)) +
#   stat_summary(fun.data = "mean_cl_boot", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
#   stat_smooth()+
#   facet_grid(p_1~p_2)

ss <- summary_presses %>% group_by(kappa, p_1, p_2) %>% summarize(mr2=mean(r.squared), hi=mr2 + sd(r.squared), lo=mr2 - sd(r.squared))
g <- ggplot(ss, aes(x=kappa, y=mr2, ymin=lo, ymax=hi)) + geom_pointrange(size=1.2) +
  xlab("Switch/stay softmax temperature (kappa)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
  ggtitle("Association between action value and\npreferential number of presses increases with kappa") + geom_line(size=1.2)+
  facet_grid(p_1~p_2)

ggsave(g, file="exp/explore_exploit_kappa_r2_exp_expanded.pdf", width=49, height=49)

# Proportion matching: ----------------------------------------------------

#what about proportion matching?
# sum_df <- res_combined %>% filter(kappa == 4) #RM: Just using kappa = 4
# ggplot(filter(sum_df, log_p1_p2 > -1 & log_n1_n2 < 1.9), aes(x=log_n1_n2, y=log_p1_p2)) + 
#   geom_point() + stat_smooth(method="lm") + facet_grid(p_1~p_2)

# Needed to set log_n1_n2 > -Inf; otherwise the cor() returns NaN due to an undefined value of log_n1_n2 
cor_stuff <- res_combined %>% filter(log_p1_p2 > -1 & log_n1_n2 < 1.9 & log_n1_n2 > -Inf) %>% group_by(kappa) %>% summarize(vigor_value=cor(log_n1_n2, log_p1_p2))

# cor_stuff2 <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(kappa) %>% summarize(vigor_value=cor(log_n1_n2, log_p1_p2))

# # Can use tally() to help examine the data
# cor_stuff <- res_combined %>% filter(log_p1_p2 > -1 & log_n1_n2 < 1.9) %>% group_by(kappa, replication) %>%
#   tally(p_1, name="np1")
  
# 50 warnings: In cor(log_n1_n2, log_p1_p2) : the standard deviation is zero, if we add p_1, p_2 to group_by()
# QUESTION: I suppose it wouldn't make sense to group by p_1, p_2 because their ratio will be constant, given they are deterministic in the current simulation

# When we extend kappa from 4 to 8, we actually do see a downward trend
g <- ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() +
  stat_smooth(method="loess",se=FALSE, color="black") +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
  ylab("Log-linear association between log\nresponse ratio and log probability ratio") +
  xlab("Switch/stay softmax temperature (kappa)") +
  ggtitle("Probability matching depends\non explore/exploit tradeoff") +
  theme_bw(base_size=21) #+ 
  #geom_hline(yintercept=0.8, linetype="dashed")

ggsave(g, file="log_lin_explore_exploit_exp_expanded.pdf", width=8, height=8)


# Got this error, which is also now happening to the ex_plot above
# Error in `fortify()`:
#   ! `data` must be a data frame, or other object coercible by `fortify()`, not an S3 object with class gg/ggplot.
# Run `rlang::last_error()` to see where the error occurred.
# added log_n1_n2 > -Inf & log_n1_n2 < Inf

# df <- res_combined %>% filter(kappa==4 & replication==6 & log_n1_n2 > -Inf & log_n1_n2 < Inf)
# ex_plot <- ggplot(df, aes(x=log_p1_p2, y=log_n1_n2, color=p_1+p_2))  + geom_point() + stat_smooth(method="lm") +
#   theme_bw(base_size=21) + 
#   ggtitle("Log response ratios \nvs. log reward probabilities") + xlab("log(p1/p2)") +
#   ylab("log(n1/n2)") 
