# Based on 6/8/2022 meeting, this script modifies matching_simulations_exp.R
# to include different probability simulations

library(dplyr)
library(ggplot2)
# library(doMC)
library(doFuture)
library(doRNG)
library(foreach)
library(future.batchtools)
library(iterators)

# repo_dir <- "~/Documents/GitHub/spott_modeling/code/ins_forward_simulation"
# setwd(repo_dir)
repo_dir <- "/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/ins_forward_simulation"
setwd(repo_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

out_dir <- "/nas/longleaf/home/maruofan/mnhallqlab/projects/spott_modeling/matching_exp"
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# initial_params <- c(alpha=0.1, gamma=2, nu=0.5, omega=0, kappa = 4)

# rw_prob <- expand.grid(p1 = seq(0.05, .95, .1), p2=seq(0.05, .95, .1))
rw_prob <- expand.grid(kappas = seq(0.5, 8, by=0.5), p1 = seq(0.05, .15, .1), p2=seq(0.05, .15, .1))
rw_prob$cond_id <- 1:nrow(rw_prob)

chunk_size <- 10
registerDoFuture() # tell dopar to use future compute mechanism

# future set up: for using slurm as the backend of future
# https://tdhock.github.io/blog/2019/future-batchtools/
future::plan(
  future.batchtools::batchtools_slurm,
  template = "slurm-simple",
  resources = list(
    walltime = 10 * 60 * chunk_size, # 10 minutes per condition
    memory = 2000, # 2 GB
    ncpus = 1, # just needs one CPU within each chunk
    chunks.as.arrayjobs = FALSE
  )
)

# for (j in 1:nrow(rw_prob)){
# 
#   # grwalk defaut: function(len, start=0.5, step_sd=0.025, max_p=0.8, min_p=0.2) 
#   task_environment <- setup_task_environment(
#     prew = list(
#       expression(grwalk(n_trials, start = rw_prob$p1[j], 0, max_p=1, min_p=0)),
#       expression(grwalk(n_trials, start = rw_prob$p2[j], 0, max_p=1, min_p=0))
#     ),
#     n_trials = 100, #sne 2019 may have used 200
#     model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
#   )
# }

res_combined <- foreach(
  cond = iter(rw_prob, by = "row"), .options.future = list(chunk.size = chunk_size),
  .export = c("grwalk")
) %dorng% {
  task_environment <- setup_task_environment(
    prew = list(
      expression(grwalk(n_trials, start = cond$p1, 0, max_p=1, min_p=0)),
      expression(grwalk(n_trials, start = cond$p2, 0, max_p=1, min_p=0))
    ),
    #n_trials = 100, #sne 2019 may have used 200
    model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
  )
  
  pars <- c(alpha=0.1, gamma=2, nu=0.5, omega=0, kappa = 4) #initial_params #$value
  pars["kappa"] <- cond$kappas
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$kappa <- cond$kappas
  
  res_combined <- bind_rows(sdf)
  write.csv(res_combined, file = file.path(out_dir, paste0("matching_sim_exp_", cond$cond_id, ".csv")), row.names=F)
  return(res_combined)
}

# registerDoMC(6)
# kappas <- seq(0.5, 8, by=0.5) 
# res <- foreach(i=seq_along(kappas)) %dopar% {
#   pars <- initial_params #$value
#   pars["kappa"] <- kappas[i]
#   xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
#   sdf <- xx$sum_df
#   sdf$kappa <- kappas[i]
#   return(sdf)
# }

# output_dir <- "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/ins_forward_simulation/figures"
# setwd(output_dir)
# 
# res_combined <- bind_rows(res)
# cor_stuff <- res_combined %>% group_by(kappa, replication) %>% summarize(vigor_value=cor(avg_value, nresp))
# 
# # Association between total value\nand response vigor
# ex_plot <- ggplot(res_combined %>% filter(kappa==2 & replication==6), aes(x=avg_value, y=nresp))  + geom_point() + stat_smooth(method="lm") +
#   theme_bw(base_size=21) + ggtitle("Association between total value\nand response vigor") + xlab("Total value of actions within a trial (a.u.)") +
#   ylab("Number of responses per trial")
# 
# # Number of pressess associated with Q1 and Q2 values
# overall_presses <- res_combined %>% group_by(kappa, replication) %>% do({
#   df <- . 
#   df <- df %>% select(nresp, Q_1, Q_2) %>% mutate_all(scale)
#   m <- lm(nresp ~ Q_1 + Q_2, df)
#   broom::tidy(m)
# }) %>% ungroup()
# 
# overall_presses <- overall_presses %>% select(kappa, replication, term, estimate, p.value) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)
# 
# ss <- overall_presses %>% group_by(kappa, term) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))
# 
# g <- ggplot(ss, aes(x=kappa, y=mest, ymin=lo, ymax=hi, color=term)) +
#   geom_pointrange(size=1.2, position=position_dodge(width=0.25)) + geom_hline(yintercept=0) +
#   ylab("Std. coef. of value predicting button presses") +
#   xlab("Switch/stay softmax temperature (kappa)") + 
#   ggtitle("Number of presses equally associated with\nQ(1) and Q(2) values") +
#   scale_color_discrete("Predictor") 
# 
# ## Selectivity based on current value
# 
# # Number of presses for action 1 and 2
# summary_presses <- res_combined %>% group_by(kappa, replication) %>% do({
#   df <- .
#   df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
#   m <- lm(n_1 ~ Q_1 + Q_2, df)
#   broom::glance(m)
# }) %>% ungroup()
# 
# detailed_presses <- res_combined %>% group_by(kappa, replication) %>% do({
#   df <- .
#   df <- df %>% select(n_1, Q_1, Q_2) %>% mutate_all(scale)
#   m <- lm(n_1 ~ Q_1 + Q_2, df)
#   broom::tidy(m)
# }) %>% ungroup()
# 
# 
# test2 <- detailed_presses %>% select(kappa, replication, term, estimate, p.value) %>% filter(term %in% c("Q_1", "Q_2")) #%>% spread(key=term, value=estimate)
# 
# ss <- test2 %>% group_by(kappa, term) %>% summarize(mest=mean(estimate), hi=mest + sd(estimate), lo=mest - sd(estimate))
# g <- ggplot(ss, aes(x=kappa, y=mest, ymin=lo, ymax=hi, color=term)) +
#   geom_pointrange(size=1.2) + geom_line(size=1.2) + geom_hline(yintercept=0) +
#   ylab("Std. coef. of value predicting action 1 presses") +
#   xlab("Switch/stay softmax temperature (kappa)") + 
#   ggtitle("Number of presses for action 1 positively associated with\nQ(1) and negatively associated with Q(2)") +
#   scale_color_discrete("Predictor")
# 
# # Association between action value and\npreferential number of presses increases with kappa
# ss <- summary_presses %>% group_by(kappa) %>% summarize(mr2=mean(r.squared), hi=mr2 + sd(r.squared), lo=mr2 - sd(r.squared))
# g <- ggplot(ss, aes(x=kappa, y=mr2, ymin=lo, ymax=hi)) + geom_pointrange(size=1.2) +
#   xlab("Switch/stay softmax temperature (kappa)") + ylab("R2 of association between presses (n1) and values (Q1 and Q2)") +
#   ggtitle("Association between action value and\npreferential number of presses increases with kappa") + geom_line(size=1.2)
# 
# ## Log ratios (proportion matching)
# 
# sum_df <- res[[8]] #RM: Just using kappa = 4
# 
# cor.test(~n1_n2 + Q1_Q2, filter(sum_df, n1_n2 < 15)) #ratio #RM seems like there are NaN's, but leaving this for now
# cor.test(~n1_m_n2 + Q1_m_Q2, sum_df) #difference
# 
# cor.test(~n1_n2 + p1_p2, filter(sum_df, n1_n2 < 15))
# 
# cor_stuff <- res_combined %>% filter(log_p1_p2 > -1 & log_n1_n2 < 1.9) %>% group_by(kappa, replication) %>% summarize(vigor_value=cor(log_n1_n2, log_p1_p2))
# g <- ggplot(cor_stuff, aes(x=kappa, y=vigor_value)) + #geom_boxplot() + 
#   stat_smooth(method="loess",se=FALSE, color="black") +
#   stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1, fun.args=(conf.int=.99), geom="pointrange") +
#   ylab("Log-linear association between log\nresponse ratio and log probability ratio") +
#   xlab("Switch/stay softmax temperature (kappa)") +
#   ggtitle("Probability matching depends\non explore/exploit tradeoff") +
#   theme_bw(base_size=21)
# 
# ex_plot <- ggplot(res_combined %>% filter(kappa==4 & replication==6), aes(x=log_p1_p2, y=log_n1_n2))  + geom_point() + stat_smooth(method="lm") +
#   theme_bw(base_size=21) + 
#   ggtitle("Log response ratios \nvs. log reward probabilities") + xlab("log(p1/p2)") +
#   ylab("log(n1/n2)") + annotate(geom="text", x=-0.5, y=3, label="kappa = 4", size=8, hjust=0)
