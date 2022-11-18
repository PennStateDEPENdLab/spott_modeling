# Plots "a" values in matching as gamma or nu changes

library(dplyr)
library(ggplot2)
library(ggpmisc)
library(tidyr)

repo_dir <- "~/Documents/GitHub/spott_modeling/code/ins_forward_simulation"
# repo_dir <- "/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/ins_forward_simulation"
setwd(repo_dir)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

initial_params <-c(alpha=0.1259690, gamma=3.2626238, nu=0.5724897, omega=3.4277531, kappa = 2.1928352) 
#initial_params <- c(alpha=0.1, gamma=2, nu=0.5, omega=0, kappa = 2)

# example environment with 2 actions that start at .7 and .3 reward probability, with .08 step size
task_environment <- setup_task_environment(
  prew = list(
    expression(grwalk(n_trials, start = 0.7, 0.08)),
    expression(grwalk(n_trials, start = 0.3, 0.08))
  ),
  n_trials = 100,
  model = "exp" #model # note that the $model element can be edited and then passed back into a simulation function
)

library(doMC)
registerDoMC(6)

# Simulate with gamma_nu grid ---------------------------------------------


# gamma_nu <- expand.grid(gamma = c(1, 1.5, 2, 2.5),
#                         nu = c(0.25, 0.5, 0.75, 1))
# 
# res <- foreach(i=1:nrow(gamma_nu)) %dopar% {
#   pars <- initial_params #$value
#   pars["gamma"] <- gamma_nu[i,1]
#   pars["nu"] <- gamma_nu[i,2]
#   xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
#   sdf <- xx$sum_df
#   sdf$gamma <- gamma_nu[i,1]
#   sdf$nu <- gamma_nu[i,2]
#   return(sdf)
# }
# 
# save(res, file = "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data/matching_sim_GammaNu.RData")
# 
# output_dir <- "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/ins_forward_simulation/figures"
# setwd(output_dir)
# 
# res_combined <- bind_rows(res)
# 
# cor_stuff <- res_combined %>% group_by(gamma, nu, replication) %>% summarize(vigor_value=cor(avg_value, nresp))
# 
# ex_plot <- ggplot(res_combined %>% filter(gamma==2 & nu ==0.5 & replication==6), aes(x=avg_value, y=nresp))  + geom_point() + stat_smooth(method="lm") +
#   theme_bw(base_size=21) + ggtitle("Association between total value\nand response vigor") + xlab("Total value of actions within a trial (a.u.)") +
#   ylab("Number of responses per trial")
# 
# ## Plot gamma
# log_lm_4gamma <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(gamma) %>% do({
#   df <- .
#   df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
#   m <- lm(log_n1_n2 ~ log_p1_p2, df)
#   broom::tidy(m)
# }) %>% ungroup()
# 
# log_lm_4gamma2 <- log_lm_4gamma %>% pivot_wider(names_from = term, values_from = estimate)
# plot(log_lm_4gamma2$gamma, log_lm_4gamma2$log_p1_p2)
# 
# ## Plot nu
# log_lm_4nu <- res_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(nu) %>% do({
#   df <- .
#   df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
#   m <- lm(log_n1_n2 ~ log_p1_p2, df)
#   broom::tidy(m)
# }) %>% ungroup()
# 
# log_lm_4nu2 <- log_lm_4nu %>% pivot_wider(names_from = term, values_from = estimate)
# plot(log_lm_4nu2$nu, log_lm_4nu2$log_p1_p2)


# Simulate along gamma ----------------------------------------------------

## gamma
gammas <- 1.1^(seq(from=-30, to=25, length.out=16))
res_gamma <- foreach(i=seq_along(gammas)) %dopar% {
  pars <- initial_params #$value
  pars["gamma"] <- gammas[i]
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$gamma <- gammas[i]
  return(sdf)
}

save(res_gamma, file = "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data/matching_sim_Gamma.RData")

res_combined_gamma <- bind_rows(res_gamma)
cor_stuff_gamma <- res_combined_gamma %>% group_by(gamma, replication) %>% summarize(vigor_value=cor(avg_value, nresp))

log_lm_gamma <- res_combined_gamma %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(gamma) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
  m <- lm(log_n1_n2 ~ log_p1_p2, df)
  broom::tidy(m)
}) %>% ungroup()

log_lm_gamma2 <- log_lm_gamma %>% pivot_wider(names_from = term, values_from = estimate)
plot(log_lm_gamma2$gamma, log_lm_gamma2$log_p1_p2)


# Simulate along nu -------------------------------------------------------

## nu
nus <- seq(0.05, 2.5, by=0.2) 
res_nu <- foreach(i=seq_along(nus)) %dopar% {
  pars <- initial_params #$value
  pars["nu"] <- nus[i]
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$nu <- nus[i]
  return(sdf)
}

save(res_nu, file = "~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data/matching_sim_Nu.RData")

res_combined_nu <- bind_rows(res_nu)
cor_stuff_nu <- res_combined_nu %>% group_by(nu, replication) %>% summarize(vigor_value=cor(avg_value, nresp))

log_lm_nu <- res_combined_nu %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(nu) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
  m <- lm(log_n1_n2 ~ log_p1_p2, df)
  broom::tidy(m)
}) %>% ungroup()

log_lm_nu2 <- log_lm_nu %>% pivot_wider(names_from = term, values_from = estimate)
plot(log_lm_nu2$nu, log_lm_nu2$log_p1_p2)


# Simulate along omega ----------------------------------------------------

## omega
omegas <- seq(-2, 5, by=0.5)
res_omega <- foreach(i=seq_along(omegas)) %dopar% {
  pars <- initial_params #$value
  pars["omega"] <- omegas[i]
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$omega <- omegas[i]
  return(sdf)
}

save(res_omega, file = "~/Documents/GitHub/spott_modeling/data/matching_sim_Omega.RData")

res_combined_omega <- bind_rows(res_omega)
cor_stuff_omega <- res_combined_omega %>% group_by(omega, replication) %>% summarize(vigor_value=cor(avg_value, nresp))

log_lm_omega <- res_combined_omega %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(omega) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)# %>% mutate_all(scale)
  m <- lm(log_n1_n2 ~ log_p1_p2, df)
  broom::tidy(m)
}) %>% ungroup()

log_lm_omega2 <- log_lm_omega %>% pivot_wider(names_from = term, values_from = estimate)
#plot(log_lm_omega2$omega, log_lm_omega2$log_p1_p2)
g <- ggplot(log_lm_omega2, aes(x=omega, y=log_p1_p2)) + 
  geom_point() +
  ylab("a") + 
  ggtitle("Change of a values as omega varies") +
  labs(subtitle="Other parameters were recovered from PANDAA: \n alpha=0.1259690, gamma=3.2626238, nu=0.5724897, kappa = 2.1928352")
plot(g)


# Investigating the quadratic shape a vs. omega plot ----------------------

## Plotting p1/p2 by r1/r2 for different omega values (10/24/22 meeting)

# not logged
g <- ggplot(res_combined_omega, aes(y=p1_p2, x=n1_n2)) +
  geom_point() +
  facet_wrap(~omega, ncol = 3)

# logged  
g_log <- ggplot(res_combined_omega, aes(x=y=log_p1_p2, log_n1_n2)) +
  geom_point() +
  facet_wrap(~omega, ncol = 3)

## Plotting a at combinations of kappa and omega (10/24/22 meeting)

kappa_omega <- expand.grid(kappa = seq(1, 6, by = 1),
                         omega = seq(0, 5, by=0.5))

res_kappa_omega <- foreach(i=1:nrow(kappa_omega )) %dopar% {
  pars <- initial_params #$value
  pars["kappa"] <- kappa_omega [i,1]
  pars["omega"] <- kappa_omega [i,2]
  xx <- repeat_forward_simulation(pars, task_environment) # default is n=100
  sdf <- xx$sum_df
  sdf$kappa <- kappa_omega [i,1]
  sdf$omega <- kappa_omega [i,2]
  return(sdf)
}

save(res_kappa_omega, file = "~/Documents/GitHub/spott_modeling/data/matching_sim_OmegaKappa.RData")

res_kappa_omega_combined <- bind_rows(res_kappa_omega)
log_lm_KappaOmega <- res_kappa_omega_combined %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf) %>% group_by(kappa, omega) %>% do({
  df <- .
  df <- df %>% select(log_n1_n2, log_p1_p2)
  m <- lm(log_n1_n2 ~ log_p1_p2, df)
  broom::tidy(m)
}) %>% ungroup()

# a values at different kappa-omega combinations
g <- ggplot(log_lm_KappaOmega %>% filter(term == "log_p1_p2"), aes(x = kappa, y = omega, color = estimate))+
  geom_tile() ## geom_tile()

# a vs. omega at different kappa values
g <- ggplot(log_lm_KappaOmega %>% filter(term == "log_p1_p2"), aes(x = omega, y = estimate)) +
  geom_point(aes(shape = as.factor(kappa))) +
  ylab("a") 
  #facet_wrap(~kappa, ncol = 2)

# a vs. kappa at different omega values
g <- ggplot(log_lm_KappaOmega %>% filter(term == "log_p1_p2"), aes(x = kappa, y = estimate)) +
  geom_point() +
  ylab("a") + 
  facet_wrap(~omega, ncol = 3)

# Putting the facets in the plot above together
g <- ggplot(log_lm_KappaOmega %>% filter(term == "log_p1_p2"), aes(x = kappa, y = estimate)) +
  geom_point(aes(color = omega)) +
  ylab("a")

# plot a at different omega/kappa ratios; facets are omega values
log_lm_KappaOmega <- log_lm_KappaOmega %>% mutate(omega_kappa = omega/kappa)
g <- ggplot(log_lm_KappaOmega %>% filter(term == "log_p1_p2"), aes(x = omega_kappa, y = estimate)) +
  geom_point() +
  ylab("a") +
  facet_wrap(~omega, ncol = 3)

# plot log_n1_n2 vs. log_p1_p2 along different combinations of kappa/omega
g_log2 <- ggplot(res_kappa_omega_combined, aes(x=log_p1_p2, y=log_n1_n2)) +
  stat_poly_line()+
  geom_point(alpha = 0.1) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  facet_grid(kappa~omega)

plot(g_log2)
