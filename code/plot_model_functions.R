#simulate performance of model in solving instrumental task
library(tidyverse)
library()
setwd("~/Documents/GitHub/spott_modeling")
source("code/ins_forward_simulation/ins_simulation_functions.R")
source("code/ins_forward_simulation/ins_learning_choice_rules.R")

setwd("~/Documents/GitHub/spott_modeling/code/ins_forward_simulation/Parameter_values/")

#simple plots of model functions under different parameter settings
# gamma_sim <- expand.grid(gamma=c(0.1, 1, 10), Q1=0.5, Q2=0.5, nu=1, beta=1, tau=700, rtlast=seq(25, 650, by=15))
# gamma_sim <- gamma_sim %>% mutate(Q=rowSums(select(., Q1, Q2)))

gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=c(0, 1, 3, 5), nu=-1, beta=200, tau=700, rt_last=seq(25, 600, by=15))
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=tau - rt_last, y=presp, color=factor(Q))) + geom_line(size=1.2) + facet_wrap(~gamma_fac, ncol=1) +
  xlab("Time since last response (ms)") + ylab("p(respond)") +
  ggtitle("p(respond) as a function of Q*, gamma, and time") +
  labs(subtitle="beta = 200, nu = -1") +
  theme_bw(base_size=24) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Total value (Q*)", palette="Dark2") + theme(legend.position="top")

plot(g)
ggsave(g, file="figures/presp_gamma_time_Qstar.pdf", width=8, height=8)



gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=seq(0, 10, by=0.1), nu=-1, beta=50, tau=700, rt_last=25)
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=Q, y=presp)) + geom_line() + facet_wrap(~gamma, ncol=1) +
  xlab("Total value (Q*)") + ylab("p(respond)") +
  ggtitle("Probability of response as a function of Q* and gamma") +
  labs(subtitle="nu = -1, beta = 50, time since last response = 675ms") +
  geom_point(data=data.frame(x=1, y=0.5), aes(x=x, y=y)) + theme_bw(base_size=16)

ggsave(g, file="figures/presp_gamma_Qstar.pdf", width=8, height=6)

gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=1, nu=seq(-2, 2, by=0.05), beta=50, tau=700, rt_last=25)
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=nu, y=presp)) + geom_line() + facet_wrap(~gamma_fac, ncol=1) + xlab("Basal vigor (nu)") + ylab("p(respond)") +
  ggtitle("Probability of response as a function of gamma and nu") +
  labs(subtitle="Q* = 1, beta = 50, time since last response = 675ms") +
  geom_point(data=data.frame(x=-1, y=0.5), aes(x=x, y=y)) + theme_bw(base_size=16)


gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=seq(0, 10, by=0.1), nu=-1, beta=50, tau=700, rt_last=25)
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=Q, y=presp)) + geom_line() + facet_wrap(~gamma, ncol=1) +
  xlab("Total value (Q*)") + ylab("p(respond)") +
  ggtitle("Probability of response as a function of Q* and gamma") +
  labs(subtitle="nu = -1, beta = 50, time since last response = 675ms") +
  geom_point(data=data.frame(x=1, y=0.5), aes(x=x, y=y)) + theme_bw(base_size=16)

ggsave(g, file="figures/presp_gamma_Qstar.pdf", width=8, height=6)

gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=c(0, 1, 3, 5), nu=seq(-2, 2, by=0.05), beta=50, tau=700, rt_last=25)
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=nu, y=presp, color=factor(Q))) + geom_line(size=1.2) + facet_wrap(~gamma_fac, ncol=1) + xlab("Basal vigor (nu)") + ylab("p(respond)") +
  ggtitle("p(respond) as a function of Q*, gamma, and nu") +
  labs(subtitle="beta = 50, time since last response = 675ms") +
  theme_bw(base_size=16) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Total value (Q*)", palette="Dark2") + theme(legend.position="top")

ggsave(g, file="figures/presp_gamma_nu_Qstar.pdf", width=8, height=8)


gamma_sim <- expand.grid(gamma=c(2), Q=c(2), nu=-1, beta=c(50, 100, 200, 400), tau=1700, rt_last=seq(0, 1700, by=15))
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response, gamma_sim)))
gamma_sim$beta_fac <- factor(gamma_sim$beta, levels=unique(gamma_sim$beta), labels=paste0("beta = ", unique(gamma_sim$beta)))
g <- ggplot(gamma_sim, aes(x=tau - rt_last, y=presp, color=factor(beta))) + geom_line(size=1.4) + # facet_wrap(~beta_fac, ncol=1) +
  xlab("Time since last response (ms)") + ylab("p(respond)") +
  ggtitle("p(respond) as a function of beta and time") +
  labs(subtitle="gamma = 2, nu = -1, Q*=2") +
  theme_bw(base_size=16) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Recovery rate (beta)", palette="Set1") + theme(legend.position="top") #+
  #xlim(c(0,100))

ggsave(g, file="figures/presp_beta_time.pdf", width=8, height=8)


# library(cowplot)
# ggdraw(g + theme_bw()) +

## choice rule
#p_switch <- function(Q_c, Q_u, kappa, cost) { 1 / (1 + exp(-1*kappa*(Q_u - cost - Q_c) )) }

sim <- expand.grid(kappa=c(0.4, 2, 5), Q_u=1, Q_c=1, cost=seq(0, 2, by=0.05))
sim$pswitch <- unlist(do.call(Map, c(f=p_switch, sim)))
#sim$beta_fac <- factor(sim$beta, levels=unique(sim$beta), labels=paste0("beta = ", unique(sim$beta)))

g <- ggplot(sim, aes(x=cost, y=pswitch, color=factor(kappa))) + geom_line(size=1.4)+ # facet_wrap(~beta_fac, ncol=1) +
  xlab("Switch cost (a.u.)") + ylab("p(switch)") +
  ggtitle("p(switch) as a function of cost and kappa") +
  labs(subtitle="Q_u=1, Q_c=1") +
  theme_bw(base_size=16) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Softmax temperature (kappa)", palette="Accent") + theme(legend.position="top")

ggsave(g, file="figures/pswitch_kappa_cost.pdf", width=8, height=8)

sim <- expand.grid(kappa=c(2), Q_u=seq(0, 2, by=0.05), Q_c=seq(0, 2, by=0.5), cost=0.5)
sim$pswitch <- unlist(do.call(Map, c(f=p_switch, sim)))
#sim$beta_fac <- factor(sim$beta, levels=unique(sim$beta), labels=paste0("beta = ", unique(sim$beta)))

g <- ggplot(sim, aes(x=Q_u, y=pswitch, color=factor(Q_c))) +
  geom_line(size=1.4)+ # facet_wrap(~beta_fac, ncol=1) +
  xlab("Q unchosen (a.u.)") + ylab("p(switch)") +
  ggtitle("p(switch) as a function of relative Q values") +
  labs(subtitle="kappa=2, cost=0.5") +
  theme_bw(base_size=16) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Q chosen (a.u.)", palette="Accent") + theme(legend.position="top") +
  geom_hline(yintercept=0.5)

ggsave(g, file="figures/pswitch_Qu_Qc.pdf", width=8, height=8)

task_environment <- list(
  #prew=c(0.5,0.3),
  n_trials=60,
  trial_length=6000, #6 seconds
  bin_size=60,
  time_resolution=30
)

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

env <- reshape2::melt(task_environment$prew, varnames=c("Trial", "Response")) %>%
  mutate(Response=factor(Response, levels=c(1,2), labels=c("x", "y")))

g <- ggplot(env, aes(x=Trial, y=value, color=Response)) + geom_line(size=1.4) +
  ylab("p(reward)") + theme_cowplot(font_size=16) + ggtitle("Example random walk contingency")

ggsave(g, file="figures/grwalk_example.pdf", width=10, height=2.5)
