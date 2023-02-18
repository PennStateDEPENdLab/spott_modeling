#simulate performance of model in solving instrumental task
library(tidyverse)
library()
setwd("~/Documents/GitHub/spott_modeling")
source("code/ins_forward_simulation/ins_simulation_functions.R")
source("code/ins_forward_simulation/ins_learning_choice_rules.R")

setwd("~/Documents/GitHub/spott_modeling/code/ins_forward_simulation/Parameter_values/")

# gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=c(0, 1, 3, 5), nu=-1, tau=700, rt_last=seq(25, 600, by=15))
# gamma_sim <- expand.grid(gamma=c(1.1^(seq(from=-30, to=25, length.out=20))), Q=c(0, 1, 3, 5), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))
# gamma_sim <- expand.grid(gamma=c(0.4, 2, 5), Q=c(0, 1, 3, 5), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))

gamma_sim <- expand.grid(gamma=c(0.05, 0.3, 0.5, 3, 5, 10), Q=c(0, 1, 3, 5), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))
gamma_sim <- expand.grid(gamma=c(1.1^(seq(from=-20, to=11, length.out=10))), Q=c(0, 1, 3, 5), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))
gamma_sim <- expand.grid(gamma=c((seq(from=0.2, to=1, by=0.2))), Q=c(0, 1, 3, 5), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))


gamma_sim$presp <- unlist(do.call(Map, c(f=p_response_exp, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=tau - rt_last, y=presp, color=factor(Q))) + geom_line(size=1.2) + 
  facet_grid(gamma_fac ~ nu) +
  #facet_wrap(~gamma_fac, ncol=1) +
  xlab("Time since last response (ms)") + ylab("p(respond)") +
  ggtitle("p(respond) as a function of Q*, gamma, nu, and time") +
  #labs(subtitle="beta = 200, nu = -1") +
  #theme_bw(base_size=24) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Total value (Q*)", palette="Dark2") + theme(legend.position="top")

plot(g)



gamma_sim$presp <- unlist(do.call(Map, c(f=p_response_exp, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=Q, y=presp)) + geom_line() + 
  facet_grid(gamma_fac ~ nu) +
  #facet_wrap(~gamma, ncol=1) +
  xlab("Total value (Q*)") + ylab("p(respond)") +
  ggtitle("Probability of response as a function of Q* and gamma") +
  #labs(subtitle="nu = -1, beta = 50, time since last response = 675ms") +
  labs(subtitle="time since last response = 675ms") +
  geom_point(data=data.frame(x=1, y=0.5), aes(x=x, y=y)) #+ theme_bw(base_size=16)

plot(g)

gamma_sim <- expand.grid(gamma=c((seq(from=0.2, to=1, by=0.2))), Q=1, nu=seq(-2, 2, by=0.05),  tau=700, rt_last=seq(25, 600, by=15))
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response_exp, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=nu, y=presp)) + geom_line() + facet_wrap(~gamma_fac, ncol=1) + xlab("Basal vigor (nu)") + ylab("p(respond)") +
  ggtitle("Probability of response as a function of gamma and nu") +
  labs(subtitle="Q* = 1, time since last response = 675ms") +
  geom_point(data=data.frame(x=-1, y=0.5), aes(x=x, y=y)) #+ theme_bw(base_size=16)

plot(g)

gamma_sim <- expand.grid(gamma=c((seq(from=0.2, to=1, by=0.2))), Q=seq(0, 10, by=0.1), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response_exp, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=Q, y=presp)) + geom_line() + 
  facet_grid(gamma_fac ~ nu) +
  #facet_wrap(~gamma, ncol=1) +
  xlab("Total value (Q*)") + ylab("p(respond)") +
  ggtitle("Probability of response as a function of Q* and gamma") +
  labs(subtitle="time since last response = 675ms") +
  geom_point(data=data.frame(x=1, y=0.5), aes(x=x, y=y)) #+ theme_bw(base_size=16)
plot(g)

gamma_sim <- expand.grid(gamma=c((seq(from=0.2, to=1, by=0.2))), Q=c(0, 1, 3, 5), nu=c(0.25, 0.5, 1, 1.5, 2, 2.5),  tau=700, rt_last=seq(25, 600, by=15))
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response_exp, gamma_sim)))
gamma_sim$gamma_fac <- factor(gamma_sim$gamma, levels=unique(gamma_sim$gamma), labels=paste0("gamma = ", unique(gamma_sim$gamma)))
g <- ggplot(gamma_sim, aes(x=nu, y=presp, color=factor(Q))) + geom_line(size=1.2) + facet_wrap(~gamma_fac, ncol=1) + xlab("Basal vigor (nu)") + ylab("p(respond)") +
  ggtitle("p(respond) as a function of Q*, gamma, and nu") +
  labs(subtitle="time since last response = 675ms") +
  #theme_bw(base_size=16) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Total value (Q*)", palette="Dark2") + theme(legend.position="top")
plot(g)


