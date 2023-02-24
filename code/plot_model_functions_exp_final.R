# This script examines behaviors of the choice rules at different parameter values
# The plots inform which parameter values are likely recoverable for the model
# The selected parameter values are used in parameter recovery

library(tidyverse)
library()

setwd("~/Documents/GitHub/spott_modeling")
source("code/ins_forward_simulation/ins_simulation_functions.R")
source("code/ins_forward_simulation/ins_learning_choice_rules.R")

setwd("~/Documents/GitHub/spott_modeling/figures/ins_forward_simulation/Parameter_values/")

# alpha <- seq(0.1, 0.8, 0.1)
# gamma <- 1.1^(seq(from=-16, to=15, length.out=10))
# nu <- seq(0, 2, 0.25)
# kappa <- seq(2, 6, 1)
# omega <- seq(0, 4, 0.5)
# 
# Q <- c(1, 1.5, 2)
# tau <- 700
# rt_last <- seq(25, 600, by=15) # rt_last <- 25


# gamma, nu ---------------------------------------------------------------

gamma_sim <- expand.grid(gamma=1.1^(seq(from=-16, to=15, length.out=10)), Q=c(1, 1.5, 2), nu=seq(0, 2, 0.25),  tau=700, rt_last=seq(25, 600, by=15))
gamma_sim$presp <- unlist(do.call(Map, c(f=p_response_exp, gamma_sim)))
gamma_sim$gamma_fac <- round(gamma_sim$gamma, 2)
g <- ggplot(gamma_sim, aes(x=tau - rt_last, y=presp, color=factor(Q))) + geom_line(size=1.2) + 
  facet_grid(gamma_fac ~ nu) +
  xlab("Time since last response (ms)") + ylab("p(respond)") +
  ggtitle("p(respond) as a function of Q*, gamma, nu, and time") +
  theme_bw(base_size=12) + theme(panel.grid.minor = element_blank()) +
  scale_color_brewer("Total value (Q*)", palette="Dark2") + theme(legend.position="top")

ggsave(g, file="presp_gamma_nu_time_Qstar.pdf", width=10, height=12)



# kappa, omega ------------------------------------------------------------

kappa_omega_q <- expand.grid(kappa = seq(2, 6, by = 1),
                             omega = seq(0, 4, by = 0.5), 
                             q1=seq(0.2, 0.8, by=0.1), q2=seq(0.2, 0.8, by=0.1), 
                             cur_action=1)


for(i in 1:nrow(kappa_omega_q)){
  koq <- kappa_omega_q[i,]
  these_ps <- p_sticky_softmax(c(koq$q1, koq$q2), 
                               koq$cur_action,
                               kappa =  koq$kappa,
                               omega = koq$omega
  )
  kappa_omega_q$p1[i] <- these_ps[1]
  kappa_omega_q$p2[i] <- these_ps[2]
  
}

kappa_omega_q <- kappa_omega_q %>% mutate(q_diff = q1-q2)

g <- ggplot(kappa_omega_q, aes(x=q_diff, y=p1)) +
  geom_point() +
  geom_line() + 
  facet_grid(kappa~omega) +
  xlab("relative Q values between options") + ylab("p(choose option 1)") +
  ggtitle("p(choose option 1) as a function of relative Q values") +
  labs(subtitle = "current choise set to option 1") + 
  #theme_bw(base_size=12) + 
  theme(axis.text = element_text(size = 8)) + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept=0.5)

ggsave(g, file="pwhich_kappa_omega_RelQ.pdf", width=12, height=12)
