library(ggplot2)

# setwd("~/Documents/GitHub/spott_modeling/data")
setwd("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data")
load("ggDF.RData")

alphap <- ggplot(data = ggDF, aes(x = alpha, y = alpha_transformed, color=as.factor(kappa_mean))) +
  geom_point() +
  xlab(sprintf("Sim learning rate (alpha), r: %4.2f", cor(ggDF$alpha,ggDF$alpha_transformed))) + ylab("Est learning rate") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) +
  xlim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) +
  facet_grid(nu_mean~gamma_mean)

plot(alphap)

gammap <- ggplot(data = ggDF, aes(x = gamma, y = gamma_transformed, color=as.factor(kappa_mean))) + 
  geom_point() +
  #geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim vigor sensitivity (gamma), r: %4.2f", cor(ggDF$gamma,ggDF$gamma.1))) + ylab("Est vigor sensitivity") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) + 
  xlim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) +
  facet_grid(nu_mean~alpha_mean)

plot(gammap)


nup <- ggplot(data = ggDF, aes(x = nu, y = nu_transformed, color=as.factor(kappa_mean))) +
  geom_point() +
  #geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim basal vigor (nu), r: %4.2f", cor(ggDF$nu,ggDF$nu_transformed))) + ylab("Est basal vigor") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$nu), min(ggDF$nu_transformed)),max(max(ggDF$nu), max(ggDF$nu_transformed))) + 
  xlim(min(min(ggDF$nu), min(ggDF$nu_transformed)),max(max(ggDF$nu), max(ggDF$nu_transformed))) +
  facet_grid(gamma_mean~alpha_mean)

plot(nup)