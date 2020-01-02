library(ggplot2)
library(reshape2)
library(Rmisc)


rm(list = ls()) 
modelT = "Updated"
fitM = "kappaexponent"
rootD = "c:/Users/zzo1/Dropbox/GitLAb/spott_modeling/spott_modeling/"
sim <- read.csv(sprintf("%sdata/vba_input_%s/stan_population_demo_parameters.csv",rootD, modelT))
est <- read.csv(sprintf("%soutputs/vba_out/ffx/vba_input_%s/suuvid_%s/vba_input_%s_suuvid_%s_ffx_global_statistics.csv", rootD, modelT, fitM, modelT, fitM))
ggDF <- data.frame(sim, est)
alphap <- ggplot(data = ggDF, aes(x = alpha, y = alpha_transformed)) + 
  geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim learning rate, r: %4.2f", cor(ggDF$alpha,ggDF$alpha_transformed))) + ylab("Est learning rate") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) + 
  xlim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) 


gammap <- ggplot(data = ggDF, aes(x = gamma, y = gamma_transformed)) + 
  geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim vigor sensitivity, r: %4.2f", cor(ggDF$gamma,ggDF$gamma_transformed))) + ylab("Est vigor sensitivity") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) + 
  xlim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) 


kappap <- ggplot(data = ggDF, aes(x = kappa, y = kappa_transformed)) + 
  geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim temperature, r: %4.2f", cor(ggDF$kappa,ggDF$kappa_transformed))) + ylab("Est temperature") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$kappa), min(ggDF$kappa_transformed)),max(max(ggDF$kappa), max(ggDF$kappa_transformed))) + 
  xlim(min(min(ggDF$kappa), min(ggDF$kappa_transformed)),max(max(ggDF$kappa), max(ggDF$kappa_transformed))) 


costp <- ggplot(data = ggDF, aes(x = cost, y = cost_transformed)) + 
  geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim cost, r: %4.2f", cor(ggDF$cost,ggDF$cost_transformed))) + ylab("Est cost") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$cost), min(ggDF$cost_transformed)),max(max(ggDF$cost), max(ggDF$cost_transformed))) + 
  xlim(min(min(ggDF$cost), min(ggDF$cost_transformed)),max(max(ggDF$cost), max(ggDF$cost_transformed))) 


betap <- ggplot(data = ggDF, aes(x = beta, y = beta_transformed)) + 
  geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim recovery rate, r: %4.2f", cor(ggDF$beta,ggDF$beta_transformed))) + ylab("Est recovery rate") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$beta), min(ggDF$beta_transformed)),max(max(ggDF$beta), max(ggDF$beta_transformed))) + 
  xlim(min(min(ggDF$beta), min(ggDF$beta_transformed)),max(max(ggDF$beta), max(ggDF$beta_transformed))) 


nup <- ggplot(data = ggDF, aes(x = nu, y = nu_transformed)) + 
  geom_point(size = 2, color =  "deeppink4") +
  xlab(sprintf("Sim basal vigor, r: %4.2f", cor(ggDF$nu,ggDF$nu_transformed))) + ylab("Est basal vigor") +
  geom_abline(intercept = 0) +
  #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
  ylim(min(min(ggDF$nu), min(ggDF$nu_transformed)),max(max(ggDF$nu), max(ggDF$nu_transformed))) + 
  xlim(min(min(ggDF$nu), min(ggDF$nu_transformed)),max(max(ggDF$nu), max(ggDF$nu_transformed))) 


multiplot(alphap, gammap, kappap, costp, betap, nup, cols = 2)

# 
# ##### suuvid minimal
# 
# rm(list = ls()) 
# rootD = "c:/Users/zzo1/Dropbox/GitLAb/spott_modeling/spott_modeling/"
# sim <- read.csv(sprintf("%sdata/vba_input_simulated_fixedkappabetacost20b/stan_population_demo_parameters.csv",rootD))
# est <- read.csv(sprintf("%soutputs/vba_out/ffx/vba_input_simulated_fixedkappabetacost20b/suuvid_minimal/vba_input_simulated_fixedkappabetacost20b_suuvid_minimal_ffx_global_statistics.csv", rootD))
# ggDF <- data.frame(sim, est)
# alphap <- ggplot(data = ggDF, aes(x = alpha, y = alpha_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim learning rate, r: %4.2f", cor(ggDF$alpha,ggDF$alpha_transformed))) + ylab("Est learning rate") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) + 
#   xlim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) 
# 
# 
# gammap <- ggplot(data = ggDF, aes(x = gamma, y = gamma_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim vigor sensitivity, r: %4.2f", cor(ggDF$gamma,ggDF$gamma_transformed))) + ylab("Est vigor sensitivity") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) + 
#   xlim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) 
# 
# #####  suuvid full 20p
# 
# rm(list = ls()) 
# rootD = "c:/Users/zzo1/Dropbox/GitLAb/spott_modeling/spott_modeling/"
# sim <- read.csv(sprintf("%sdata/vba_input_fulllowbeta/stan_population_demo_parameters.csv",rootD))
# est <- read.csv(sprintf("%soutputs/vba_out/ffx/vba_input_simulated_fulllowbeta/suuvid_base/vba_input_simulated_fulllowbeta_suuvid_base_ffx_global_statistics.csv", rootD))
# ggDF <- data.frame(sim, est)
# alphap <- ggplot(data = ggDF, aes(x = alpha, y = alpha_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim learning rate, r: %4.2f", cor(ggDF$alpha,ggDF$alpha_transformed))) + ylab("Est learning rate") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) + 
#   xlim(min(min(ggDF$alpha), min(ggDF$alpha_transformed)),max(max(ggDF$alpha), max(ggDF$alpha_transformed))) 
# 
# 
# gammap <- ggplot(data = ggDF, aes(x = gamma, y = gamma_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim vigor sensitivity, r: %4.2f", cor(ggDF$gamma,ggDF$gamma_transformed))) + ylab("Est vigor sensitivity") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) + 
#   xlim(min(min(ggDF$gamma), min(ggDF$gamma_transformed)),max(max(ggDF$gamma), max(ggDF$gamma_transformed))) 
# 
# 
# kappap <- ggplot(data = ggDF, aes(x = kappa, y = kappa_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim temperature, r: %4.2f", cor(ggDF$kappa,ggDF$kappa_transformed))) + ylab("Est temperature") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$kappa), min(ggDF$kappa_transformed)),max(max(ggDF$kappa), max(ggDF$kappa_transformed))) + 
#   xlim(min(min(ggDF$kappa), min(ggDF$kappa_transformed)),max(max(ggDF$kappa), max(ggDF$kappa_transformed))) 
# 
# 
# costp <- ggplot(data = ggDF, aes(x = cost, y = cost_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim cost, r: %4.2f", cor(ggDF$cost,ggDF$cost_transformed))) + ylab("Est cost") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$cost), min(ggDF$cost_transformed)),max(max(ggDF$cost), max(ggDF$cost_transformed))) + 
#   xlim(min(min(ggDF$cost), min(ggDF$cost_transformed)),max(max(ggDF$cost), max(ggDF$cost_transformed))) 
# 
# 
# betap <- ggplot(data = ggDF, aes(x = beta, y = beta_transformed)) + 
#   geom_point(size = 2, color =  "deeppink4") +
#   xlab(sprintf("Sim recovery rate, r: %4.2f", cor(ggDF$beta,ggDF$beta_transformed))) + ylab("Est recovery rate") +
#   geom_abline(intercept = 0) +
#   #geom_errorbar(aes(ymin=XX, ymax=XX), width=.1, color =  "deeppink3") +
#   ylim(min(min(ggDF$beta), min(ggDF$beta_transformed)),max(max(ggDF$beta), max(ggDF$beta_transformed))) + 
#   xlim(min(min(ggDF$beta), min(ggDF$beta_transformed)),max(max(ggDF$beta), max(ggDF$beta_transformed))) 
# 
# multiplot(alphap, gammap, kappap, costp, betap, cols = 2)
# 
