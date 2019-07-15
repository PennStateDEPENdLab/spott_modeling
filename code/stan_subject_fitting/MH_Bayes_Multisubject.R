library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(gridExtra)
library(reshape)
library(rstan)
library(truncnorm)
library(Rmisc)
library(rstudioapi)
library(coda)
library(GGally)
library(tidyverse)

graphics.off()
rm(list=ls(all=TRUE))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#setwd(dirname(getSourceEditorContext()$path))
setwd("~/Data_Analysis/spott_modeling")

nrIter   = 400
nrWarmUp = 300
nrChains = 2

test_stan_sim = read.csv("data/stan_population_demo_trialdata_nomiss.csv.gz")
#test_stan_sim = test_stan_sim[test_stan_sim$id %in% seq(1:21),]

#P = 21
#P = length(unique(test_stan_sim$id))
P = 50

pars <- c("alpha", "kappa", "c", "beta", "gamma", "nu")

MaxTr <- max(test_stan_sim$instrial)


dataList <- list(
  N   = length(test_stan_sim$id),
  P   = P,
  pInd  = test_stan_sim$id,
  td  = test_stan_sim$tdiff,
  nreward = test_stan_sim$nreward,
  npress = test_stan_sim$npress,
  nswitch = test_stan_sim$switch,
  action = test_stan_sim$key + 1, #no action = 1, left = 2, right = 3
  prevkey = test_stan_sim$curkey
  #MaxTr=MaxTr
)


gen_myinits<- function() {
  list(alpha = runif(P, .4,.6),
       kappa = runif(P, 0.2,0.4),
       c = runif(P,0.2,0.4),
       beta = runif(P, 30, 500),
       gamma = runif(P,0.2,0.4),
       nu = runif(P,0.2,0.4))
}

customInits <- list(gen_myinits(), 
                    gen_myinits(), 
                    gen_myinits(), 
                    gen_myinits())


VigorModeling <-
  stan(file = "code/stan_subject_fitting/qlvigor1.stan",
       data   = dataList,
       thin   = 1,
       warmup = nrWarmUp,
       iter   = nrIter,
       chains = nrChains,
       init   = customInits,
       pars   = pars,
       control = list(adapt_delta = 0.99999),
       verbose=TRUE
  )


codaSamples = mcmc.list(lapply(1:ncol(VigorModeling) , function(x) {
  mcmc(as.array(VigorModeling)[, x, ])
}))

#load("VigorMultiPersonTrackingIM2018-11-20.Rdata")

source("postcalc.R")

resulttable <- zcalc(codaSamples)
saveNonConverged <- resulttable[resulttable$RHAT>1.1,]
if (nrow(saveNonConverged) == 0){
  print("Convergence criterion was met for every parameter.")
}else{ 
  print("Not converged parameter(s):")
  show(saveNonConverged)
}

saveLowESS <- resulttable[resulttable$ESS<1000,]
if (nrow(saveLowESS) == 0){
  print("ESS is higher than 1000 for every parameter.")
}else{ 
  print("Parameters with low ESS:")
  show(saveLowESS)
}


N = length(test_stan_sim$id)
I = (nrIter - nrWarmUp)*nrChains

AllQx = array(rep(NA, N*I), dim = c(N,I)) 
AllQy = array(rep(NA, N*I), dim = c(N,I)) 


for (nn in 1:N){
  AllQx[nn, ] = unlist(codaSamples[,sprintf("Qx_track[%i]",nn)])
  AllQy[nn, ] = unlist(codaSamples[,sprintf("Qy_track[%i]",nn)])
}



# learningRateMean =  resulttable[grep("^A", rownames(resulttable)),1]
# inverseTemperatureMean =  resulttable[grep("^kappa", rownames(resulttable)),1]
# switchCostpenaltyMean =  resulttable[grep("^c", rownames(resulttable)),1]
# vigorSensitivityMean =  resulttable[grep("^gamma", rownames(resulttable)),1]
# baseVigorMean =  resulttable[grep("^nu", rownames(resulttable)),1]
# recoveryRateMean =  resulttable[grep("^beta", rownames(resulttable)),1]
# 
# learningRateMean = learningRateMean[-15]
# inverseTemperatureMean= inverseTemperatureMean[-15]
# switchCostpenaltyMean= switchCostpenaltyMean[-15]
# vigorSensitivityMean = vigorSensitivityMean[-15]
# baseVigorMean = baseVigorMean[-15]
# recoveryRateMean = recoveryRateMean[-15]
# 
# 
# simLearningRate = aggregate(test_stan_sim$alpha, by=list(test_stan_sim$id), FUN=mean)[2]
# simInverseTemperature = aggregate(test_stan_sim$kappa, by=list(test_stan_sim$id), FUN=mean)[2]
# simSwitchCostPenalty = aggregate(test_stan_sim$cost, by=list(test_stan_sim$id), FUN=mean)[2]
# simVigorSensitivity = aggregate(test_stan_sim$gamma, by=list(test_stan_sim$id), FUN=mean)[2]
# simBaseVigor = aggregate(test_stan_sim$nu, by=list(test_stan_sim$id), FUN=mean)[2]
# simRecoveryRate = aggregate(test_stan_sim$beta, by=list(test_stan_sim$id), FUN=mean)[2]
# 
# names(simLearningRate) = "simLearningRate"
# names(simInverseTemperature) = "simInverseTemperature"
# names(simSwitchCostPenalty) = "simSwitchCostPenalty"
# names(simVigorSensitivity) = "simVigorSensitivity"
# names(simBaseVigor) = "simBaseVigor"
# names(simRecoveryRate) = "simRecoveryRate"
# 
# personSpecificParameters = data.frame(learningRateMean,simLearningRate,simInverseTemperature,inverseTemperatureMean,
#                                       switchCostpenaltyMean,simSwitchCostPenalty,
#                                       vigorSensitivityMean, simVigorSensitivity,
#                                       baseVigorMean, simBaseVigor,
#                                       recoveryRateMean, simRecoveryRate)
# 
# # ggpairs(personSpecificParameters)
# 
# png(filename="SimEstPlot21QT.png")
# 
# a = ggplot(personSpecificParameters,aes(learningRateMean, simLearningRate)) + geom_point() +
#   ylim(0, 0.7) + xlim(0, 0.7) +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(learningRateMean, simLearningRate)), x = "Est learning rate",  y = "Sim learning rate")
# 
# b = ggplot(personSpecificParameters,aes(inverseTemperatureMean, simInverseTemperature)) + geom_point() +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(inverseTemperatureMean,simInverseTemperature)), x = "Est inv temp",  y = "Sim inv temp")
# 
# c = ggplot(personSpecificParameters,aes(switchCostpenaltyMean, simSwitchCostPenalty)) + geom_point() +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(switchCostpenaltyMean, simSwitchCostPenalty)), x = "Est switch cost",  y = "Sim switch cost")
# 
# d = ggplot(personSpecificParameters,aes(vigorSensitivityMean, simVigorSensitivity)) + geom_point() +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(vigorSensitivityMean, simVigorSensitivity)), x = "Est vigor sens",  y = "Sim vigor sens")
# 
# e = ggplot(personSpecificParameters,aes(recoveryRateMean, simRecoveryRate)) + geom_point() +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(recoveryRateMean, simRecoveryRate)), x = "Est recov rate",  y = "Sim recov rate")
# 
# f = ggplot(personSpecificParameters,aes(baseVigorMean, simBaseVigor)) + geom_point() +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(baseVigorMean, simBaseVigor)), x = "Est base vigor",  y = "Sim base vigor")
# 
# 
# multiplot(a,b,c,d,e,f, cols = 3)
# 
# dev.off()

save.image(sprintf("VigorHierarchicalPersonTracking%s.Rdata", Sys.Date()))



