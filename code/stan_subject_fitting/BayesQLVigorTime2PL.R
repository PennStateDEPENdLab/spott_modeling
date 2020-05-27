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

graphics.off()
#rm(list=ls(all=TRUE))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd(dirname(getSourceEditorContext()$path))
#setwd('~/work/Vigor')
test_stan_sim <- read.csv("../../data/stan_population_demo_trialdata.csv.gz")

test_stan_sim = test_stan_sim[test_stan_sim$id %in% seq(1:20),]

#data transformation to undo curkey clairvoyance and setup accurate active_action variable
#active_action should contain the key that has been pressed most recently in *previous* time bins
#this is adapted code from suuvid_get_data.m. I believe I've now undone the clairvoyance in the simulation
#code, where curkey was being provided to the model before the first press, so much of this will not be
#needed long-term.
by_subj <- split(test_stan_sim, test_stan_sim$id)
by_subj <- lapply(by_subj, function(ss) {
  #right shift reinforcement
  new_trial = c(1, diff(ss$instrial))
  choice = ss$key
  reinforcement = ss$nreward
  
  #remove the curkey clairvoyance for now: generates a ramping up of predicted probability for first chosen action in a trial
  zero_curkey=1
  for (i in 1:length(new_trial)) {
    if (choice[i] > 0) {
      zero_curkey = 0 #disable zeroing now that a key has been pressed  
    } else if (new_trial[i] == 1) {
      zero_curkey = 1 #on a new trial, enable zeroing out
      #if (i > 1) { ss$curkey[i-1] = 0 } #also zero out preceding timestep because we double-lag curkey. This will ensure a 0 on a new trial  
    }
    
    if (zero_curkey == 1) {
      ss$curkey[i] = 0
    }
  }
  
  #convert curkey to represent the currently active key entering this trial (row), *before*
  #the subject has made a choice. That way, curkey can be used in sticky softmax
  #to determine whether the subject will stay with the most recently pressed button
  ss <- ss %>% group_by(instrial) %>% 
    mutate(curkey=dplyr::lag(curkey, default=0)) %>% ungroup()
  
  return(ss)
})

#looks correct
#by_subj[[1]] %>% select(instrial, latency_bin, key, curkey, npress, nreward, bincenter, tdiff) %>% View()

#put back together as one data.frame
test_stan_sim <- do.call(rbind, by_subj)

P = 20

POI <- c("A",
         "omega",  "meanOmega", "sdOmega",
         "kappa", "meanKappa", "sdKappa",
         "gamma", "meanGamma", "sdGamma",
         "nu", "sdNu")

dataList <- list(
  N   = length(test_stan_sim$id),
  P   = P,
  pInd  = test_stan_sim$id,
  #tInd = test_stan_sim$instrial,
  td  = test_stan_sim$tdiff/1000, #scale down to seconds for parameter scaling
  nreward = test_stan_sim$nreward,
  npress = test_stan_sim$npress,
  #nswitch = test_stan_sim$switch, #deprecated in new model
  key = test_stan_sim$key + 1, #move to 2 = left, 3 = right 
  #MaxTr = MaxTr,
  active_action = test_stan_sim$curkey
)

modelString = "
data {
  int<lower=1> N;
  int<lower=1> P; 
  int<lower=1,upper=P> pInd[N];
  vector[N] td;
  int<lower=0,upper=1> nreward[N];
  int<lower=0,upper=1> npress[N];   
  int<lower=1,upper=3> key[N]; 
  int<lower=0,upper=2> active_action[N]; // active_action is the most recently pressed key (0 if no key yet pressed in trial)
}

transformed data {
  vector[2] initV;  // initial values for Qy and Qx
  initV = rep_vector(0,2); // initialize to zero
}

parameters { 
  vector<lower=0.01,upper=.99>[P] A; // person-specific learning rate 
  //real meanlogitA; // population mean learning rate
  //real<lower=0> sdlogitA; // population sd learning rate

  vector<lower=0.001,upper=20>[P] kappa;  // person-specific inverse temperature 
  real<lower=0.001,upper=20> meanKappa; // population mean inverse temperature 
  real<lower=0> sdKappa; // population sd inverse temperature 

  vector<lower=0,upper=4>[P] omega; // person-specific stickiness
  real<lower=0,upper=4> meanOmega; // population mean stickiness
  real<lower=0> sdOmega; // population sd stickiness

  vector<lower=0.01,upper=5>[P] gamma; // person-specific vigor sensitivity 
  real<lower=0.01,upper=5> meanGamma; // population mean vigor sensitivity 
  real<lower=0> sdGamma; // population sd vigor sensitivity 

  vector<lower=0.01,upper=.5>[P] nu; // person-specific basal vigor
  //real<lower=0.01,upper=.5> meanNu; // population mean basal vigor 
  real<lower=0> sdNu; // population sd basal vigor
}

model {

  real Qy;
  real Qx;
  real Qstar;
  real PE;
  real px;
  real py;
  real p_respond;
  vector[3] catProb;
  vector[2] cc; // 0/1 vector indicating
  vector[2] m; //value + sticky sum that enters softmax
  vector[2] Qcur;
  vector[2] p_which;

  Qy = initV[1];
  Qx = initV[2];

  for (i in 1:N) {        
    // respond or not
    Qstar = Qy + Qx; 
  
    p_respond = 1/(1 + exp( -gamma[pInd[i]]*Qstar * (td[i] - nu[pInd[i]] ) ));
  
    npress[i] ~ bernoulli(p_respond);
    
    cc = initV;
    if (active_action[i] > 0) { cc[active_action[i]] = 1; }
    Qcur[1] = Qx;
    Qcur[2] = Qy;
  
    //sticky softmax implementation
    m = kappa[pInd[i]]*Qcur + omega[pInd[i]]*cc; //m is 2x1 and contains the weighted value + stickiness
    m = m - max(m); // rescale for avoiding floating point overflow
    p_which = exp(m)/sum(exp(m));
  
    catProb[1] = 1 - p_respond; //no response
    catProb[2] = p_which[1] * p_respond;
    catProb[3] = p_which[2] * p_respond;
  
    if (key[i] == 2) { 
      PE = nreward[i] - Qx; 
      Qx = Qx + A[pInd[i]]*PE; 
    }
  
    if (key[i] == 3) { 
      PE = nreward[i] - Qy;
      Qy = Qy + A[pInd[i]]*(nreward[i] - Qy);
    }
  
    key[i] ~ categorical(catProb);
  }

  // priors
  kappa ~ normal(meanKappa, sdKappa);
  omega ~ normal(meanOmega, sdOmega);
  gamma ~ normal(meanGamma, sdGamma);
  nu ~ normal(.15, sdNu);
}
"
writeLines(modelString , con = "VigorHierarchical.stan")

gen_myinits <- function() { 
  list(A = runif(P, 0.2,0.8),
       kappa = runif(P, 0.2,0.4),
       omega = runif(P,0.2,0.4), 
       gamma = runif(P,1,3),
       nu = runif(P,0.01,0.11))
}

list1 <-   gen_myinits()
list2 <-   gen_myinits()

customInits <- list(list1, list2)

twoarmBandit <-
  stan(file = "VigorHierarchical.stan",
       data = dataList,
       thin = 1,
       warmup = 500,
       iter = 1000,
       chains = 2,
       init = customInits,
       pars = POI
  )


codaSamples = mcmc.list(lapply(1:ncol(twoarmBandit) , function(x) {
  mcmc(as.array(twoarmBandit)[, x, ])
}))

source("posteriorSummaryStats.R")

resulttable <- summarizePost(codaSamples)
saveNonConverged <- resulttable[resulttable$RHAT>1.1,]
if (nrow(saveNonConverged) == 0){
  print("Convergence criterion was met for every parameter.")
} else{ 
  print("Not converged parameter(s):")
  show(saveNonConverged)
}

saveLowESS <- resulttable[resulttable$ESS<200,]
if (nrow(saveLowESS) == 0){
  print("ESS is higher than 200 for every parameter.")
} else{ 
  print("Parameters with low ESS:")
  show(saveLowESS)
}

save.image(sprintf("VigorHierarchicalMultilevel%s16.Rdata", Sys.Date()))



learningRateMean =  resulttable[grep("^A", rownames(resulttable)),1]
inverseTemperatureMean =  resulttable[grep("^kappa", rownames(resulttable)),1]
switchCostpenaltyMean =  resulttable[grep("^omega", rownames(resulttable)),1]
vigorSensitivityMean =  resulttable[grep("^gamma", rownames(resulttable)),1]
baseVigorMean =  resulttable[grep("^nu", rownames(resulttable)),1]

simLearningRate = aggregate(test_stan_sim$alpha, by=list(test_stan_sim$id), FUN=mean)[2]
simInverseTemperature = aggregate(test_stan_sim$kappa, by=list(test_stan_sim$id), FUN=mean)[2]
simSwitchCostPenalty = aggregate(test_stan_sim$cost, by=list(test_stan_sim$id), FUN=mean)[2]
simVigorSensitivity = aggregate(test_stan_sim$gamma, by=list(test_stan_sim$id), FUN=mean)[2]
simBaseVigor = aggregate(test_stan_sim$nu, by=list(test_stan_sim$id), FUN=mean)[2]
simRecoveryRate = aggregate(test_stan_sim$beta, by=list(test_stan_sim$id), FUN=mean)[2]

names(simLearningRate) = "simLearningRate"
names(simInverseTemperature) = "simInverseTemperature"
names(simSwitchCostPenalty) = "simSwitchCostPenalty"
names(simVigorSensitivity) = "simVigorSensitivity"
names(simBaseVigor) = "simBaseVigor"
names(simRecoveryRate) = "simRecoveryRate"

personSpecificParameters = data.frame(learningRateMean,simLearningRate,simInverseTemperature,inverseTemperatureMean,
                                      switchCostpenaltyMean,simSwitchCostPenalty,
                                      vigorSensitivityMean, simVigorSensitivity,
                                      baseVigorMean, simBaseVigor,
                                      simRecoveryRate)

# ggpairs(personSpecificParameters)

png(filename="SimEstPlotMultilevel21.png")

a = ggplot(personSpecificParameters,aes(simLearningRate, learningRateMean)) + geom_point() +
  ylim(0, 0.7) + xlim(0, 0.7) +
  geom_abline(intercept = 0) +
  ylim(min(min(simLearningRate), min(learningRateMean)),max(max(simLearningRate), max(learningRateMean))) + 
  xlim(min(min(simLearningRate), min(learningRateMean)),max(max(simLearningRate), max(learningRateMean))) + 
  labs(title = sprintf("Est-sim corr: %2.2f",cor(learningRateMean, simLearningRate)), y = "Est learning rate",  x = "Sim learning rate")

b = ggplot(personSpecificParameters,aes(simInverseTemperature,inverseTemperatureMean)) +
  geom_abline(intercept = 0) +    
  geom_point() +
  ylim(min(min(simInverseTemperature), min(inverseTemperatureMean)),max(max(simInverseTemperature), max(inverseTemperatureMean))) + 
  xlim(min(min(simInverseTemperature), min(inverseTemperatureMean)),max(max(simInverseTemperature), max(inverseTemperatureMean))) + 
  labs(title = sprintf("Est-sim corr: %2.2f",cor(inverseTemperatureMean,simInverseTemperature)), y = "Est inv temp",  x = "Sim inv temp")

c = ggplot(personSpecificParameters,aes(simSwitchCostPenalty, switchCostpenaltyMean)) + geom_point() +
  geom_abline(intercept = 0) +
  ylim(min(min(simSwitchCostPenalty), min(switchCostpenaltyMean)),max(max(simSwitchCostPenalty), max(switchCostpenaltyMean))) + 
  xlim(min(min(simSwitchCostPenalty), min(switchCostpenaltyMean)),max(max(simSwitchCostPenalty), max(switchCostpenaltyMean))) + 
  labs(title = sprintf("Est-sim corr: %2.2f",cor(switchCostpenaltyMean, simSwitchCostPenalty)), y = "Est switch cost",  x = "Sim switch cost")

d = ggplot(personSpecificParameters,aes(simVigorSensitivity, vigorSensitivityMean)) + geom_point() +
  geom_abline(intercept = 0) +
  ylim(min(min(simVigorSensitivity), min(vigorSensitivityMean)),max(max(simVigorSensitivity), max(vigorSensitivityMean))) + 
  xlim(min(min(simVigorSensitivity), min(vigorSensitivityMean)),max(max(simVigorSensitivity), max(vigorSensitivityMean))) + 
  labs(title = sprintf("Est-sim corr: %2.2f",cor(vigorSensitivityMean, simVigorSensitivity)), y = "Est vigor sens",  x = "Sim vigor sens")


f = ggplot(personSpecificParameters,aes(simBaseVigor,baseVigorMean)) + geom_point() +
  geom_abline(intercept = 0) +
  ylim(min(min(simBaseVigor), min(baseVigorMean)),max(max(simBaseVigor), max(baseVigorMean))) + 
  xlim(min(min(simBaseVigor), min(baseVigorMean)),max(max(simBaseVigor), max(baseVigorMean))) + 
  labs(title = sprintf("Est-sim corr: %2.2f",cor(baseVigorMean, simBaseVigor)), y = "Est base vigor",  x = "Sim base vigor")


multiplot(a,d, f, c,b,cols = 2)

dev.off()

