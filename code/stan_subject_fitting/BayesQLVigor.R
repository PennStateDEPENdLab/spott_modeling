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
rm(list=ls(all=TRUE))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ncore = 6

# current_path <- getSourceEditorContext()$path
# setwd(dirname(current_path))
setwd('~/work/Vigor')

newdata = read.csv("allins_binned_50_long.csv")

nrTrialsbyPerson <- ddply(newdata, .(id), summarize,  trialNR=length(unique(instrial)))

MaxTr <- max(nrTrialsbyPerson$trialNR) 

# something is weird with id = 80 -- instrial number 17 is missing
  for (i in 18:25){
    newdata$instrial[newdata$instrial== i & newdata$id == 80] = i-1
    }

newdata$newID <- cumsum(c(TRUE,with(newdata, id[-1]!= id[-length(id)]))) 

N = length(newdata$newID)
P = length(unique(newdata$newID))

POI     <- c("A", "c", "kappa","gamma","nu", "Qy", "Qx", "py", "px")

dataList <- list(
  N   = N,
  P   = P,
  pInd  = newdata$newID,
  tInd = newdata$instrial,
  td  = newdata$tdiff,
  nreward = newdata$nreward,
  npress = newdata$npress,
  nswitch = newdata$switch,
  key = newdata$key+1, 
  MaxTr = MaxTr,
  prevkey = newdata$curkey
)


modelString = "
data {
int<lower=1> N;
int<lower=1> P; 
int<lower=1> MaxTr;
int<lower=1,upper=P> pInd[N];
int<lower=1,upper=MaxTr> tInd[N];
vector[N] td;
int<lower=0,upper=1> nreward[N];
int<lower=0,upper=1> npress[N];   
int<lower=0,upper=1> nswitch[N]; 
int<lower=1,upper=3> key[N]; 
int<lower=1,upper=2> prevkey[N]; 
}

transformed data {
vector[2] initV;  // initial values for Qy and Qx
initV = rep_vector(0.0, 2);
}

parameters { 
vector<lower=0,upper=1>[P] A; // person-specific learning rate 
vector<lower=0,upper=20>[P] kappa;  // person-specific inverse temperature 
vector<lower=30,upper=300>[P] beta; // person-specific recovery rates
// matrix<lower=0,upper=1>[P,MaxTr] c; // person and trial specific switch cost penalty
vector<lower=0,upper=1>[P] c; // person and trial specific switch cost penalty
vector<lower=0,upper=10>[P] gamma; // person-specific vigor sensitivity 
vector<lower=0,upper=10>[P] nu; // person-specific basel vigor -- IN 2PL this can be negative but there this wouldn't make sense, right?
}

model {

# real Qy;
# real Qx;
real Qc;
real Qu;
real Qstar;
real PE;
# real px;
# real py;
real phitb;
real prespond;
real pswitch;
vector[3] catProb;

// add tracking Qy, Qx, py, px + add population level 
Qy = initV[1];
Qx = initV[2];

for (i in 1:N) {        
// respond or not
phitb = 1-exp(-1*td[i]/beta[pInd[i]]);
Qstar = Qy + Qx; 

prespond = phitb / (1 + exp(-gamma[pInd[i]]*(Qstar-nu[pInd[i]])) ) ;

npress[i] ~ bernoulli(prespond); 

if (prevkey[i] == 1) {  // previous button press, curkey in raw data
Qc = Qx;
Qu = Qy;
} else {
Qc = Qy;
Qu = Qx;
}

pswitch = 1/(1+exp(-1*kappa[pInd[i]]*(Qu-c[pInd[i]]-Qc)));

nswitch[i] ~ bernoulli(pswitch); 

if (prevkey[i] == 1) { 
px = (1-pswitch)*prespond;
py = pswitch*prespond;
 } else {
py = (1-pswitch)*prespond;
px = pswitch*prespond;
}


if (key[i] == 2) { 
PE = nreward[i] - Qx; 
Qx = Qx + A[pInd[i]]*PE; 
}

if (key[i] == 3) { 
PE = nreward[i] - Qy;
Qy = Qy + A[pInd[i]]*(nreward[i] - Qy);
 }
 

catProb[1] = 1-px-py;
catProb[2] = px;
catProb[3] = py;

key[i] ~ categorical(catProb);
}
}
"
writeLines(modelString , con = "Vigor.stan")

gen_myinits<- function(){list(A = runif(P, 0.2,0.4),
                               kappa = runif(P, 0.2,0.4),
                               c = runif(P,0.2,0.4),
                               gamma = runif(P,0.2,0.4),
                               nu = runif(P,0.2,0.4))}
list1 <-   gen_myinits()
list2 <-   gen_myinits()
list3 <-   gen_myinits()

customInits <- list(list1, list2, list3)

twoarmBandit <-
  stan(file = "Vigor.stan",
    data = dataList,
    thin = 1,
    warmup = 500,
    iter = 1000,
    chains = 3,
    init = customInits,
    pars = POI
  )


codaSamples = mcmc.list(lapply(1:ncol(twoarmBandit) , function(x) {
  mcmc(as.array(twoarmBandit)[, x, ])
}))

save.image(sprintf("VigorIM%s.Rdata", Sys.Date()))

source("posteriorSummaryStats.R")

resulttable <- summarizePost(codaSamples)
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

save.image(sprintf("Vigor%s.Rdata", Sys.Date()))
# 
# load("Vigor2018-05-07.Rdata")
# 
# learningRateMean =  resulttable[grep("^A", rownames(resulttable)),1]
# inverseTemperatureMean =  resulttable[grep("^kappa", rownames(resulttable)),1]
# switchCostpenaltyMean =  resulttable[grep("^c", rownames(resulttable)),1]
# vigorSensitivity =  resulttable[grep("^gamma", rownames(resulttable)),1]
# baseVigor =  resulttable[grep("^nu", rownames(resulttable)),1]
# 
# personSpecificParameters = data.frame(learningRateMean,inverseTemperatureMean,vigorSensitivity,baseVigor)
# 
# ggpairs(personSpecificParameters)
# 
# switchCostpenaltyPersonbyTrial = data.frame(switchCostpenaltyMean, rep(seq(1:26),24))
# colnames(switchCostpenaltyPersonbyTrial) = c("switchCostPenalty", "personID")
# 
# ggplot(data = switchCostpenaltyPersonbyTrial, aes(switchCostPenalty)) + geom_histogram() + facet_wrap("personID")
# A = unlist(codaSamples[, "^A"])

# original code

# dataList <- list(
#   N        = numSubjs,
#   T        = max(pushByPerson),
#   Tsubj    = Tsubj,
#   choice   = choice,
#   outcome  = outcome
# )
# 
# 
# modelString = "
# data {
# int<lower=1> N;
# int<lower=1> T;               
# int<lower=1,upper=T> Tsubj[N];                 
# int<lower=1,upper=2> choice[N,T];     
# real outcome[N,T];  # no lower and upper bounds   
# }
# 
# transformed data {
# vector[2] initV;  # initial values for EV
# initV = rep_vector(0.0, 2);
# }
# 
# parameters {
# # Declare all parameters as vectors for vectorizing
# # Hyper(group)-parameters  
# vector[2] mu_p;  
# vector<lower=0>[2] sigma;
# 
# # Subject-level raw parameters (for Matt trick)
# vector[N] A_pr;    # learning rate
# vector[N] tau_pr;  # inverse temperature
# }
# 
# transformed parameters {
# # subject-level parameters
# vector<lower=0,upper=1>[N] A;
# vector<lower=0,upper=5>[N] tau;
# 
# for (i in 1:N) {
# A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
# tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i] ) * 5;
# }
# }
# 
# model {
# # Hyperparameters
# mu_p  ~ normal(0, 1); 
# sigma ~ cauchy(0, 5);  
# 
# # individual parameters
# A_pr   ~ normal(0,1);
# tau_pr ~ normal(0,1);
# 
# # subject loop and trial loop
# for (i in 1:N) {
# vector[2] ev; # expected value
# real PE;      # prediction error
# 
# ev = initV;
# 
# for (t in 1:(Tsubj[i])) {        
# # compute action probabilities
# choice[i,t] ~ categorical_logit( tau[i] * ev );
# 
# # prediction error 
# PE = outcome[i,t] - ev[choice[i,t]];
# 
# # value updating (learning) 
# ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
# }
# }
# }
# 
# generated quantities {
# # For group level parameters
# real<lower=0,upper=1> mu_A; 
# real<lower=0,upper=5> mu_tau;
# 
# # For log likelihood calculation
# real log_lik[N]; 
# 
# mu_A   = Phi_approx(mu_p[1]);
# mu_tau = Phi_approx(mu_p[2]) * 5;
# 
# { # local section, this saves time and space
# for (i in 1:N) {
# vector[2] ev; # expected value
# real PE;      # prediction error
# 
# # Initialize values
# ev = initV;
# 
# log_lik[i] = 0;
# 
# for (t in 1:(Tsubj[i])) {
# # compute action probabilities
# log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tau[i] * ev);
# 
# # prediction error 
# PE = outcome[i,t] - ev[choice[i,t]];
# 
# # value updating (learning) 
# ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
# }
# }   
# }
# }
# "




# 
# learningRateMean =  resulttable[grep("^A", rownames(resulttable)),1]
# learningRateMode =  resulttable[grep("^A", rownames(resulttable)),3]
# 
# inverseTemperatureMean =  resulttable[grep("^tau", rownames(resulttable)),1]
# inverseTemperatureMode =  resulttable[grep("^tau", rownames(resulttable)),3]
# 
# Results = data.frame(learningRateMean,learningRateMode,inverseTemperatureMean,inverseTemperatureMode)
# 
# cat("Simulated (empirical) values and means and SDs of person mean estimates \n")
# alphaM = mean(alpha)
# alphaSD = sd(alpha)
# alphaestM = mean(learningRateMean)
# alphaestSD = sd(learningRateMean)
# 
# betaM = mean(1/beta)
# betaSD = sd(1/beta)
# betaestM = mean(inverseTemperatureMean)
# betaestSD = sd(inverseTemperatureMean)
# 
# dfSum = data.frame(alphaM,alphaestM,alphaSD,alphaestSD,betaM,betaestM,betaSD,betaestSD)
# show(dfSum)
# 
# cat("Posterior stats on variables of interest \n")
# show(zcalc(codaSamples, filters = c("mu_A", "mu_tau","mu_p","sigma")))
# 
# save.image(sprintf("alldata%sTNnophi.Rdata", Sys.Date()))
# 
# # load("alldata2017-12-13TNnophi200500.Rdata")
# # 
# # df = data.frame(learningRateMean,inverseTemperatureMean)
# # 
# # ggplot(df,aes(learningRateMean, inverseTemperatureMean)) + geom_point()
# 
# 
# png(filename="SimEstPlots.png")
# 
# df = data.frame(learningRateMean, inverseTemperatureMean, alpha, 1/beta)
# a = ggplot(df,aes(learningRateMean, alpha)) + geom_point() + 
#   ylim(0, 0.7) + xlim(0, 0.7) +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(learningRateMean, alpha)), x = "Est learning rate",  y = "Sim learning rate") 
# 
# b = ggplot(df,aes(inverseTemperatureMean, 1/beta)) + geom_point() + 
#   ylim(minb, maxb) + xlim (minb, maxb) +
#   labs(title = sprintf("Est-sim corr: %2.2f",cor(inverseTemperatureMean, 1/beta)), x = "Est inv temp",  y = "Sim inv temp") 
# 
# multiplot(a,b, cols = 2)
# 
# dev.off()
# 
# 
# # 
# # for (ii in 1:numPush){
# #   if (Trials[ii] == 1){ 
# #     newInsData[ii] <-  1  }
# #   if (Trials[ii] < Trials[ii+1]){ 
# #     newInsData[ii+1] <-  1  }
# #   if (Trials[ii] == Trials[ii+1]){
# #     
# #     newInsData[ii+1] <-  
# #   }
# #   
# # }
# 
# # 
# # if (ncore > 1) {
# #   numCores <- parallel::detectCores()
# #   if (numCores < ncore){
# #     options(mc.cores = numCores)
# #     warning('Number of cores specified for parallel computing greater than number of locally available cores. Using all locally available cores.')
# #   }
# #   else{
# #     options(mc.cores = ncore)
# #   }
# # } else {
# #   options(mc.cores = 1)
# # }



# parVals <- rstan::extract(twoarmBandit, permuted=T)
# 
# A   <- parVals$A
# tau  <- parVals$tau
# 
# 
# for (i in 1:numSubjs) {
#   if (indPars=="mean") {
#     allIndPars[i, ] <- c( mean(A[, i]), 
#                           mean(tau[, i]) )
#   } else if (indPars=="median") {
#     allIndPars[i, ] <- c( median(A[, i]), 
#                           median(tau[, i]) )
#   } else if (indPars=="mode") {
#     allIndPars[i, ] <- c( estimate_mode(A[, i]),
#                           estimate_mode(tau[, i]) )
#   }
# }
# 
# allIndPars           <- cbind(allIndPars, subjList)
# colnames(allIndPars) <- c("A", 
#                           "tau", 
#                           "subjID")


# Individual parameters (e.g., individual posterior means)
# allIndPars <- array(NA, c(numSubjs, numPars))
# allIndPars <- as.data.frame(allIndPars)