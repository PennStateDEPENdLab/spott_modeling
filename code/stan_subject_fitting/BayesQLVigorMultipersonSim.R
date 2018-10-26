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

setwd(dirname(getSourceEditorContext()$path))
#setwd('~/work/Vigor')

library(tidyverse)
source("ins_simulation_functions.R")
source("ins_learning_choice_rules.R")

nrTrails = 30
P = 10

initial_params <- list(
  value=c(     alpha=0.1,   gamma=2,   nu=-1,   beta=200, cost=0.01, kappa=3), #make it more decisive/exploitative
  lower=c(     alpha=0.001, gamma=0.1, nu=0,   beta=25,  cost=0,    kappa=0.001),
  upper=c(     alpha=0.99,  gamma=100, nu=5,   beta=500, cost=5,    kappa=5),
  par_scale=c( alpha=1e-1,  gamma=1e1, nu=1e0, beta=1e0, cost=1e-1, kappa=1e-1)
)

task_environment <- list(
  #prew=c(0.5,0.3),
  n_trials=nrTrails,
  trial_length=6000, #6 seconds
  bin_size=50
)

#gaussian random walk probabilities
task_environment$prew <- cbind(grwalk(task_environment$n_trials, start=0.5, 0.08), grwalk(task_environment$n_trials, start=0.5, 0.08))

#simulate random uniform numbers that control the environment
#this needs to be constant in optimization so that the cost function is on the same scale across iterations
#random numbers on choices for trials and timesteps. Last dim is p_response, p_switch, outcome (three points at which outputs are probabilistic)
task_environment$n_timesteps <- with(task_environment, trial_length/bin_size)
task_environment$outcomes <- with(task_environment, array(runif(n_timesteps*n_trials*3), dim=c(n_trials, n_timesteps, 3)))

test_stan_sim <- sim_data_for_stan (initial_params$value, task_environment, n=P)

POI     <- c("A","c", "kappa",
             "gamma", 
             "nu",
             "beta")



dataList <- list(
  N   = length(test_stan_sim$id),
  P   = P,
  pInd  = test_stan_sim$id,
  #tInd = test_stan_sim$instrial,
  td  = test_stan_sim$tdiff,
  nreward = test_stan_sim$nreward,
  npress = test_stan_sim$npress,
  nswitch = test_stan_sim$switch,
  key = test_stan_sim$key+1, 
  #MaxTr = MaxTr,
  prevkey = test_stan_sim$curkey
)


modelString = "
data {
int<lower=1> N;
int<lower=1> P; 
 #int<lower=1> MaxTr;
int<lower=1,upper=P> pInd[N];
 #int<lower=1,upper=MaxTr> tInd[N];
vector[N] td;
int<lower=0,upper=1> nreward[N];
int<lower=0,upper=1> npress[N];   
int<lower=0,upper=1> nswitch[N]; 
int<lower=1,upper=3> key[N]; 
int<lower=1,upper=2> prevkey[N]; 
}

transformed data {
vector[2] initV;  // initial values for Qy and Qx
initV = rep_vector(0,2);
}

parameters { 
vector<lower=.01,upper=.99>[P] A; // person-specific learning rate

vector<lower=0.001,upper=5>[P] kappa;  // person-specific inverse temperature 

vector<lower=50,upper=500>[P] beta; // person-specific recovery rates

vector<lower=0,upper=5>[P] c; // person-specific switch cost penalty

vector<lower=0.1,upper=50>[P] gamma; // person-specific vigor sensitivity 

vector<lower=-5,upper=5>[P] nu; // person-specific base vigor -- IN 2PL this can be negative but there this wouldn't make sense, right?
}

model {

real Qy;
real Qx;
real Qc;
real Qu;
real Qstar;
real PE;
real px;
real py;
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

prespond = phitb / (1 + exp(-gamma[pInd[i]]*(Qstar+nu[pInd[i]])) ) ;

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
Qy = Qy + A[pInd[i]]*PE;
 }
 
catProb[1] = 1-px-py;
catProb[2] = px;
catProb[3] = py;

key[i] ~ categorical(catProb);
}
}
"
writeLines(modelString , con = "VigorMultiPerson.stan")

gen_myinits<- function(){list(A = runif(P, .4,.6),
                               kappa = runif(P, 0.2,0.4),
                               c = runif(P,0.2,0.4),
                               gamma = runif(P,0.2,0.4),
                               nu = runif(P,0.2,0.4))}
list1 <-   gen_myinits()
list2 <-   gen_myinits()

customInits <- list(list1, list2)

twoarmBandit <-
  stan(file = "VigorMultiPerson.stan",
    data = dataList,
    thin = 1,
    warmup = 300,
    iter = 500,
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

# save.image(sprintf("VigorMultiPerson%s.Rdata", Sys.Date()))

learningRateMean =  resulttable[grep("^A", rownames(resulttable)),1]
inverseTemperatureMean =  resulttable[grep("^kappa", rownames(resulttable)),1]
switchCostpenaltyMean =  resulttable[grep("^c", rownames(resulttable)),1]
vigorSensitivity =  resulttable[grep("^gamma", rownames(resulttable)),1]
baseVigor =  resulttable[grep("^nu", rownames(resulttable)),1]
recoveryRate =  resulttable[grep("^beta", rownames(resulttable)),1]

personSpecificParameters = data.frame(learningRateMean,inverseTemperatureMean,switchCostpenaltyMean,vigorSensitivity,baseVigor,recoveryRate)
ggpairs(personSpecificParameters)


# learningRateMode =  resulttable[grep("^A", rownames(resulttable)),3]
# inverseTemperatureMode =  resulttable[grep("^tau", rownames(resulttable)),3]
