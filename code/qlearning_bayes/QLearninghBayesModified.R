library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(gridExtra)
library(reshape)
library(rstan)
library(truncnorm)
library(Rmisc)
library(coda)
library(rstudioapi)

graphics.off()
rm(list=ls(all=TRUE))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ncore = 2

current_path <- getSourceEditorContext()$path
setwd(dirname(current_path))

source("qlearning_sim_functions.R")


#setwd('~/work/QLearning')

## simulate Q learning agent at different levels of alpha and beta (LR and temperature)
set.seed(1050)
nsubjects <- 50
ntrials <- 150

sim_dataset <- sim_qlearning_group(nsubjects=nsubjects, ntrials=ntrials, 
                                   preward=c(0.75, 0.25), #two-armed bandit with 90% for one option, 30% for the other
                                   alpha=list(meana = 0.4, mina = 0.01, maxa = 0.9, sda = 0.2),
                                   beta=list(meanb = 1, minb = 0.1, maxb = 10, sdb = 1), #this is inverse temp
                                   #beta=list(fixed=1), #this is inverse temp
                                   quiet=FALSE, seed=1050)

choicesT = melt(t(sim_dataset$choices))
rewardsT = melt(t(sim_dataset$rewards))

dataIns = data.frame(choicesT$X2, choicesT$value, rewardsT$value)
dataIns = na.omit(dataIns)
colnames(dataIns) = c("subjID", "choice", "outcome")

nrSubj = length(unique(dataIns$subjID))

write.table(dataIns, "dataIns.txt", sep = "\t", row.names = F)

###################################################
# Vanilla Two-Arm Bandit from BayesHDM  ###########
###################################################

### Some inputs their functions need ######
data = "dataIns.txt"
inits = "random"
indPars = "mean"

if (file.exists(data)) {
  rawdata <- read.table( data, header = T, sep="\t")
} else {
  stop("** The data file does not exist. Please check it again. **\n  e.g., data = '/MyFolder/SubFolder/dataFile.txt', ... **\n")
}  
# Remove rows containing NAs
NA_rows_all = which(is.na(rawdata), arr.ind = T)  # rows with NAs
NA_rows = unique(NA_rows_all[, "row"])
if (length(NA_rows) > 0) {
  rawdata = rawdata[-NA_rows, ]
  cat("The number of rows with NAs=", length(NA_rows), ". They are removed prior to modeling the data. \n", sep="")
}

# Individual Subjects
subjList <- unique(rawdata[,"subjID"])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects

# Specify the number of parameters and parameters of interest 
numPars <- 2
POI     <- c("mu_A", "mu_tau", "mu_p",
             "sigma",
             "A", "tau")

#  I took out "log_lik" frm monitoring for now as there are lots of NANs in Rhat, probably because of small sample size

modelName <- "bandit2arm_delta"

# Information for user
cat("\nModel name = ", modelName, "\n")
cat("Data file  = ", data, "\n")


################################################################################
# THE DATA.  
###################################################################

Tsubj <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject

for ( i in 1:numSubjs )  {
  curSubj  <- subjList[ i ]
  Tsubj[i] <- sum( rawdata$subjID == curSubj )  # Tsubj[N]
}

# Setting maxTrials
maxTrials <- max(Tsubj)

# Information for user continued
cat(" # of (max) trials per subject = ", maxTrials, "\n\n")

choice  <- array(1, c(numSubjs, maxTrials) )
outcome <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  tmp          <- subset(rawdata, rawdata$subjID == curSubj)
  choice[i, 1:useTrials] <- tmp$choice
  outcome[i, 1:useTrials] <- tmp$outcome
}

dataList <- list(
  N        = numSubjs,
  T        = maxTrials,
  Tsubj    = Tsubj,
  choice   = choice,
  outcome  = outcome,
  numPars  = numPars
)

# inits
if (inits[1] != "random") {
  if (inits[1] == "fixed") {
    inits_fixed <- c(0.5, 1.0)
  } else {
    if (length(inits)==numPars) {
      inits_fixed <- inits
    } else {
      stop("Check your inital values!")
    }
  }  
  genInitList <- function() {
    list(
      mu_p   = c( qnorm(inits_fixed[1]), qnorm(inits_fixed[2] / 5) ),
      sigma  = c(1.0, 1.0),
      A_pr   = rep(qnorm(inits_fixed[1]), numSubjs),
      tau_pr = rep(qnorm(inits_fixed[2]/5), numSubjs)
    )
  }
} else {
  genInitList <- "random"
}
if (ncore > 1) {
  numCores <- parallel::detectCores()
  if (numCores < ncore){
    options(mc.cores = numCores)
    warning('Number of cores specified for parallel computing greater than number of locally available cores. Using all locally available cores.')
  }
  else{
    options(mc.cores = ncore)
  }
} else {
  options(mc.cores = 1)
}


modelString = "
data {
  int<lower=1> N;
int<lower=1> T;               
int<lower=1,upper=T> Tsubj[N];                 
int<lower=1,upper=2> choice[N,T];     
real outcome[N,T];  // no lower and upper bounds   
}

transformed data {
vector[2] initV;  // initial values for EV
initV = rep_vector(0.0, 2);
}

parameters {
// Declare all parameters as vectors for vectorizing
// Hyper(group)-parameters  
vector[2] mu_p;  
vector<lower=0>[2] sigma;

// Subject-level parameters 
vector<lower=0,upper=1>[N] A; // learning rate
vector<lower=0,upper=7>[N] tau;  // inverse temperature
}

model {
// Hyperparameters
mu_p  ~ normal(0, 1); 
sigma ~ cauchy(0, 5);  

for (i in 1:N) {
A[i] ~ normal(mu_p[1],sigma[1]);
tau[i] ~ normal(mu_p[2],sigma[2]);
}

// subject loop 
for (i in 1:N) {
vector[2] ev; // expected value
real PE;      // prediction error

ev = initV;

//  trial loop
for (t in 1:(Tsubj[i])) {        
// compute action probabilities
choice[i,t] ~ categorical_logit( tau[i] * ev ); // categorical distribution, with the parameters on the logit scale

// prediction error 
PE = outcome[i,t] - ev[choice[i,t]];

// value updating (learning) 
ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
}
}
}

generated quantities {
// For group level parameters
real<lower=0,upper=1> mu_A;
real<lower=0,upper=5> mu_tau;

mu_A   = Phi_approx(mu_p[1]);
mu_tau = Phi_approx(mu_p[2]) * 5;
}

"

writeLines(modelString , con = "bandit2arm_deltaZ2.stan")

twoarmBandit <-
  stan(
    file = 'bandit2arm_deltaZ2.stan',
    data = dataList,
    thin = 1,
    warmup = 1000,
    iter = 3000,
    chains = 2,
    init = genInitList,
    pars = POI
  )

source("postcalc.R")
codaSamples = mcmc.list(lapply(1:ncol(twoarmBandit) , function(x) {
  mcmc(as.array(twoarmBandit)[, x, ])
}))

resulttable <- zcalc(codaSamples)
saveNonConverged <- resulttable[resulttable$RHAT>1.1,]
if (nrow(saveNonConverged) == 0){
  print("Convergence criterion was met for every parameter.")
} else{ 
  print("Not converged parameter(s):")
  show(saveNonConverged)
}


learningRateMean =  resulttable[grep("^A", rownames(resulttable)),1]
learningRateMode =  resulttable[grep("^A", rownames(resulttable)),3]

inverseTemperatureMean =  resulttable[grep("^tau", rownames(resulttable)),1]
inverseTemperatureMode =  resulttable[grep("^tau", rownames(resulttable)),3]

Results = data.frame(learningRateMean,learningRateMode,inverseTemperatureMean,inverseTemperatureMode)

cat("Simulated (empirical) values and means and SDs of person mean estimates \n")
alphaM = mean(sim_dataset$alpha_subj)
alphaSD = sd(sim_dataset$alpha_subj)
alphaestM = mean(learningRateMean)
alphaestSD = sd(learningRateMean)

betaM = mean(sim_dataset$beta_subj)
betaSD = sd(sim_dataset$beta_subj)
betaestM = mean(inverseTemperatureMean)
betaestSD = sd(inverseTemperatureMean)

dfSum = data.frame(alphaM,alphaestM,alphaSD,alphaestSD,betaM,betaestM,betaSD,betaestSD)
show(dfSum)

cat("Posterior stats on variables of interest \n")
show(zcalc(codaSamples, filters = c("mu_A", "mu_tau","mu_p","sigma")))

save.image(sprintf("alldata%sTNnophi.Rdata", Sys.Date()))

# load("alldata2017-12-13TNnophi200500.Rdata")
# 
# df = data.frame(learningRateMean,inverseTemperatureMean)
# 
# ggplot(df,aes(learningRateMean, inverseTemperatureMean)) + geom_point()


png(filename="SimEstPlots.png", res=300, width=8, height=8, units="in")

df = data.frame(learningRateMean, inverseTemperatureMean, alpha=sim_dataset$alpha_subj, beta=sim_dataset$beta_subj)
a = ggplot(df,aes(learningRateMean, alpha)) + geom_point() + 
  #ylim(0, 0.7) + xlim(0, 0.7) +
  labs(title = sprintf("Est-sim corr: %2.2f",cor(learningRateMean, df$alpha)), x = "Est learning rate",  y = "Sim learning rate") 

b = ggplot(df,aes(inverseTemperatureMean, beta)) + geom_point() + 
  #ylim(minb, maxb) + xlim (minb, maxb) +
  labs(title = sprintf("Est-sim corr: %2.2f",cor(inverseTemperatureMean, df$beta)), x = "Est inv temp",  y = "Sim inv temp") 
  #labs(title = sprintf("Est-sim corr: %2.2f", robust::covRob(cbind(inverseTemperatureMean, df$beta), corr = TRUE)), x = "Est inv temp",  y = "Sim inv temp") 

multiplot(a,b, cols = 1)

dev.off()

#test ML estimation using fitQ function

initialPars <- c(0.1, 2) #alpha, beta
lb <- c(0.005, 0.04) #lower bound
ub <- c(0.8, 10) #upper bound
parscale <- c(.1, 1) #rough step sizes for parameters 

parmat <- matrix(NA, nrow=length(sim_dataset$slist), ncol=2)
for (s in 1:length(sim_dataset$slist)) {
  rvec <- sim_dataset$rewards[s,]
  choices <- sim_dataset$choices[s,]
  
  optResult <- nlminb(start=initialPars, objective=fitQ, outcomes=rvec, choices=choices,
                      scale=1/parscale, lower=lb, upper=ub, control=list(eval.max=500, iter.max=500))
  
  #fitdf <- fitQ(optResult$par, outcomes=rvec, choices=choices, optim=FALSE)
  parmat[s,] <- optResult$par
  
}

cor(sim_dataset$alpha_subj, parmat[,1])
cor(sim_dataset$beta_subj, parmat[,2])

plot(sim_dataset$alpha_subj, parmat[,1])
plot(sim_dataset$beta_subj, parmat[,2])

cor(parmat)

#compare against vanilla code
library(hBayesDM)

vv <- bandit2arm_delta(data="dataIns.txt")

cor(sim_dataset$alpha_subj, vv$allIndPars$A)
plot(sim_dataset$alpha_subj, vv$allIndPars$A)
cor(sim_dataset$beta_subj, vv$allIndPars$tau)
plot(sim_dataset$beta_subj, vv$allIndPars$tau)

# omat <- get_omat(500, c(0.9, 0.1))
# testcase <- simsubject(.1, .5, omat)
# rvec <- apply(testcase$R, 1, function(row) { row[which(!is.na(row))] })
# choices <- testcase$choices
# 
# optResult <- nlminb(start=initialPars, objective=fitQ, outcomes=rvec, choices=choices,
#                     scale=1/parscale, lower=lb, upper=ub, control=list(eval.max=500, iter.max=500))
# 
# optResult$par
# fitdf <- fitQ(optResult$par, outcomes=rvec, choices=choices, optim=FALSE)