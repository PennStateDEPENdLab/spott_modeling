library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(gridExtra)
library(reshape)
library(rstan)
library(truncnorm)

graphics.off()
rm(list=ls(all=TRUE))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ncore = 2

#setwd('~/work/QLearning')

## simulate Q learning agent at different levels of alpha and beta (LR and temperature)
set.seed(1050)
nsubjects <- 10
ntrials <- 10

meana <- 0.2
mina <- 0.06
maxa <- 0.5
sda <- 0.06
#alpha <- rnorm(nsubjects, meana, sda)
#enforce constraints on reasonable alpha values
#alpha[alpha < mina] <- mina
#alpha[alpha > maxa] <- maxa
#hist(alpha)

alpha <-  rtruncnorm(nsubjects, a=mina, b=maxa, mean = meana, sd = sda)

minb <- 0.1
maxb <- 7
#beta <- rgamma(nsubjects, shape=4, scale=0.3)
#beta[beta < minb] <- minb
#beta[beta > maxb] <- maxb
shape=4
scale=0.3
beta <-  rtruncnorm(nsubjects, a=minb, b=maxb, mean = shape*scale, sd = sqrt(shape*scale^2))

summary(beta)

#master reward probs for contingency
preward <- c(0.7, 0.3)

get_omat <- function(ntrials, preward) {
  outcomes <- sapply(preward, function(o) { rbinom(ntrials, size=1, prob=o) })  
}

softmax <- function(Q, beta) {
  ( exp((Q - max(Q))/beta) ) / (sum( exp((Q - max(Q))/beta) )) #Divide by temperature
}

simsubject <- function(alpha, beta, outcomes) {
  Q <- matrix(NA, nrow=nrow(outcomes)+1, ncol=ncol(outcomes))
  R <- matrix(NA, nrow=nrow(outcomes), ncol=ncol(outcomes))
  choices <- rep(NA, nrow(outcomes))
  Q[1,] <- 0 #initialize no expectation of reward
  ovec <- 1:ncol(outcomes) #vector of possible choices
  for (i in 1:ntrials) {
    #softmax choice
    choice <- choices[i] <- sample(x=ncol(outcomes), size=1, prob=softmax(Q[i,], beta))
    
    #harvest outcome
    R[i,choice] <- outcomes[i,choice]
    
    #learn from it
    Q[i+1,choice] <- Q[i,choice] + alpha*(R[i,choice] - Q[i,choice])
    
    #carry forward other values
    Q[i+1, ovec[!ovec == choice]] <- Q[i, ovec[!ovec == choice]]
  }
  
  return(list(Q=Q, R=R, choices=choices, alpha=alpha, beta=beta, outcomes=outcomes))
}

slist <- list()
for (s in 1:nsubjects) {
  omat <- get_omat(ntrials, preward)
  slist[[s]] <- simsubject(alpha[s], beta[s], omat)
  slist[[s]]$rvec <- apply(slist[[s]]$R, 1, function(row) { row[which(!is.na(row))] })
}

rewards <- do.call(rbind, lapply(slist, function(s) { s$rvec } ))
choices <- do.call(rbind, lapply(slist, function(s) { s$choices } ))

alphas <- do.call(rbind, lapply(slist, function(s) { s$alpha } ))
betas <- do.call(rbind, lapply(slist, function(s) { s$beta } ))

choicesT = melt(t(choices))
rewardsT = melt(t(rewards))

dataIns = data.frame(choicesT$X2, choicesT$value, rewardsT$value)
dataIns = na.omit(dataIns)
colnames(dataIns) = c("subjID", "choice", "outcome")

nrSubj = length(unique(dataIns$subjID))

write.table(dataIns, "dataIns.txt", sep = "\t", row.names = F) ## Following the original format for data

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
POI     <- c("mu_A", "mu_tau", 
             "sigma",
             "A", "tau")

#  I took out "log_lik" frm monitoring for now as there are lots of NANs in Rhat, probably because of small sample size

modelName <- "bandit2arm_delta"

# Information for user
cat("\nModel name = ", modelName, "\n")
cat("Data file  = ", data, "\n")


################################################################################
# THE DATA.  ###################################################################

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
real outcome[N,T];  # no lower and upper bounds   
}

transformed data {
vector[2] initV;  # initial values for EV
initV = rep_vector(0.0, 2);
}

parameters {
# Declare all parameters as vectors for vectorizing
# Hyper(group)-parameters  
vector[2] mu_p;  
vector<lower=0>[2] sigma;

# Subject-level raw parameters (for Matt trick)
vector[N] A_pr;    # learning rate
vector[N] tau_pr;  # inverse temperature
}

transformed parameters {
# subject-level parameters
vector<lower=0,upper=1>[N] A;
vector<lower=0,upper=5>[N] tau;

for (i in 1:N) {
# Level-2 learning rate and inverse temperature distributions
#A fast approximation to the cumulative unit normal distribution function phi is implemented
#sin Stan as the function Phi_approx.
A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );# with approx to the unit normal cdf 
tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i] ) * 5; # with scaled version of the above
}
}

model {
# Hyperparameters
mu_p  ~ normal(0, 1); 
sigma ~ cauchy(0, 5);  

# individual parameters
A_pr   ~ normal(0,1);
tau_pr ~ normal(0,1);

# subject loop 
for (i in 1:N) {
vector[2] ev; # expected value
real PE;      # prediction error

ev = initV;

#  trial loop
for (t in 1:(Tsubj[i])) {        
# compute action probabilities
choice[i,t] ~ categorical_logit( tau[i] * ev ); # categorical distribution, with the parameters on the logit scale

# prediction error 
PE = outcome[i,t] - ev[choice[i,t]];

# value updating (learning) 
ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
}
}
}

generated quantities {
# For group level parameters
real<lower=0,upper=1> mu_A;
real<lower=0,upper=5> mu_tau;

# For log likelihood calculation
real log_lik[N];

mu_A   = Phi_approx(mu_p[1]);
mu_tau = Phi_approx(mu_p[2]) * 5;
}

## I took part of this out to save computation time: we don't really need the loglikelihood now
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

"

writeLines(modelString , con = "bandit2arm_deltaZ.stan")

twoarmBandit <-
  stan(
    file = 'bandit2arm_deltaZ.stan',
    data = dataList,
    thin = 1,
    warmup = 1000,
    iter = 5000,
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
}else{ 
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
show(zcalc(codaSamples, filters = c("mu_A", "mu_tau", "sigma")))

save.image(sprintf("alldata%sTN.Rdata", Sys.Date()))

df = data.frame(learningRateMean, inverseTemperatureMean, alpha, 1/beta)
a = ggplot(df,aes(learningRateMean, alpha)) + geom_point() + 
  ylim(mina, maxa) + xlim (mina, maxa) +
  labs(title = sprintf("Est-sim corr: %2.2f",cor(learningRateMean, alpha)), x = "Est learning rate",  y = "Sim learning rate") 

b = ggplot(df,aes(inverseTemperatureMean, 1/beta)) + geom_point() + 
  ylim(minb, maxb) + xlim (minb, maxb) +
  labs(title = sprintf("Est-sim corr: %2.2f",cor(inverseTemperatureMean, 1/beta)), x = "Est inv temp",  y = "Sim inv temp") 

multiplot(a,b, cols = 2)
