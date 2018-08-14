library(ggplot2)
library(dplyr)
prob_win <- runif(200,0,1)
rdat<- data.frame(prob_win)
rdat$reward <- 0 
rdat$reward [rdat$prob_win >=.1]  <- 1 #50/50 reward/omission outcomes

epsilon <- .05 #learning rate
rdat$predicted_value <- NA
rdat$predicted_value[1] <- 0.1 #initial expectation (prior) of 10% reward probability

for (n in 2:nrow(rdat)){
  rdat$predicted_value[n] <- rdat$predicted_value[n-1] + 
    epsilon*(rdat$reward[n] - rdat$predicted_value[n-1])
}

rdat$trial <- 1:nrow(rdat)
#View(rdat)
#x1 <-rdat$predicted_value

ggplot(rdat, aes(x=trial, y=predicted_value)) + geom_line() +
  geom_point(aes(x=trial, y = reward))


## simulate Q learning agent at different levels of alpha and beta (LR and temperature)
set.seed(1050)
nsubjects <- 120
ntrials <- 100

meana <- 0.1
mina <- 0.02
maxa <- 0.4
sda <- 0.06
alpha <- rnorm(nsubjects, meana, sda)
#enforce constraints on reasonable alpha values
alpha[alpha < mina] <- mina
alpha[alpha > maxa] <- maxa
hist(alpha)

minb <- 0.1
maxb <- 7
beta <- rgamma(nsubjects, shape=4, scale=0.3)
beta[beta < minb] <- minb
beta[beta > maxb] <- maxb
summary(beta)
hist(beta)



test <- sim_qlearning_group(nsubjects=50, ntrial=50, preward=c(0.9, 0.1),
                                        alpha=list(meana = 0.1, mina = 0.02, maxa = 0.4, sda = 0.06),
                                        beta=list(meanb = 1, minb = 0.1, maxb = 10, sdb = 0.3), #this is inverse temp
                                        quiet=FALSE, seed=1050)
  




sub1 <- slist[[1]]$Q
omat <- get_omat(500, c(0.9, 0.1))

testcase <- simsubject(.02, .01, omat)

library(tidyverse)
mm <- reshape2::melt(testcase$Q) %>% dplyr::rename(trial=X1, action=X2)
ggplot(mm, aes(x=trial, y=value, color=factor(action))) + geom_line()


setwd("/Users/mnh5174/Data_Analysis/SPOTT/archive")
save(rewards, choices, file="Qlearning_sims_for_Zita_27Nov2017.RData")
save(slist, file="Qlearning_sims_master_27Nov2017.RData")

#correlations with Zita's fitted values
fitted <- read.csv("/Users/mnh5174/Downloads/ResultsToydata.csv")
hist(fitted$learningRateMean)

mean(fitted$learningRateMean)
sd(fitted$learningRateMean)

mean(fitted$inverseTemperatureMean)
sd(fitted$inverseTemperatureMean)

cor(fitted$learningRateMean, alphas)
cor(fitted$inverseTemperatureMean, betas)

# vv <- simsubject(0.15, 1e9, outcomes)
# 
# toplot <- data.frame(vv$Q)
# toplot$trial <- 1:nrow(toplot)
# toplot <- reshape2::melt(toplot, id.var="trial")
# library(ggplot2)
# ggplot(toplot, aes(x=trial, y=value, color=variable)) + geom_line()


