library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)

repo_dir <- "/Users/maruofan/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data" #Desktop
repo_dir <- "/Users/ruofanma/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data" #laptop
setwd(repo_dir)

data_files <- list.files(repo_dir)
data_files <- data_files[grepl(".csv", data_files)]


# Find a for each participant ---------------------------------------------

a_list <- rep(NA, length(data_files))
i <- 1

for (file in data_files){
  insblock_temp <- read.csv(file)
  insblock_temp <- insblock_temp %>% 
    group_by(instrial) %>% 
    summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), respA = sum (key=="8*"), respB = sum(key == "9(")) %>%
    mutate(p1_p2 = prewardA/prewardB, n1_n2 = respA/respB) %>%
    mutate(log_p1_p2 = log(p1_p2), log_n1_n2 = log(n1_n2))
  
  insblock_temp <- insblock_temp %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf)
  coef<- lm(log_n1_n2 ~ log_p1_p2, insblock_temp) %>% coefficients()
  a <- coef[2]
  a_list[i] <- a
  i <- i+1
}


# For each participant, is the better option pressed more? ----------------

pbest_coef_list <- rep(NA, length(data_files))
prew_coef_list <- rep(NA, length(data_files))
pbest2_coef_list <- rep(NA, length(data_files))
i <- 1

for (file in data_files){
  insblock_temp <- read.csv(file)
  insblock_temp <- insblock_temp %>% 
    group_by(instrial) %>% 
    summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), respA = sum (key=="8*"), respB = sum(key == "9(")) %>%
    mutate(n_best = ifelse(prewardA >= prewardB, respA, respB), p_best = ifelse(prewardA >= prewardB, prewardA, prewardB)) %>%
    mutate(proportion_best = n_best/(respA+respB), nresp = respA+respB, p_total = prewardA + prewardB)
  
  pbest_coef<-lm(proportion_best ~ p_best, insblock_temp) %>% coefficients() # Do they choose the better option more as the reward probability increases?
  pbest_coef <- pbest_coef[2]
  pbest_coef_list[i] <- pbest_coef
  
  prew_coef<-lm(nresp ~ p_total, insblock_temp) %>% coefficients() # Do the press more when total reward probability increases?
  prew_coef <- prew_coef[2]
  prew_coef_list[i] <- prew_coef
  
  pbest2_coef <- lm(nresp ~ p_best, insblock_temp) %>% coefficients() # Do the press more when the better reward probability increases?
  pbest2_coef <- pbest2_coef[2]
  pbest2_coef_list[i] <- pbest2_coef
  
  i <- i+1
}

# length(data_files) is 23
sum(pbest_coef_list >=0) #10
sum(prew_coef_list >=0) #13
sum(pbest2_coef_list >=0) #23



# Visualizing -------------------------------------------------------------

data <- read.csv(data_files[2])

## Ploting prewardA vs. trial, or prewardB vs. trial
## label is total # of press in the trial
insblock_temp <- data # read.csv(file)
insblock_temp <- insblock_temp %>% 
  group_by(instrial) %>% 
  summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), 
            trial = unique(instrial), n_press = max(eventNumber), 
            respA = sum (key=="8*"), respB = sum(key == "9(")) %>%
  mutate(total_prew = prewardA + prewardB)

g <- ggplot(insblock_temp, aes(x=trial, y=total_prew, label=n_press)) + 
  geom_text(position=position_dodge(width=0.5))

plot(g)


## Ploting prewardA and prewardB on the same plot, vs. trial
## label is total # of press in the trial

insblock_temp <- data # read.csv(file)
insblock_temp <- insblock_temp %>% 
  group_by(instrial) %>% 
  summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), 
            trial = unique(instrial), n_press = max(eventNumber), 
            respA = sum (key=="8*"), respB = sum(key == "9(")) %>%
  mutate(total_prew = prewardA + prewardB) %>%
  pivot_longer(cols = c("prewardA", "prewardB"), names_to = "response", values_to = "preward")

g <- ggplot(insblock_temp, aes(x=trial, y=preward, label=n_press)) + 
  geom_point(aes(color = response)) + 
  geom_text(position=position_dodge(width=0.5))
  
plot(g)

## Plotting the above but for all the participants

df <- data.frame()
for (file in data_files){
  insblock_temp <- read.csv(file)
  ID <- substring(file,1,3)
  insblock_temp$ID <- ID
  insblock_temp <- insblock_temp %>% 
    group_by(instrial) %>% 
    summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), 
              trial = unique(instrial), n_press = max(eventNumber), ID = unique(ID),
              respA = sum (key=="8*"), respB = sum(key == "9(")) %>%
    mutate(total_prew = prewardA + prewardB) %>%
    pivot_longer(cols = c("prewardA", "prewardB"), names_to = "response", values_to = "preward")
  
  df <- rbind(df, insblock_temp)
}

g <- ggplot(df, aes(x=trial, y=preward, label=n_press)) + 
  geom_point(aes(color = response)) + 
  geom_text(position=position_dodge(width=0.5)) +
  facet_wrap(~ID)

plot(g)

## plotting total reward (prewardA + prewardB)
g <- ggplot(df, aes(x=trial, y=total_prew, label=n_press)) + 
  geom_text(position=position_dodge(width=0.5)) +
  facet_wrap(~ID)

plot(g)
                                                     
#For total response scaling with total reward, ggplot(df, aes(x=p_total, y=n_resp)) + geom_point() + facet_wrap(~id)
            
                                                            
                                                                

