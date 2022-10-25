library(dplyr)
library(broom)

repo_dir <- "/Users/maruofan/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data" #Desktop
repo_dir <- "/Users/ruofanma/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data" #laptop
setwd(repo_dir)
insblock_001 <- read.csv("001_insblock.csv")
insblock_002 <- read.csv("002_insblock.csv")

data_files <- list.files(repo_dir)[grepl(".csv", data_files)]

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



