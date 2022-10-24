library(dplyr)

repo_dir <- "/Users/maruofan/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data" #Desktop
repo_dir <- "/Users/ruofanma/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data" #laptop
setwd(repo_dir)
insblock_001 <- read.csv("001_insblock.csv")
insblock_002 <- read.csv("002_insblock.csv")


insblock_temp <- insblock_001 %>% group_by(instrial) %>% summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), respA = sum (key=="8*"), respB = sum(key == "9("))
insblock_temp$p1_p2 <- insblock_temp$prewardA/insblock_temp$prewardB
insblock_temp$n1_n2 <- insblock_temp$respA/insblock_temp$respB
insblock_temp$log_p1_p2 <- log(insblock_temp$p1_p2)
insblock_temp$log_n1_n2 <- log(insblock_temp$n1_n2)

insblock_temp <- insblock_temp %>% filter(log_n1_n2 > -Inf & log_n1_n2 < Inf)
summary(lm(log_n1_n2 ~ log_p1_p2, insblock_temp))

