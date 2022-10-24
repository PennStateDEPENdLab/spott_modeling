library(dplyr)

repo_dir <- "/Users/maruofan/Documents/GitHub/spott_modeling/data/SPOTT_gauss_ran_walk/data"
setwd(repo_dir)
insblock_001 <- read.csv("001_insblock.csv")
insblock_002 <- read.csv("002_insblock.csv")

insblock_temp <- insblock_001 %>% group_by(instrial) %>% summarize(prewardA = unique(prewardA), prewardB = unique(prewardB), respA = sum(key == "9("), respB = sum (key=="8*"))
