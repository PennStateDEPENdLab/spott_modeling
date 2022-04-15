library(stringr)
setwd("~/Documents/GitHub/spott_modeling/code/param_recovery")

sink("sim_IDs.txt")
for (i in 1:3240){
  cat(paste0("cond", str_pad(i, 6, pad = "0")))
  cat("\n")
}
sink()

