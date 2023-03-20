library(stringr)
setwd("~/Documents/GitHub/spott_modeling/code/param_recovery")

sink("sim_IDs.txt")
for (i in c(9684, 32155:32157)){
  cat(paste0("cond", str_pad(i, 5, pad = "0")))
  cat("\n")
}
sink()

