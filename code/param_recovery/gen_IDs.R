sink("sim_IDs.txt")
for(i in 61:62){
  cat(paste0("cond", str_pad(i, 6, pad = "0")))
  cat("\n")
}
sink()

