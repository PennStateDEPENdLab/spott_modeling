# this script checks which simulation conditions are missing, when the number of output files is fewer than expected

outputs <- list.files("/nas/longleaf/home/maruofan/mnhallqlab/projects/spott_modeling/outputs/par_sim_exp_full")
save(outputs, file = "/nas/longleaf/home/maruofan/GitHub/spott_modeling/data/parallel_outputs.RData")
# I just downloaded the "parallel_outputs.RData" file from longleaf to work locally 

out_IDs <- str_pad(1:32400, 5, pad = "0")
out_IDs <- paste0("cond", out_IDs)

for(i in 1:32400){
  if(!(out_IDs[i] %in% outputs)){
    print(i)
  }
}

# [1] 9449
# [1] 9685
# [1] 9686
# [1] 9687
# [1] 9688
# [1] 9689