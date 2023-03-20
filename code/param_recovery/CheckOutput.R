vba_out_dir <- "/proj/mnhallqlab/projects/spott_modeling/outputs/par_vba_out_full_ffx/"

simIDs <- read.delim("/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/param_recovery/sim_IDs.txt", header = FALSE)
for(i in 1:4){
  if(!file.exists(paste0(vba_out_dir, simIDs[i,], "/exp/", simIDs[i,], "_exp_ffx_global_statistics.csv"))){
    print(i)
  }
}