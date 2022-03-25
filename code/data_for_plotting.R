library(dplyr)

# Simulation settings should match ins_forward_simulation_parallel.R
nsubjects <- 50
model <- "exp"

# means of gamma
gvals <- 1.1^(seq(from=-30, to=25, length.out=20)) # prioritize low values. spans .057 -- 10.83

# means of kappa
kvals <- c(1, 4, 8) #1.14^(seq(-10, 20, 2)) # prioritize low values. spans .27 -- 13.74

sim_grid <- expand.grid(
  model = model,
  alpha_mean = seq(0.1, 0.9, by = 0.1), # increment 0.1 <- 0.01
  alpha_sd = 0.1, # fixed for now
  alpha_min = 0.02, # set constraints on learning rates
  alpha_max = 0.98,
  gamma_mean = gvals,
  nu_mean = seq(1, 5, by = 1), # Keeping nu positive
  nu_sd = 0.5,
  # nu_sd = 1,
  nu_min = 0.001,
  nu_max = 10,
  omega_mean = 0, #seq(-5, 5, by = 1), # increment by 1 #Zita: omega=seq(0, 5, by=1)
  omega_sd = 1,
  kappa_mean = kvals
)

# # smaller nu values
# sim_grid <- expand.grid(
#   model = model,
#   alpha_mean = seq(0.1, 0.9, by = 0.1), # increment 0.1 <- 0.01
#   alpha_sd = 0.1, # fixed for now
#   alpha_min = 0.02, # set constraints on learning rates
#   alpha_max = 0.98,
#   gamma_mean = gvals,
#   nu_mean = c(0.25, 0.5, 1, 1.5, 2, 2.5), # Keeping nu positive
#   nu_sd = 0.3,
#   # nu_sd = 1,
#   nu_min = 0.001,
#   nu_max = 10,
#   omega_mean = 0, #seq(-5, 5, by = 1), # increment by 1 #Zita: omega=seq(0, 5, by=1)
#   omega_sd = 1,
#   kappa_mean = kvals
# )

# select data for testing
# data_ind <- c(1:150, 1900:2050, 4057:4107)
data_ind <- c(1:2700)

# Preapre data for plotting

sim_dir <- "/proj/mnhallqlab/projects/spott_modeling/par_sim_exp/fix_omega_kappa/"
# data e.g., /proj/mnhallqlab/projects/spott_modeling/par_sim_exp/fix_omega_kappa/cond000001/stan_population_parameters_1.csv
  
vba_out_dir <- "/proj/mnhallqlab/projects/spott_modeling/par_rec_exp/rec_fix_omega_kappa/outputs/vba_out/ffx/"
# data e.g., /proj/mnhallqlab/projects/spott_modeling/par_rec_exp/rec_fix_omega_kappa/outputs/vba_out/ffx/cond000001/exp/cond000001_exp_ffx_global_statistics.csv

# Local
# simIDs <- read.delim("~/Documents/GitHub/spott_modeling/code/param_recovery/sim_IDs.txt", header = FALSE)
# sim <- read.csv(paste0("/Users/ruofanma/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data/stan_population_parameters_",11,".csv"))
# est <- read.csv(paste0("/Users/ruofanma/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/outputs/vba_out/ffx/",simIDs[11,],"_time2pl_ffx_global_statistics.csv"))

# LL
simIDs <- read.delim("/nas/longleaf/home/maruofan/GitHub/spott_modeling/code/param_recovery/sim_IDs.txt", header = FALSE)
sim <- read.csv(paste0(sim_dir, simIDs[1,], "/stan_population_parameters_1.csv"))
est <- read.csv(paste0(vba_out_dir, simIDs[1,], "/exp/", simIDs[1,], "_exp_ffx_global_statistics.csv"))

for(i in data_ind[2:length(data_ind)]){
  sim_current <- read.csv(paste0(sim_dir, simIDs[i,], "/stan_population_parameters_",i, ".csv"))
  est_current <- read.csv(paste0(vba_out_dir, simIDs[i,], "/exp/", simIDs[i,], "_exp_ffx_global_statistics.csv"))
  
  sim <- sim %>% rbind(sim_current)
  est <- est %>% rbind(est_current)
}

# Get the mean values used to generate simulated data, because plotting in panels requires discrete values
# Variable names are parameter_mean (e.g., alpha_mean)
sim_grid_rep <- sim_grid[data_ind,] %>% slice(rep(1:n(), each = nsubjects))

# Combine the simulated data and the underlying parameter values
sim <- cbind(sim, sim_grid_rep)

#Combine the simulation parameter values and recovered parameter values for plotting
ggDF <- data.frame(sim, est)

save(ggDF, file = "/nas/longleaf/home/maruofan/GitHub/spott_modeling/data/ggDF.RData")
# hist(ggDF$nu, bin = 6)
