library(doFuture)
library(doRNG)
library(foreach)
library(future.batchtools)
library(iterators)
registerDoFuture()

sim_grid <- expand.grid(alpha=seq(0.001, 0.9, by=0.1), # increment 0.1 <- 0.01
                        gamma=seq(0.1, 100, by = 3),  # increment 1 <- 0.1
                        nu=seq(-5, 5, by=1),
                        #beta=seq(25, 500, by=25), # increment 25 <- 1
                        omega=seq(-5, 5, by=1), # increment 1 <- 0.1
                        kappa=seq(0.001, 10, by=1)) #increment 1 <- 0.1

row_ind <- 1:nrow(sim_grid)

#split_df_rs <- ??

chunk_size <- 10000 #set how many items from the dataframe are sent to each parallel execution of the loop

# future set up: for using slurm as the backend of future
#https://tdhock.github.io/blog/2019/future-batchtools/
future::plan(
  future.batchtools::batchtools_slurm,
  template = "slurm-simple",
  resources = list(
    walltime = 60 * 60,
    memory = 1000, # 1 GB
    ncpus = 1,
    chunks.as.arrayjobs = TRUE
  )
)
future.debug = TRUE

future.apply::future_apply(row_ind, c(1), future.chunk.size=chunk_size,  function(row_index){
  print(sim_grid[row_index,])
})


# loop set up to iterate over the dataframe in parallel
# each interation handles "chunk.size" number of elements
# %dorng% sets the loop to run in parallel
# res <- foreach(
#   df = iter(sim_grid), .packages=c("brms", "tidybayes", "emmeans", "glue", "dplyr", "ggdist", "data.table"),
#   .options.future = list(chunk.size = chunk_size)) %dorng% {
    
#     print(head(df))
    
#     }



#########
#########


# # par_list contains values of alpha, gamma, nu, omega, kappa; e.g., par_list <- sim_grid[1,]
# # par_list is one element in sim_grid, so the function run_sim goes through one iteration in the original for loop
# run_sim <- function(par_list){
#   these_params <- list(
#     alpha=expression(rtruncnorm(nsubjects, a=0.01, b=0.99, mean=par_list$alpha, sd=0.2)),
#     gamma=expression(rgamma(nsubjects, shape=par_list$gamma, rate=1)),
#     nu=expression(rnorm(nsubjects, mean=par_list$nu, sd=0)), #deprecated parameter
#     omega=expression(rnorm(nsubjects, mean=par_list$omega, sd=2)), #switch omega/stickiness
#     kappa=expression(rgamma(nsubjects, shape=par_list$kappa, rate=1)) #(inverse) temperature on value-guided component of choice
#   )
#   
#   #simulate data using a population distribution on the parameters -- takes a few minutes
#   stan_population <- sim_spott_free_operant_group(
#     nsubjects=20, task_environment = task_environment,
#     these_params)
#   
#   #distill subject parameters
#   parmat <- stan_population %>% group_by(id) %>% summarize_at(vars(alpha, gamma, nu, omega, kappa), mean)
#   
#   #name the path for simulation outputs
#   out_dir <- file.path(repo_dir, "data", paste0("vba_input_simX", as.character(i))) # Now how do I name the new document?
#   if (!dir.exists(out_dir)) { dir.create(out_dir) }
#   dsplit <- stan_population %>% select(-alpha, -gamma, -nu, -omega, -kappa)
#   
#   dsplit <- split(dsplit, dsplit$id)
#   for (d in seq_along(dsplit)) {
#     id <- names(dsplit)[d]
#     data <- dsplit[[d]]
#     readr::write_csv(data, file=file.path(out_dir, sprintf("%03s_spott_20.csv", id)))
#   }
#   
#   #this writes the combined data for all subjects for multi-subject/hierarchical fitting
#   write.csv(parmat, file=file.path(out_dir, "stan_population_demo_parameters.csv"), row.names=F)
# }