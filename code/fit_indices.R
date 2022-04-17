setwd("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/data")
DF_20 <- get(load("ggDF_20-trial_full.RData"))
DF_50 <- get(load("ggDF_50-trial_full.RData"))

# relative_bias
# Input: parameter is a string (e.g., "alpha"); 
#       data is the dataset containing the original 
#       parameter value and the recovered parameter estimate
# Output: relative bias
relative_bias <- function(parameter, data){
  true_param <- paste0(parameter, "_mean")
  est_param <- paste0(parameter, "_transformed")
  size <- nrow(data)
  return(sum((data[,est_param]-data[,true_param])/data[,true_param])/size)
}

# RMSE
# Input: parameter is a string (e.g., "alpha"); 
#       data is the dataset containing the original 
#       parameter value and the recovered parameter estimate
# Output: RMSE
RMSE <- function(parameter, data){
  true_param <- paste0(parameter, "_mean")
  est_param <- paste0(parameter, "_transformed")
  size <- nrow(data)
  return(sqrt(sum((data[,est_param]-data[,true_param])^2)/size))
}

fit_ind_20 <- data.frame(alpha = c(relative_bias("alpha", DF_20), RMSE("alpha", DF_20)),
                      gamma = c(relative_bias("gamma", DF_20), RMSE("gamma", DF_20)),
                      nu = c(relative_bias("nu", DF_20), RMSE("nu", DF_20)),
                      kappa = c(relative_bias("kappa", DF_20), RMSE("kappa", DF_20)),
                      omega = c(relative_bias("omega", DF_20), RMSE("omega", DF_20)))

fit_ind_50 <- data.frame(alpha = c(relative_bias("alpha", DF_50), RMSE("alpha", DF_50)),
                         gamma = c(relative_bias("gamma", DF_50), RMSE("gamma", DF_50)),
                         nu = c(relative_bias("nu", DF_50), RMSE("nu", DF_50)),
                         kappa = c(relative_bias("kappa", DF_50), RMSE("kappa", DF_50)),
                         omega = c(relative_bias("omega", DF_50), RMSE("omega", DF_50)))


