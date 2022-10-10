

# Cost function -----------------------------------------------------------

# fn for optim()
# fn is the cost function: A function to be minimized (or maximized), with first 
# argument the vector of parameters over which minimization is to take place. 
# It should return a scalar result.

# params <-  c(alpha=, gamma=, nu=, omega=, kappa=)
# a <- 0.8

distance_a <- function(params, ){
  nsubjects <- # This is the sample size that will go into the denominator of cost
    
  ## This is wrong. I think this is all done by the optim()
  # Need a list of different combos of parameters
  # For each combo
  # (1) simulate data (with repetitions, i.e., multiple subjects) -- check ins_forward; there are already simulated data
  # and (2) calculate a by lm
  # 
  cost <- 
}
  



# Optimizer ---------------------------------------------------------------

starting_vals <- c(alpha=, gamma=, nu=, omega=, kappa=)

lower_bounds <- c(alpha=, gamma=, nu=, omega=, kappa=)
upper_bounds <- c(alpha=, gamma=, nu=, omega=, kappa=)
  
relativeScale <- c(alpha=, gamma=, nu=, omega=, kappa=)

# Need to change after writing the input function to fn; 

# lower and upper are needed for the "L-BFGS-B" method

For the "SANN" method it specifies a function to generate a new candidate point. If it is NULL a default Gaussian Markov kernel is used.

# optimizedParams <- optim(starting_vals, fn=maximizeCorrelation, method="L-BFGS-B", 
                        lower=lower_bounds, upper=upper_bounds,
                        control=list(parscale=relativeScale),
                        orig=simAmp_ntrial_oneReg_allSub, BOLD=bold_ts_oneReg_allSub, event_data=event_data_allSub,
                        TR=TR)