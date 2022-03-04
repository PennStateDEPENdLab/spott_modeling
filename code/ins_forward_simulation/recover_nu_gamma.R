setwd("~/Data_Analysis/spott_modeling/code/ins_forward_simulation")
library(tidyverse)
library(cowplot)
#########
## response functions
# New exponential recovery
# multiple process decay -- constant plus value-guided recovery
p_response_exp <- function(Qstar, tdiff=NULL, gamma=1, nu=1) {
  
  tdiff = tdiff/1000 # rescale parameters in seconds (avoid crazy values)
  
  p_respond_tb <- 1 - exp(-1*(nu + gamma*Qstar)*tdiff)
  return(p_respond_tb)
}

# Old time2pl response
# notes: if Qstar = 0, presp will always be 0. Almost makes me wonder about some sort of Qstar^gamma parameterization
p_response <- function(Qstar, tdiff=NULL, gamma=1, nu=1) {
  tdiff = (tdiff)/1000 # rescale parameters in seconds (avoid crazy values)
  
  p_respond_tb <- 1 / (1 + exp(-gamma*Qstar*(tdiff+nu))) #probability of making a response in this bin
  return(p_respond_tb)
}


# SSE fitting function (objective)
fit_preds <- function(params, Qstar, tdiff, pvec, model="exp", optimize = TRUE) {
  if (model == "exp") {
    phat <- sapply(seq_along(Qstar), function(i) {
      p_response_exp(Qstar[i], tdiff[i], gamma = params["gamma"], nu = params["nu"])
    })
  } else if (model == "2pl") {
    phat <- sapply(seq_along(Qstar), function(i) {
      p_response(Qstar[i], tdiff[i], gamma = params["gamma"], nu = params["nu"])
    })
  }
  
  # SSE objective -- minimize discrepancy in predicted versus observed probabilities
  objective <- sum((pvec - phat)^2)
  
  if (optimize) {
    return(objective)
  } else {
    return(list(phat=phat, pvec=pvec, sse = objective))
  }
}

#########
## visualization of 2pl

# time2pl values of gamma
gvals_2pl <- 1.1^(seq(from=-30, to=25, length.out=20)) # prioritize low values. spans .057 -- 10.83

sim_grid <- expand.grid(
  gamma = gvals_2pl,
  nu = seq(-5, 5, by = 1),
  Qstar = seq(.02, 2, by = .1),
  tdiff = seq(50, 5000, by=150)
)

sim_grid$presp <- apply(sim_grid, 1, function(row) {
  do.call(p_response, as.list(row))
})

to_plot <- sim_grid %>%
  mutate(gamma_f = factor(gamma), nu_f = factor(nu))

pdf("big_grid.pdf", width=25, height=25)
g <- ggplot(to_plot, aes(x=gamma_f, y=nu_f, fill=presp)) + geom_tile() + facet_grid(Qstar ~ tdiff) + scale_fill_viridis_c()
plot(g)
dev.off()

# look at whether the term inside parentheses (time elapsed + nu) controls decision boundary as expected
to_plot <- to_plot %>% 
  mutate(parens = factor(tdiff/1000 + nu))
pdf("big_grid_parens.pdf", width=25, height=25)
g <- ggplot(to_plot, aes(x=gamma_f, y=parens, fill=presp)) + geom_tile() + facet_wrap(~Qstar) + scale_fill_viridis_c()
plot(g)
dev.off()

## visualize exponential recovery

# both nu and gamma have to be positive rates of recovery
gvals_exp <- 1.1^(seq(from=-35, to=1, length.out=20)) # even lower values for exp

sim_grid_exp <- expand.grid(
  gamma = gvals_exp,
  nu = seq(0.1, 2, by = .2),
  Qstar = seq(.05, 2.5, by = .3),
  tdiff = seq(50, 5000, by=100) 
)

sim_grid_exp$presp <- apply(sim_grid_exp, 1, function(row) {
  do.call(p_response_exp, as.list(row))
})

to_plot <- sim_grid_exp %>%
  mutate(gamma_f = factor(gamma), nu_f = factor(nu))

pdf("big_grid_exp.pdf", width=25, height=25)
g <- ggplot(to_plot, aes(x=gamma_f, y=nu_f, fill=presp)) + geom_tile() + facet_grid(Qstar ~ tdiff) + scale_fill_viridis_c()
plot(g)
dev.off()

### PARAMETER FITTING
# values over which to fit exp model
recover_grid_exp <- expand.grid(
  gamma = gvals_exp,
  nu = seq(0.1, 3, by = .2),
  Qstar = seq(.05, 2.5, by = .1),
  tdiff = seq(50, 5000, by=100)
)

recover_grid_exp$presp <- apply(recover_grid_exp, 1, function(row) {
  do.call(p_response_exp, as.list(row))
})

# Parameter boundaries for exp model. All decay rates must be > 0
exp_params <- list(
  value=c(gamma =    .1,   nu  =    1), 
  lower=c(gamma = 1e-10,   nu = 1e-10),
  upper=c(gamma =    10,   nu =    10),
  par_scale=c(gamma=1e-1,  nu = 1e-1)
)

recover_opt_exp <- recover_grid_exp %>% group_by(gamma, nu) %>% nest()

res_exp <- recover_opt_exp %>%
  mutate(optres = purrr::map(data, function(df) {
    nlminb(start=exp_params$value, objective=fit_preds, 
           lower=exp_params$lower, upper=exp_params$upper, 
           Qstar = df$Qstar, tdiff = df$tdiff, pvec = df$presp, model="exp",
           scale=1/exp_params$par_scale, control=list(eval.max=500, iter.max=500))
  }))

res_df_exp <- res_exp %>% select(-data) %>% 
  mutate(pars = purrr::map_df(optres, ~.x$par %>% setNames(c("gamma_fit", "nu_fit")))) %>%
  select(-optres) %>%
  unnest(pars) 

pdf("fit_plots_exp.pdf", width=8, height=8)
g1 <- ggplot(res_df_exp, aes(x=gamma, y=gamma_fit)) + geom_point() + ggtitle("Exp decay simulated versus fit gamma")
g2 <- ggplot(res_df_exp, aes(x=nu, y=nu_fit)) + geom_point() + ggtitle("Exp decay simulated versus fit gamma")
gg <- plot_grid(g1, g2, nrow=1)
plot(gg)
dev.off()

## Optimizing time2pl

# values over which to fit exp model
recover_grid_2pl <- expand.grid(
  gamma = gvals_2pl,
  nu = seq(-5, 5, by = .25),
  Qstar = seq(.02, 3, by = .1),
  tdiff = seq(50, 5000, by=150)
)

recover_grid_2pl$presp <- apply(recover_grid_2pl, 1, function(row) {
  do.call(p_response, as.list(row))
})

# Parameter boundaries for exp model. All decay rates must be > 0
params_2pl <- list(
  value=c(gamma =    1,   nu  =    0), 
  lower=c(gamma = 1e-10,   nu = -100),
  upper=c(gamma =    20,   nu =  100),
  par_scale=c(gamma=1e-1,  nu = 1e0)
)

recover_opt_2pl <- recover_grid_2pl %>% group_by(gamma, nu) %>% nest()

res_2pl <- recover_opt_2pl %>%
  mutate(optres = purrr::map(data, function(df) {
    nlminb(start=exp_params$value, objective=fit_preds, 
           lower=exp_params$lower, upper=exp_params$upper, 
           Qstar = df$Qstar, tdiff = df$tdiff, pvec = df$presp, model = "2pl",
           scale=1/exp_params$par_scale, control=list(eval.max=500, iter.max=500))
  }))

res_df_2pl <- res_2pl %>% select(-data) %>% 
  mutate(pars = purrr::map_df(optres, ~.x$par %>% setNames(c("gamma_fit", "nu_fit")))) %>% 
  unnest(pars) %>% select(-optres)

pdf("fit_plots_2pl.pdf", width=8, height=8)
g1 <- ggplot(res_df_2pl, aes(x=gamma, y=gamma_fit)) + geom_point() + ggtitle("2pl decay simulated versus fit gamma")
g2 <- ggplot(res_df_2pl, aes(x=nu, y=nu_fit)) + geom_point() + ggtitle("2pl decay simulated versus fit nu")
gg <- plot_grid(g1, g2, nrow=1)
plot(gg)

g1 <- ggplot(res_df_2pl, aes(x=gamma, y=gamma_fit, color=nu)) + geom_point() + ggtitle("2pl decay simulated versus fit gamma")
g2 <- ggplot(res_df_2pl, aes(x=nu, y=nu_fit, color=gamma)) + geom_point() + ggtitle("2pl decay simulated versus fit nu")
gg <- plot_grid(g1, g2, nrow=1)
plot(gg)

dev.off()

### LEFTOVERS
#optimizing test case
# 
# sim_grid %>% setDT()
# xx <- split(sim_grid, by=c("gamma", "nu"))
# test <- xx[[1]]
# 
# 
# elapsed_time <- system.time(optResult <- nlminb(start=exp_params$value, objective=fit_preds, 
#                                                 lower=exp_params$lower, upper=exp_params$upper, 
#                                                 Qstar = test$Qstar, tdiff = test$tdiff, pvec = test$presp,
#                                                 scale=1/exp_params$par_scale, control=list(eval.max=500, iter.max=500)))
# 
# abc <- fit_preds(optResult$par, Qstar = test$Qstar, tdiff = test$tdiff, pvec = test$presp, optimize=F)
# 
# plot(abc$phat, abc$pvec)
# hist(abc$phat - abc$pvec)
# 
# # recovery for each pair of nu and gamma values
# # notes on numerator version
# # larger nu + gamma*Qstar => higher response probability
# # when Qstar is 0 and tdiff is 500ms, values of beta ~ 1.4 yield .5 probability
