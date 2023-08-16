# Runs model on simulated data

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

filename <- here::here("data", "simulated", "sim_1.csv")
modelpath <- here::here("stan-models", "beta_mixture_DP.stan")
data <- read.csv(filename)

stan_data <- list(
  N = dim(data)[1],
  N_group = 4,
  ages = data[c("x", "y")],
  group_nos = data$group,
  min_age = 0,
  max_age = 1,
  K = 5
)

model <- cmdstan_model(modelpath)

fit <- model$sample(
  data = stan_data, 
  seed = 870310, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 3000,
  iter_sampling = 5000
)

fit$save_object(here::here("data", "beta_sim_1_draws.rds"))