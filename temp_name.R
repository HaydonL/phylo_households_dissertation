library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

filename <- here::here("data", "simulated", "sim_1.csv")
modelpath <- here::here("stan-models", "logit_gaussian_mixture_DP.stan")
data <- read.csv(filename)

stan_data <- list(
  N = dim(data)[1],
  K = 5,
  N_group = 4,
  ages = data[c("x", "y")],
  group_nos = data$group,
  min_age = 0,
  max_age = 1
)

model <- cmdstan_model(modelpath)

fit <- model$sample(
  data = stan_data, 
  seed = 4274915, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit$save_object(here::here("data", "logit_sim_1.rds"))

mcmc_trace(fit$draws("eta"))
mcmc_trace(fit$draws("weights"))

weight_draws <- fit$draws("weights")
