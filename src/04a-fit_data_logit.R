library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
library(data.table)
color_scheme_set("brightblue")

filename <- here::here("data", "pairs_tsi.csv")
modelpath <- here::here("stan-models", "logit_gaussian_mixture_DP.stan")
pairs_tsi <- read.csv(filename)

setDT(pairs_tsi)
pairs_tsi[, group := 1 + as.integer(2 * (SEX.SOURCE == "M") + same_hh)]

stan_data <- list(
  N = pairs_tsi[, .N],
  K = 5,
  N_group = 4,
  ages = as.matrix(pairs_tsi[, .(AGE_TRANSMISSION.SOURCE, 
                            AGE_INFECTION.RECIPIENT)]),
  group_nos = pairs_tsi[, .(group)][[1]],
  min_age = 15,
  max_age = 50
)

model <- cmdstan_model(modelpath)

fit <- model$sample(
  data = stan_data, 
  seed = 25491, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit$save_object(here::here("data", "logit_pairs_draws.rds"))
