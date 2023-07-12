library(rstan)
options(mc.cores = parallel::detectCores())

filename <- here::here("data", "simulated", "rline_1.csv")
modelpath <- here::here("stan-models", "beta_mixture_DP_one_group.stan")
data <- read.csv(filename)

K <- 5 # number of clusters

stan_data <- list(
  N = dim(data)[1],
  K = K,
  ages = data,
  min_age = 0,
  max_age = 1
)

fit <- stan(file = modelpath, data = stan_data, iter = 23000,
            warmup = 3000, seed = 538164, chains = 4)
saveRDS(fit, here::here("data", "rline_1_draws_20k.rds"))
