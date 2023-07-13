library(rstan)
options(mc.cores = parallel::detectCores())

args_line <-  as.list(commandArgs(trailingOnly=TRUE))
print(args_line)
if(length(args_line) > 0)
{
  stopifnot(args_line[[1]]=='-indir')
  indir <- args_line[[2]]
}

filename <- file.path(indir, "data", "simulated", "rline_1.csv")
modelpath <- file.path(indir, "stan-models", "beta_mixture_DP_one_group.stan")

## For use on laptop:
#filename <- here::here("data", "simulated", "rline_1.csv")
#modelpath <- here::here("stan-models", "beta_mixture_DP_one_group.stan")
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
