print("Running")
library(cmdstanr)

args_line <-  as.list(commandArgs(trailingOnly=TRUE))
print(args_line)
if(length(args_line) > 0)
{
  stopifnot(args_line[[1]]=='-indir')
  indir <- args_line[[2]]
}

filename <- file.path(indir, "data", "simulated", "rline_1.csv")
modelpath <- file.path(indir, "stan-models", "beta_mixture_DP_one_group_flex_alpha.stan")

## For use on laptop:
#filename <- here::here("data", "simulated", "rline_1.csv")
#modelpath <- here::here("stan-models", "beta_mixture_DP_one_group_flex_alpha.stan")

model <- cmdstan_model(modelpath)
data <- read.csv(filename)

K <- 5 # number of clusters

stan_data <- list(
  N = dim(data)[1],
  K = K,
  ages = data,
  min_age = 0,
  max_age = 1
)

fit <- model$sample(
  data = stan_data, 
  seed = 538164, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 3000,
  iter_sampling = 20000
)

saveRDS(fit, here::here("data", "rline_1_draws_20k_flex.rds"))
