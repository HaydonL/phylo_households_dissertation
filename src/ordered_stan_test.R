library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
library(data.table)
library(mvtnorm)
color_scheme_set("brightblue")

modelpath <- here::here("stan-models", "ordered_test.stan")

model <- cmdstan_model(modelpath)

set.seed(6341874)
mus <- matrix(rnorm(6), nrow = 3)
matrices <- list(matrix(c(1, 0.5, 0.5, 1), nrow = 2),
                 matrix(c(2, 0, 0, 2), nrow = 2),
                 matrix(c(5, -4, -4, 5), nrow = 2)
                )

data <- matrix(nrow = 200, ncol = 2)
groups <- sample(2, 200, replace = TRUE, prob = c(0.6, 0.4))

for (index in 1:200){
  group <- groups[index]
  sample <- rmvnorm(1, mean = mus[group, ], matrices[[group]])
  data[index, ] <- sample
}

stan_data <- list(
  N = 200,
  K = 2,
  ages = data
)

fit <- model$sample(
  data = stan_data, 
  seed = 25491, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 1000,
  iter_sampling = 5000
)

mcmc_trace(fit$draws("ordered_mus"))

