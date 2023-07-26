# Runs model on simulated data

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

filename <- here::here("data", "simulated", "rline_1.csv")
modelpath <- here::here("stan-models", "beta_count_2D.stan")
data <- read.csv(filename)

stan_data <- list(
  N = dim(data)[1],
  N_group = 1,
  ages = data,
  group_nos = rep(1, dim(data)[1]),
  min_age = 0,
  max_age = 1
)

model <- cmdstan_model(modelpath)

fit <- model$sample(
  data = stan_data, 
  seed = 846125, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit$summary()
mcmc_hist(fit$draws("mus"))

## DP one group 

stan_data4 <- list(
  N = dim(data)[1],
  K = 6,
  ages = data,
  min_age = 0,
  max_age = 1
)

modelpath4 <- here::here("stan-models", "beta_mixture_DP_one_group.stan")
model4 <- cmdstan_model(modelpath4)

fit4 <- model4$sample(
  data = stan_data4, 
  seed = 846125, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 1000,
  iter_sampling = 3000
)

fit4$summary()
mcmc_hist(fit4$draws("mus"))
mcmc_hist(fit4$draws("kappas"))

draws4 <- fit4$draws(c("weights", "mus", "kappas", "psis", "pred_class", "lp__"))
draws4 <- as_draws_matrix(draws4)


post_pars <- fit4$draws(c("weights", "mus", "lp__"), format = "matrix")

m = 3000 # of draws
K = 3 # of classes
J = 2 

# initialize mcmc arrays
mcmc <- array(data = NA, dim = c(m = m, K = K, J = J))

# assign posterior draws to the array
mcmc[, , 1] <- post_pars$weights
for (i in 1:(J - 1)) {
  mcmc[, , i + 1] <- post_pars$p[, , i]
}

# set of selected relabeling algorithm
set <-
  c("PRA",
    "ECR",
    "ECR-ITERATIVE-1",
    "AIC",
    "ECR-ITERATIVE-2",
    "STEPHENS",
    "DATA-BASED")

# find the MAP draw as a pivot
mapindex = which.max(post_pars$lp__)


pdf("beta_mixture_DP_hist.pdf")
mcmc_hist(fit4$draws("mus"))
dev.off()

pdf("beta_mixture_DP_trace.pdf")
mcmc_trace(fit4$draws("mus"))
dev.off()

## DP sample code

stan_data5 <- list(
  K = 3,
  N = 300,
  y = rnorm(300),
  alpha_shape = 1,
  alpha_rate = 1,
  sigma_shape = 1,
  sigma_rate = 1
)

modelpath5 <- here::here("stan-models", "GPmixture.stan")
model5 <- cmdstan_model(modelpath5)

fit5 <- model5$sample(
  data = stan_data5, 
  seed = 846125, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 500,
  iter_sampling = 2000
)

# Test SG process

modelpath6 <- here::here("stan-models", "beta_mixture_SG_one_group.stan")
model6 <- cmdstan_model(modelpath6)

fit6 <- model6$sample(
  data = stan_data4, 
  seed = 846125, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 5000,
  iter_sampling = 10000,
  adapt_delta = 0.999
)

pdf("SG_trace_weights.pdf")
mcmc_trace(fit6$draws("weights"))
dev.off()

pdf("SG_trace_alpha.pdf")
mcmc_trace(fit6$draws("alpha"))
dev.off()

pdf("SG_trace_eta.pdf")
mcmc_trace(fit6$draws("eta"))
dev.off()

pdf("SG_trace_mus.pdf")
mcmc_trace(fit6$draws("mus"))
dev.off()

pdf("SG_trace_lp.pdf")
mcmc_trace(fit6$draws("lp__"))
dev.off()

modelpath7 <- here::here("stan-models", "logit_gaussian_mixture_DP_one_group.stan")
model7 <- cmdstan_model(modelpath7)

fit7 <- model7$sample(
  data = stan_data4, 
  seed = 846125, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 3000,
  iter_sampling = 5000
)
