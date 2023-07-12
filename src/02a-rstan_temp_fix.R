library(rstan)
options(mc.cores = parallel::detectCores())
library(posterior)
library(bayesplot)
library(label.switching)
library(dplyr)

filename <- here::here("data", "simulated", "rline_1.csv")
modelpath <- here::here("stan-models", "beta_count_2D.stan")
data <- read.csv(filename)

## DP one group 

K <- 5 # number of clusters

stan_data <- list(
  N = dim(data)[1],
  K = K,
  ages = data,
  min_age = 0,
  max_age = 1
)

modelpath <- here::here("stan-models", "beta_mixture_DP_one_group.stan")
fit <- stan(file = modelpath, data = stan_data, iter = 5500,
            warmup = 2500, seed = 846125, chains = 4)
saveRDS(fit, here::here("data", "rline_1_draws.rds"))
fit <- readRDS(here::here("data", "rline_1_draws.rds"))
print(fit, c("weights", "mus", "kappas", "psis", "pred_class", "lp__"))

pdf("5_cluster_traceplot.pdf")
traceplot(fit, c("weights", "mus"))
dev.off()

traceplot(fit, "lp__")
# extract stan fit as the required format of the input
pars <- fit %>% names %>% `[`(1:10)
fit@model_pars

post_par <- rstan::extract(fit,
                         c("weights", "mus", "kappas", "psis", "pred_class", 
                           "eta", "alpha","lp__"))

post_class_p <- post_par$pred_class
post_class <- apply(post_class_p, c(1, 2), which.max)

m <- 3000*4 # of draws
J <- 6

# initialize mcmc arrays
mcmc <- array(data = NA, dim = c(m = m, K = K, J = J))

# assign posterior draws to the array 
# NOT lp__ or pred_class

mcmc[, , 1] <- post_par$weights
mcmc[, , 2:3] <- post_par$kappas
mcmc[, , 4:5] <- post_par$mus
mcmc[, , 6] <- post_par$psis

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
mapindex = which.max(post_par$lp__)

# switch labels
ls_lcm <-
  label.switching(
    method = set,
    zpivot = post_class[mapindex,],
    z = post_class,
    K = K,
    prapivot = mcmc[mapindex, ,],
    constraint = 1,
    mcmc = mcmc,
    p = post_class_p,
    data = data
  )
print(ls_lcm$similarity)

mcmc_permuted <- permute.mcmc(mcmc, ls_lcm$permutations$ECR)
# change dimension for each parameter defined as in the Stan code
mcmc_permuted <- array(
    data = mcmc_permuted$output,
    dim = c(12000, 1, K*J)
  )

# reassess the model convergence after switch the labels
fit_permuted <- monitor(mcmc_permuted, warmup = 0,  digits_summary = 3)

plot_trace_own <- function(k){
  sample <- mcmc_permuted[, , k]
  sample <- cbind(1:3000, sample) 
  print(dim(sample))
  sample <- as.data.frame(sample)
  colnames(sample) <- c("index", "C1", "C2", "C3", "C4")
  p <- ggplot(sample) + geom_line(aes(x = index, y = C1), alpha = 0.7) + 
    geom_line(aes(x = index, y = C2, color = "red"), alpha = 0.7) + 
    geom_line(aes(x = index, y = C3, color = "blue"), alpha = 0.7) + 
    geom_line(aes(x = index, y = C4, color = "yellow"), alpha = 0.7) + 
    labs(title = paste0("Parameter", k)) 

  print(p)
}

pdf(file = "traceplots_permuted.pdf")
par(mfrow = c(3, 5))
for (k in 1:15){
  plot_trace_own(k)
}
dev.off()


pdf("lp_5_cluster.pdf")
traceplot(fit, "lp__")
dev.off()
