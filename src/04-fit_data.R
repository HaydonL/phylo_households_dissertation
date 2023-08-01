print("Running")
library(data.table)
library(cmdstanr)
library(bayesplot)
library(posterior)

args_line <-  as.list(commandArgs(trailingOnly=TRUE))
print(args_line)
if(length(args_line) > 0)
{
  stopifnot(args_line[[1]]=='-indir')
  indir <- args_line[[2]]
}

# Process data
filename <- file.path(indir, "data", "pairs_tsi.csv")
pairs_tsi <- read.csv(filename)

setDT(pairs_tsi)
pairs_tsi[, group := as.integer(2 * (SEX.SOURCE == "M") + same_hh)]

K <- 5

modelpath <- file.path(indir, "stan-models", "logit_gassuain_DP.stan")
model <- cmdstan_model(modelpath)

for (group_no in 0:3){
  
  data <- pairs_tsi[group == group_no]
  stan_data <- list(
    N = data[, .N],
    K = K,
    ages = as.matrix(data[, .(AGE_TRANSMISSION.SOURCE, 
                              AGE_INFECTION.RECIPIENT)]),
    min_age = 15,
    max_age = 50
  )
  
  fit <- model$sample(
    data = stan_data, 
    seed = 648257, 
    chains = 4,  
    parallel_chains = 4,
    refresh = 500,
    iter_warmup = 5000,
    iter_sampling = 20000,
    adapt_delta = 0.999
  )
  
  draws <- fit$draws()
  filepath <- file.path(indir, "data", "SG_K6_aK_m10", 
                        paste0("pairs_tsi_draws_", group_no, ".rds"))
  fit$save_object(filepath)
  #saveRDS(draws, file.path(indir, "data", "SG_K6_aK_m10", 
  #                         paste0("pairs_tsi_draws_", group_no, ".rds")))
}
