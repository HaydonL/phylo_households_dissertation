# Runs model on simulated data

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
library(data.table)
color_scheme_set("brightblue")

filename <- here::here("data", "pairs_tsi_clean.csv")
modelpath <- here::here("stan-models", "logit_gaussian_mixture_SG_ordered.stan")
pairs_tsi <- read.csv(filename)
setDT(pairs_tsi)


# Set groups for model
pairs_tsi[, same_comm := as.integer(COMM.SOURCE == COMM.RECIPIENT)]

get_group <- function(same_hh, same_comm, comm_source){
  # Same household
  if (same_hh == 1){
    if (comm_source == "fishing"){ # Fishing
      return(0)
    }
    else{ # Inland
      return(1)
    }
  }
  # Different household
  else{ 
    if (same_comm == 0){
      return(2) # Inter-community transmission
    }
    else if(comm_source == "fishing"){
      return(3) # Fishing only
    }
    else{
      return(4) # Inland only
    }
  }
}

N <- nrow(pairs_tsi)
groups <- numeric(N)
for (row in 1:N){
  same_hh <- as.integer(pairs_tsi[row, .(same_hh)])
  same_comm <- as.integer(pairs_tsi[row, .(same_comm)])
  comm_source <- as.character(pairs_tsi[row, .(COMM.SOURCE)])
  groups[row] <- get_group(same_hh, same_comm, comm_source)
}

pairs_tsi[, group := 1 + groups]

write.csv(pairs_tsi, here::here("data", "Q1_2_data.csv"))

stan_data <- list(
  N = pairs_tsi[group < 5, .N],
  K = 3,
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
  seed = 28592, 
  chains = 4,  
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 3000,
  iter_sampling = 5000,
  adapt_delta = 0.95
)

fit$save_object(here::here("data", "logit_pairs_draws_1-2_SG.rds"))