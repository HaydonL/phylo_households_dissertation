# This script simulates samples from a 2D poisson process on a bounded square
# and then stores the dataframes as csv files.

# Load in helper functions
source(here::here("helper-functions", "simulate_PP.R"))
source(here::here("helper-functions", "runif2D.R"))
source(here::here("helper-functions", "rline.R"))
source(here::here("helper-functions", "roffset.R"))

outdir <- here::here("data", "simulated")
set.seed(5417326)
n_datasets <- 50 # number of generated data sets

# Fix parameters for generated data sets
rates <- rep(150, 4) 
rsamplers <- list(runif2D, rline, rline, roffset)
params <- list()

params[[1]] <- list()
params[[2]] <- list(sd = 0.1)
params[[3]] <- list(sd = 0.03)
params[[4]] <- list(sd = 0.05, offset = 0.3)

# Generate data
for (dataset_no in 1:n_datasets){
  filename <- paste0("/sim_", as.character(dataset_no), ".csv")
  data <- simulate_PP(rates, rsamplers, params)
  write.csv(data, paste0(outdir, filename), row.names = FALSE)
}

