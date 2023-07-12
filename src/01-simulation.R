# This script simulates samples from a 2D poisson process on a bounded square
# and then stores the dataframes as csv files.

# Load in helper functions
source(here::here("helper-functions", "simulate_PP.R"))
source(here::here("helper-functions", "runif2D.R"))
source(here::here("helper-functions", "rline.R"))

outdir <- here::here("data", "simulated")
set.seed(5417326)
rate <- 200
n_datasets <- 100

for (dataset_no in 1:n_datasets){
  filename <- paste0("/rline_", as.character(dataset_no), ".csv")
  data <- simulate_PP(rate, rline, sd = 0.1)
  write.csv(data, paste0(outdir, filename), row.names = FALSE)
  
  filename <- paste0("/runif2D_", as.character(dataset_no), ".csv")
  data <- simulate_PP(rate, runif2D)
  write.csv(data, paste0(outdir, filename), row.names = FALSE)
}



