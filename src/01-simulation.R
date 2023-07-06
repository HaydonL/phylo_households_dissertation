# This script simulates samples from a 2D poisson process on a bounded square
# and then stores the dataframes as csv files.

# Load in helper functions
source(here::here("helper-functions", "simulate_PP.R"))
source(here::here("helper-functions", "runif2D.R"))
source(here::here("helper-functions", "rline.R"))

outdir <- here::here("data", "simulated")
set.seed(5417326)
rate <- 200
data <- simulate_PP(rate, rline, sd = 0.1)

write.csv(data, paste0(outdir, "/rline_1.csv"), row.names = FALSE)
