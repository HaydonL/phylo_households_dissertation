library(parallel)
library(posterior)
library(data.table)
source(here::here("helper-functions", "plot_normal.R"))
source(here::here("helper-functions", "extract_data.R"))
#' Evaluate a sample of the relative intensities for young recipients
#'
#' @param fit - a CmdStanMCMC object containing the sampled posteriors
#' @param group - a positive integer, the group to be plotted
#'
#' @return intensities - a matrix of samples intensities, one row corresponds to
#' one age band.
#' @export
#'
#' @examples
sample_intensity <- function(fit, group){
  
  # Create grid of ages and list of draw numbers
  ages <- seq(15.5, 49.5, by = 1)
  draws_per_chain <- draws <- seq(10, 5000, by = 10) 
  draws <- seq(10, 20000, by = 10)
  grid <- expand.grid(ages, ages)
  
  # Create cluster for parallel programming
  n <- detectCores() - 1
  cl <- makeCluster(n)
  clusterExport(cl, list("plot_normal", "extract_data"))
  clusterEvalQ(cl, library(posterior))
  clusterEvalQ(cl, library(mvtnorm))
  eta_draws <- as_draws_matrix(fit$draws("eta"))
  
  # Calculate densities for iterations
  intensity_list <- list()
  for (chain_no in 1:4){
    intensities_tmp <- parLapply(cl, draws_per_chain, plot_normal, fit = fit, 
                          chain_no = chain_no, group_no = group, 
                          ages = ages, plot = FALSE)
    intensity_list[[chain_no]] <- intensities_tmp
  }
  intensities <- unlist(intensity_list, recursive = FALSE)
  etas <- as.numeric(eta_draws[draws, group])
  
  stopCluster(cl)
  # Calculate intensities 
  for (index in 1:length(etas)){
    intensities[[index]] <- etas[index] * intensities[[index]]
  }
  
  # Extract infections for young people
  recip_15_24_indices <- which(grid$Var2 < 24) # X: Source, Y: Recipient
  
  # Calculate intensities among young people by age of source
  for (index in 1:length(intensities)){
    yng_grid <- grid[recip_15_24_indices, ]
    setDT(yng_grid)
    yng_grid[, intensity := intensities[[index]][recip_15_24_indices]] 
    yng_grid[, grouped_int := sum(intensity), by = Var1]
    
    # Remove Var2 (Recipient) and intensity
    yng_grid[, Var2 := NULL]
    yng_grid[, intensity := NULL]
    yng_grid <- unique(yng_grid)
    intensities[[index]] <- yng_grid[,grouped_int]
  }
  
  intensities <- matrix(unlist(intensities), nrow = length(ages))
  return(intensities)
}
