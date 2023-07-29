#' Simulate points from a Poisson Process
#'
#' @param rate - a positive real number, the rate of the Poisson process
#' @param rsample - a function to sample from the density of the Poisson process
#' @param ... - further arguments to rsample
#'
#' @return A data frame containing coordinates for points from a Poisson process
#' @export
#'
#' @examples
simulate_PP <- function(rates, rsamplers, params){
  
  n_groups <- length(rates)
  coords_list <- list()
  
  for (group in 1:n_groups){
    rate <- rates[group]
    rsample <- rsamplers[[group]]
    param_list <- params[[group]]
    
    n_obs <- rpois(1, rate) # Sample total number of observations
    param_list$n <- n_obs
    
    coords <- do.call(rsample, param_list) # Sample coordinates of observations
    coords <- cbind(coords, rep(group, n_obs))
    coords_list[[group]] <- coords
  }
  
  # Combine coordinate data
  data <- as.data.frame(do.call(rbind, coords_list))
  names(data) <- c("x", "y", "group")
  return(as.data.frame(data))
}
