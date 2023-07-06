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
simulate_PP <- function(rate, rsample, ...){
  n_obs <- rpois(1, rate) # Sample total number of observations
  coords <- rsample(n_obs, ...) # Sample coordinates of observations
  return(as.data.frame(coords))
}