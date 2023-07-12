#' Calculate density of 2D beta (proportion) distribution
#'
#' @param x - vector of length 2, the data. Must be bounded between (0,1)
#' @param mus - vector of length 2, the mean parameter. Must also be bounded 
#'              between (0,1)
#' @param kappas - vector of length two, the size parameter. Must be positive
#' @param psi - real. Must be bounded between legal values determined by mus
#'
#' @return real, the value of the pdf at given data point given parameters
#' @export
#'
#' @examples
#' beta_bivariate(c(0.5, 0.5), c(0.5, 0.5), c(1, 0.2), 0.2)
library(extraDistr, include.only = "dprop") # load 1D beta proportion pdf
dbeta_bivariate <- function(x, mus, kappas, psi){
  density <- (1 + psi * (x[1] - mus[1]) * (x[2] - mus[2])) * # correlation term
    prod(dprop(x, kappas, mus)) # beta proportion pdfs
  return(density)
}

