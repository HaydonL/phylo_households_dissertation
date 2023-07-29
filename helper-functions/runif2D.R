#' Simulate points uniformly on a 2D grid
#'
#' @param n - positive integer, number of 2D observations wanted
#'
#' @return a 2 by n matrix containing observations from a uniform distribution 
#' @export
#'
#' @examples
runif2D <- function(n){
  data <- matrix(runif(2*n), ncol = 2)
  return(data)
}
