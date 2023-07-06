#' Generate distributions centred along the line y = x
#'
#' @param n - positive integer, number of 2D observations wanted
#'
#' @return a 2 by n matrix containing observations from the required 
#' distribution  
#' @export
#'
#' @examples
rline <- function(n, sd){
  n_extra <- as.integer(1.5*n) # Initially generate more samples than needed
  output_x <- c()
  output_y <- c()
  
  # Re-sample until we have enough valid observations
  while(length(output_x) < n){
    x <- runif(n_extra)
    errors <- rnorm(n_extra, 0, sd)
    y <- x + errors
    
    # Filter out observations where y not in (0,1)
    in_bounds <- y > 0 & y < 1
    x <- x[in_bounds]
    y <- y[in_bounds]
    
    # Append valid observations
    output_x <- c(output_x, x)
    output_y <- c(output_y, y)
  }
  
  # Pick out only the first n observations and return
  output_x <- output_x[1:n]
  output_y <- output_y[1:n]
  
  return(matrix(c(output_x, output_y), ncol=2))
}