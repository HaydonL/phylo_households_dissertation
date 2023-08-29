
p_valid <- function(sigma){
  phis <- function(x){
    pnorm((1-x)/sigma) - pnorm(-x/sigma)
  }
  integrate(phis, lower = 0, upper = 1)$value
}
