library(posterior)
#'Extract the parameters from a CmdStanMCMC fit object
#'
#' @param fit - CmdStanMCMC object from which to extract draws
#' @param chain_no - positive integer, the chain to extract from
#' @param draw_no - positive integer, the iteration to draw from
#'
#' @return a list containing the draws from the specified chain and iteration.
#' @export
#'
#' @examples
extract_data <- function(fit, chain_no, draw_no){
  draws <- fit$draws()
  draws <- as_draws_list(draws)
  chain_draw <- draws[[chain_no]]
  output <- list()
  
  for (item in names(chain_draw)){
    tmp <- chain_draw[item][[1]]
    output[item] <- tmp[draw_no]
  }
  return(output)
}