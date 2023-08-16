#' Produce ggplot2 theme for making density plots
#'
#' @return - a ggplot2 theme
#' @export
#'
#' @examples
grid_theme <- function(){
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(size = 10))
}
