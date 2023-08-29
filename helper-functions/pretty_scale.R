library(ggplot2)
library(scales)
#' Produces pretty scaling object for ggplot2
#'
#'
#' @return
#' @export
#'
#' @examples
pretty_scale <- function(){
  scale <- c(0, 1e-5, 5e-5, 1e-4, 5e-4, 1e-3, 5e-3, 1.1)
  scale <- rescale(scale)
  
  scale_fill_gradientn(colours = (c(
    "white",
    '#ffffcc',
    #"#ffffd9",
    "#edf8b1",
    "#c7e9b4",
    #"#7fcdbb",
    "#41b6c4",
    "#1d91c0",
    "#225ea8",
    "#253494"
    # "#081d58"
  )),
  values = scale,
  na.value = "grey90")
}
