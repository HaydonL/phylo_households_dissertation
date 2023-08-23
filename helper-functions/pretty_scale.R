library(ggplot2)
library(scales)
#' Produces pretty scaling object for ggplot2
#'
#' @param max_point - positive real - maximum value of density
#'
#' @return
#' @export
#'
#' @examples
pretty_scale <- function(min_age, max_age, max_point){
  factor <- (max_age - min_age)^2 
  scale <- c(0, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1) / 35^2
  scale <- c(factor * scale, max_point)
  
  scale_fill_gradientn(colours = (c(
    "white",
    '#ffffcc',
    "#ffffd9",
    "#edf8b1",
    "#c7e9b4",
    "#7fcdbb",
    "#41b6c4",
    "#1d91c0",
    "#225ea8",
    "#253494"
    # "#081d58"
  )),
  values = scale,
  na.value = "grey90")
}
