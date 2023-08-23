source(here::here("helper-functions", "plot_normal.R"))
source(here::here("helper-functions", "pretty_scale.R"))
#' Plot final mixture densities for all groups
#'
#' @param fit - a CmdStanMCMC object containing the sampled posteriors
#' @param chain_no - a positive integer, the chain number to be plotted
#' @param draw_no - a positive integer, the iteration to be plotted
#' @param N_groups - a positive integer, the total number of groups
#' @param ages - 
#' @param K - a positive integer, the number of clusters in the model (default 
#'            is five)
#' @param min_age a positive integer, the minimum age of observations
#' @param max_age a positive integer, the maximum age of observations
#'
#' @return
#' @export
#'
#' @examples
plot_final_mixtures <- function(mixtures, N_groups, ages, captions,
                                K = 5, min_age = 15, max_age = 50){
  for (item in mixtures){
    print(sum(item))
  }
  
  max_point <- max(unlist(mixtures))
  scale_gradient_thing <- pretty_scale(min_age, max_age, max_point)
  plots <- list()
  
  # Create plots for each group
  for (group in 1:N_groups){
    # Create data frame with densities
    x_grid <- expand.grid(ages, ages)
    x_grid$density <- mixtures[[group]]
    
    # Plot heatmap of density
    p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
      grid_theme() + labs(caption = captions[group]) +
      scale_gradient_thing #+ 
      #theme(legend.position = "none")
    force(p)
    plots[[group]] <- p 
    
    #viridis::scale_fill_viridis()
  }
  
  return(plots)
  # Extract legend
  p_legend <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, 
                                             fill = density)) + 
    grid_theme() + labs(caption = paste("Group", group)) +
    pretty_scale(min_age, max_age, max_point) +  
    theme(legend.position = "left")
  shared_legend <- extract_legend(p_legend)
  
  # Combine all the plots into one including legend
  plots$ncol <- 2
  grob <- do.call(arrangeGrob, plots)
  final_plot <- grid.arrange(grob, shared_legend, ncol = 2, 
                             widths = c(8, 1), heights = c(15))
  return(final_plot)
}