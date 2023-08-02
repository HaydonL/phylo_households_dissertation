library(ggplot2)
library(ggpubr)
source(here::here("helper-functions", "extract_data.R"))
source(here::here("helper-functions", "grid_theme.R"))
source(here::here("helper-functions", "dbeta_bivariate.R"))
#' Process the results from the beta model and produce a plots of the 
#' sampled density for a given chain, draw, and group.
#'
#' @param fit - a CmdStanMCMC object containing the sampled posteriors
#' @param chain_no - a positive integer, the chain number to be plotted
#' @param draw_no - a positive integer, the iteration to be plotted
#' @param group_no - a positive integer, the group to be plotted
#' @param filename - string, a filename to save the resulting figure (unused for 
#'                  now) 
#' @param K - a positive integer, the number of clusters in the model (default 
#'            is five)
#' @param min_age a positive integer, the minimum age of observations
#' @param max_age a positive integer, the maximum age of observations
#'
#' @return
#' @export
#'
#' @examples
plot_density <- function(fit, chain_no, draw_no, group_no, filename, K = 5, 
                         min_age = 15, max_age = 50){
  
  params <- extract_data(fit, chain_no, draw_no)
  # Set up grid of plot points
  x <- seq(min_age + 1e-5, max_age - 1e-5, length.out = 300)
  scaled_x <- (x - min_age) / (max_age - min_age) # linearly scaled ages
  density_list <- list()
  plots <- list()
  weight_names <- paste0("weights[", group_no, ",", 1:K, "]")
  weights <- as.numeric(params[weight_names])
  
  for (k in 1:K){
    x_grid <- expand.grid(x, x)
    sx_grid <- expand.grid(scaled_x, scaled_x)
    mu_names <-  c(paste0("mus[", group_no, ",", k, ",1]"), 
                   paste0("mus[", group_no, ",", k, ",2]"))
    kappa_names <-  c(paste0("kappas[", group_no, ",", k, ",1]"), 
                   paste0("kappas[", group_no, ",", k, ",2]"))
    psi_name <-  paste0("psis[", group_no, ",", k, "]")
    
    mus <- params[mu_names]
    kappas <- params[kappa_names]
    psi <- params[psi_names]
    x_grid$density <- apply(sx_grid, 1, dbeta_bivariate, mus = mus, 
                            kappas = kappas, psi = psi)
    density_list[[k]] <- x_grid$density
    
    p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
      grid_theme() + labs(title = paste0("Cluster ", k, " weight = ", 
                                         signif(weights[k]), 3)) +
      scale_fill_gradient(low = "white", high = "red")
    plots[[k]] <- p
  }
  # Final plot
  full_density <- rep(0, length(x_grid$density))
  
  for (k in 1:K){
    full_density <- full_density + weights[k] * density_list[[k]]
  }
  
  x_grid$density <- full_density
  p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(title = paste("Final mixture", draw_no)) +
    scale_fill_gradient(low = "white", high = "red")
  plots[[K + 1]] <- p 
  
  final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 3)
  #ggsave(filename, final_plot, width = 7, height = 7)
  print(final_plot)
}