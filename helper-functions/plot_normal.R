library(mvtnorm)
library(ggplot2)
library(ggpubr)
source(here::here("helper-functions", "extract_data.R"))
source(here::here("helper-functions", "grid_theme.R"))
#' Process the results from the logit normal model and produce a plots of the 
#' sampled density for a given chain, draw, and group.
#'
#' @param fit - a CmdStanMCMC object containing the sampled posteriors
#' @param chain_no - a positive integer, the chain number to be plotted
#' @param draw_no - a positive integer, the iteration to be plotted
#' @param group_no - a positive integer, the group to be plotted
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
plot_normal <- function(fit, chain_no, draw_no, group_no, ages, K = 5, 
                        min_age = 15, max_age = 50, plot = TRUE){
  
  params <- extract_data(fit, chain_no, draw_no)
  # Set up grid of plot points
  x <- ages
  scaled_x <- (x - min_age) / (max_age - min_age) # linearly scaled ages
  logit_x <- log(scaled_x / (1 - scaled_x)) # logit scaled ages
  density_list <- list()
  plots <- list()
  weight_names <- paste0("weights[", group_no, ",", 1:K, "]")
  weights <- as.numeric(params[weight_names])
  
  for (k in 1:K){
    x_grid <- expand.grid(x, x)
    sx_grid <- expand.grid(scaled_x, scaled_x)
    logit_grid <- expand.grid(logit_x, logit_x)
    mu1_names <- paste0("mus_1[", group_no, ",", k, "]")
    mu2_names <- paste0("mus_2[", group_no, ",", k, "]")
    
    Sigma_ends <- c("1,1]", "1,2]", "2,1]", "2,2]")
    Sigma_names <- c()
    for (i in 1:4){
      end <- Sigma_ends[i]
      Sigma_names[i] <- paste0("Sigmas[", group_no, ",", k, ",", end)
    }
    
    mu_1 <- as.numeric(params[mu1_names])
    mu_2 <- as.numeric(params[mu2_names])
    mus <- c(mu_1, mu_2)
    Sigmas <- as.numeric(params[Sigma_names])
    Sigma <- matrix(Sigmas, nrow = 2, byrow = TRUE)
    x_grid$density <- apply(logit_grid, 1, dmvnorm, mean = mus, sigma = Sigma)
    
    x_grid$density <- x_grid$density / (sx_grid$Var1 * (1 - sx_grid$Var1) * 
                                          sx_grid$Var2 * (1 - sx_grid$Var2))
    x_grid$density <- x_grid$density / (max_age - min_age)^2
    density_list[[k]] <- x_grid$density
    
    if (plot){
      p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
        grid_theme() + labs(title = paste0("Cluster ", k, " weight = ", 
                                           signif(weights[k]), 3)) +
        scale_fill_gradient(low = "white", high = "red")
      plots[[k]] <- p
    }
  }
  # Final plot
  full_density <- rep(0, length(x_grid$density))
  
  for (k in 1:K){
    full_density <- full_density + weights[k] * density_list[[k]]
  }
  
  x_grid$density <- full_density
  
  if (plot){
    p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
      grid_theme() + labs(title = paste("Final mixture", draw_no)) +
      scale_fill_gradient(low = "white", high = "red")
    plots[[K + 1]] <- p 
    
    final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 3, 
                            common.legend = TRUE)
    #ggsave(filename, final_plot, width = 7, height = 7)
    print(final_plot)
  }
  
  return(full_density)
}