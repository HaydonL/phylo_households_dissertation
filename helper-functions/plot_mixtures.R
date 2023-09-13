library(mvtnorm)
library(ggplot2)
library(gridExtra)
source(here::here("helper-functions", "extract_data.R"))
source(here::here("helper-functions", "grid_theme.R"))
source(here::here("helper-functions", "extract_legend.R"))
source(here::here("helper-functions", "pretty_scale.R"))
#' Process the results from the logit normal model and produce a plots of the 
#' sampled density for a given chain, draw, and group. Alternatively return the
#' mixture density if not plotting
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
plot_mixtures <- function(fit, chain_no, draw_no, group_no, ages, K = 5, 
                        min_age = 15, max_age = 50, plot = TRUE){
  cat("Plotting group:", group_no, "\n")
  cat("Chain:", chain_no, "\n")
  cat("Draw:", draw_no, "\n")
  params <- extract_data(fit, chain_no, draw_no)
  # Set up grid of plot points
  x <- ages
  scaled_x <- (x - min_age) / (max_age - min_age) # linearly scaled ages
  logit_x <- log(scaled_x / (1 - scaled_x)) # logit scaled ages
  density_list <- list()
  weight_names <- paste0("weights[", group_no, ",", 1:K, "]")
  weights <- as.numeric(params[weight_names])
  
  # Calculate densities for each cluster
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
    cat("Cluster", k, "done\n")
  }
  
  # Calculate final mixture density
  full_density <- rep(0, length(x_grid$density))
  
  for (k in 1:K){
    full_density <- full_density + weights[k] * density_list[[k]]
  }
  
  # Generate plot if required
  if (plot){
    max_point <- max(unlist(density_list))
    plots <- list()
    
    # Store plots for each individual cluster
    for (k in 1:K){
      x_grid$density <- density_list[[k]]
      p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
        grid_theme() + labs(caption = paste0("weight = ", round(weights[k], 3))) +
        theme(legend.position = "none")
      plots[[k]] <- p
    }
    
    # Generate plot for final mixture
    x_grid$density <- full_density
    p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
      grid_theme() + labs(caption = "Final mixture") +
      theme(legend.position = "none")
    plots[[K + 1]] <- p 
    
    # Generate plot just for the legend
    p_legend <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, 
                                               fill = density)) + 
      grid_theme() + labs(caption = "Final mixture") +
      theme(legend.position = "left")
    shared_legend <- extract_legend(p_legend)
    
    # Combine all the plots into one including legend
    plots$ncol <- 2
    grob <- do.call(arrangeGrob, plots)
    final_plot <- grid.arrange(grob, shared_legend, ncol = 2, 
                               widths = c(8, 1), heights = c(28))
    return(final_plot)
  }
  
  density_list[[K + 1]] <- full_density
  # Return density if not plotting
  
  output <- list()
  output$weights <- round(weights, digits = 4)
  output$densities <- density_list
  
  return(output)
}
