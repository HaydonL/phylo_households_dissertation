library(ggplot2)
library(ggpubr)
library(dplyr)
library(purrr)
library(tidyr)
library(bayesplot)
library(posterior)
library(mvtnorm)

grid_theme <- function(){
  theme(axis.text = element_blank())
}

fit <- readRDS(here::here("data", "logit_SG_IW_fit.rds"))

draws <- fit$draws(c("weights", "mus", "Sigmas"))

# Extract data from cmdstanr object
extract_data <- function(draws, chain_no, draw_no){
  draws <- as_draws_list(draws)
  chain_draw <- draws[[chain_no]]
  output <- list()
  
  for (item in names(chain_draw)){
    tmp <- chain_draw[item][[1]]
    output[item] <- tmp[draw_no]
  }
  return(output)
}

# Plot densities

plot_normal <- function(params, filename, K = 5, draw_no = NA){
  x <- seq(-6, 6, by = 0.1)
  density_list <- list()
  plots <- list()
  weight_names <- paste0("weights[", 1:K, "]")
  weights <- as.numeric(params[weight_names])
  
  for (k in 1:K){
    grid <- expand.grid(x, x)
    mu_names <-  c(paste0("mus[", k, ",1]"), paste0("mus[", k, ",2]"))
    
    Sigma_ends <- c("1,1]", "1,2]", "2,1]", "2,2]")
    Sigma_names <- c()
    for (i in 1:4){
      end <- Sigma_ends[i]
      Sigma_names[i] <- paste0("Sigmas[", k, ",", end)
    }
    
    mus <- as.numeric(params[mu_names])
    Sigmas <- as.numeric(params[Sigma_names])
    Sigma <- matrix(Sigmas, nrow = 2, byrow = TRUE)
    grid$density <- apply(grid, 1, dmvnorm, mean = mus, sigma = Sigma)
    density_list[[k]] <- grid$density
    
    p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
      grid_theme() + labs(title = paste0("Cluster ", k, " weight = ", 
                                         signif(weights[k]), 3)) +
      scale_fill_gradient(low = "white", high = "red")
    plots[[k]] <- p
  }
  # Final plot
  full_density <- rep(0, length(grid$density))
  
  for (k in 1:K){
    full_density <- full_density + weights[k] * density_list[[k]]
  }
  
  grid$density <- full_density
  p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(title = paste("Final mixture", draw_no)) +
    scale_fill_gradient(low = "white", high = "red")
  plots[[K + 1]] <- p 
  
  final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 3)
  #ggsave(filename, final_plot, width = 7, height = 7)
  print(final_plot)
}

params <- extract_data(draws, 3, 1000)
plot_normal(params, "logit_SG_IW_plots.pdf", K = 6)
