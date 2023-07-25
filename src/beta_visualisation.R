source(here::here("helper-functions", "dbeta_bivariate.R"))
library(ggplot2)
library(ggpubr)

grid_theme <- function(){
  theme(axis.text = element_blank(),
        axis.title = element_blank())
}

plot_beta <- function(mus, kappas, psi){
  x <- seq(0, 1, by = 0.005)
  grid <- expand.grid(x, x)
  grid$density <- apply(grid, 1, dbeta_bivariate, mus = mus, kappas = kappas, 
                        psi = psi)
  
  p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
    labs(title = paste("Params:", mus[1], mus[2], kappas[1], kappas[2], psi, 
                       sep = ", "),
         subtitle = paste0("Correlation: ", beta_corr(mus, kappas, psi))) +
    grid_theme()
  return(p)
}

psi_bounds <- function(mus){
  bounds <- numeric(2)
  min_array <- numeric(2)
  max_array <- numeric(2)
  
  min_array[1] <- mus[1] * mus[2]
  min_array[2] <- (1 - mus[1])*(1 - mus[2])
  max_array[1] <- mus[1] * (mus[2] - 1)
  max_array[2] <- mus[2] * (mus[1] - 1)
  
  bounds[1] <- -(1/max(min_array))
  bounds[2] <- -(1/min(max_array))
  
  return(bounds)
}

beta_corr <- function(mus, kappas, psi){
  corr <- mus[1] * (1 - mus[1]) * mus[2] * (1 - mus[2])
  corr <- corr * 1/(1 + kappas[1]) * 1/(1 + kappas[2])
  corr <- psi * sqrt(corr)
  return(corr)
}

mus <- c(0.5, 0.9)
kappas <- c(10, 0.1)
psi <- 0.7

plot_beta(mus, kappas, psi)
psi_bounds(mus)
# Need psi > - (1/ min(mu1 - mu2))

kappa_list <- list(c(0.5, 0.5), c(1, 1), c(2, 2), c(5, 5), c(10, 10), c(20, 20))

plots <- lapply(kappa_list, plot_beta, mus = mus, psi = psi)

ggarrange(plotlist = plots, nrow = 3, ncol = 2)
ggsave("beta_visualisation.pdf", width = 8, height = 8)
