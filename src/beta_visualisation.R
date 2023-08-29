source(here::here("helper-functions", "dbeta_bivariate.R"))
library(ggplot2)
library(gridExtra)

grid_theme <- function(){
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())
}

plot_beta <- function(mus, kappas, psi){
  x <- seq(0 , 1, length.out = 300)
  grid <- expand.grid(x, x)
  grid$density <- apply(grid, 1, dbeta_bivariate, mus = mus, kappas = kappas, 
                        psi = psi)
  
  p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
    labs(title = paste("Parameters:", mus[1], mus[2], kappas[1], kappas[2], psi, 
                       sep = ", "),
         subtitle = paste0("Correlation: ", round(beta_corr(mus, kappas, psi), 3))) +
    grid_theme() + scale_fill_continuous(trans = "log10", type = "viridis")
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

mus <- c(0.5, 0.5)
kappas <- c(1, 1)
psi <- 4

plot_beta(mus, kappas, psi)
psi_bounds(mus)
# Need psi > - (1/ min(mu1 - mu2))


plots <- list()

plots[[1]] <- plot_beta(mus = c(0.5, 0.5), kappas = c(1, 1), psi = 0)
plots[[2]] <- plot_beta(mus = c(0.5, 0.5), kappas = c(1, 1), psi = 4)
plots[[3]] <- plot_beta(mus = c(0.5, 0.5), kappas = c(5, 5), psi = 0)
plots[[4]] <- plot_beta(mus = c(0.2, 0.8), kappas = c(5, 5), 
                        psi = psi_bounds(c(0.2, 0.8))[1])

plots$ncol <- 2

grob <- do.call(arrangeGrob, plots)
final_plot <- grid.arrange(grob, ncol = 1)
print(final_plot)
ggsave("beta_visualisation.pdf", final_plot, width = 9, height = 8)

