library(ggplot2)
source(here::here("helper-functions/p_valid.R"))
source(here::here("helper-functions/plot_normal.R"))
# Plot actual simulated densities

g2_const <- p_valid(0.1)
g3_const <- p_valid(0.03)

integrand <- function(x){
  pnorm((1- x - 0.3)/0.05) - pnorm((-x - 0.3)/0.05)
}

g4_const <- integrate(integrand, 0, 0.7)$value

x <- seq(1e-7, 1 - 1e-7, length.out = 300)
x_grid <- expand.grid(x, x)

n_points <- dim(x_grid)[1]
densities <- list()

# Calculate densities
densities[[1]] <- rep(1, n_points)

densities[[2]] <- apply(x_grid, 1, function(input){ 
  x <- input[1]
  y <- input[2]
  dnorm(y, mean = x, sd = 0.1)/g2_const
  })

densities[[3]] <- apply(x_grid, 1, function(input){ 
  x <- input[1]
  y <- input[2]
  dnorm(y, mean = x, sd = 0.03)/g3_const
})

densities[[4]] <- apply(x_grid, 1, function(input){ 
  x <- input[1]
  y <- input[2]
  dnorm(y, mean = x + 0.3, sd = 0.05)/g4_const
})

fit <- readRDS(here::here("data", "logit_sim_1_draws_ordered.rds"))
mixtures <- lapply(1:4, plot_normal, fit = fit, chain_no = 3,
                   draw_no = 3328, ages = x, K = 5, min_age = 0,
                   max_age = 1, plot = FALSE)

max_point <- max(unlist(data))

for (plot_no in 1:4){
  x_grid <- expand.grid(x, x)
  x_grid <- rbind(x_grid, c(100, 100))
  x_grid$density <- c(data[[plot_no]], max_point)
  
  # Plot heatmap of density
  p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(caption = captions[group]) +
    pretty_scale() + 
    theme(legend.position = "none",
          panel.background = element_blank()) +
    xlim(15, 50) + ylim(15, 50)
  
  plots[[group]] <- p
}