library(ggplot2)
library(gridExtra)
source(here::here("helper-functions/p_valid.R"))
source(here::here("helper-functions/plot_normal.R"))
# Plot actual simulated densities

g2_const <- p_valid(0.1)
g3_const <- p_valid(0.03)

integrand <- function(x){
  pnorm((1- x - 0.3)/0.05) - pnorm((-x - 0.3)/0.05)
}

g4_const <- integrate(integrand, 0, 0.7)$value

x <- seq(0.002, 0.998, length.out = 300)
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

fit <- readRDS(here::here("data", "logit_sim_1_draws_ordered_SG.rds"))

mixtures <- lapply(1:4, plot_normal, fit = fit, chain_no = 2,
                   draw_no = 4328, ages = x, K = 5, min_age = 0,
                   max_age = 1, plot = FALSE)

for(i in mixtures){
  print(max(i))
}

for(i in densities){
  print(max(i))
}

#pretty <- scale_fill_gradientn(colours = c(
#  "white",
#  "#ffffd9",
#  "#7fcdbb",
#  "#225ea8",
#  "#253494",
#  "#081d58"
#),
#values = seq(0, 1, by = 0.2),
#na.value = "grey90")

max_points <- c(6.5, 20, 138, 75)
max_densities <- c(1.5, 5, 15, 12)

vals <- list()
vals[[1]] <- c(0, 0.5, 1, 1.5, 6.5)
vals[[2]] <- c(0, 2, 4, 6, 20)
vals[[3]] <- c(0, 5, 10, 15, 138)
vals[[4]] <- c(0, 4, 8, 12, 75)

pretties <- list()

for (i in 1:4){
  pretties[[i]] <- scale_fill_gradientn(colours = c(
    "white",
    "#ffffd9",
    "#7fcdbb",
    "#225ea8",
    "#253494",
    "#081d58"
  ),
  values = rescale(vals[[i]]),
  na.value = "grey90")
}
plots <- list()

for (plot_no in 1:4){
  
  pretty <- pretties[[plot_no]]
  max_point <- max_points[[plot_no]]
  
  x_grid <- expand.grid(x, x)
  x_grid <- rbind(x_grid, c(100, 100), c(-100, -100))
  x_grid$density <- c(densities[[plot_no]], max_point, 0)
  
  # Plot heatmap of density
  p1 <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    labs(caption = paste("Actual density: Group", plot_no)) +
    pretty + 
    theme(legend.position = "none",
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.line = element_line(),
          axis.title = element_blank()) +
    xlim(0, 1) + ylim(0, 1)
  
  plots[[2 * plot_no - 1]] <- p1
  
  x_grid <- expand.grid(x, x)
  x_grid <- rbind(x_grid, c(100, 100), c(-100, -100))
  x_grid$density <- c(mixtures[[plot_no]], max_point, 0)
  
  p2 <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    labs(caption = paste("Sampled density: Group", plot_no)) +
    pretty + 
    theme(legend.position = "bottom",
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.line = element_line(),
          axis.title = element_blank()) +
    xlim(0, 1) + ylim(0, 1)
  
  plots[[2 * plot_no]] <- p2
}

plots$ncol <- 4
plots$nrow <- 2
plots$heights <- c(7.3, 9.4)
plots$as.table <- FALSE
grob <- do.call(grid.arrange, plots)
#final_plot <- grid.arrange(grob)

ggsave("sim_comparison_flip.pdf", grob, width = 10, height = 6.2)
