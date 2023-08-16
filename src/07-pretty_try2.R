library(tidyverse)
source(here::here("helper-functions", "plot_normal.R"))
source(here::here("helper-functions", "extract_data.R"))
source(here::here("helper-functions", "extract_legend.R"))

#data <- list(
#  sheet1 = data.frame(
#    category = c(paste0("rep_", 1:10)),
#    value1 = rnorm(10),
#    value2 = rnorm(10)),
#  sheet2 = data.frame(
#    category = c(paste0("rep_", 1:10)),
#    value1 = rnorm(10),
#    value2 = rnorm(10)),
#  sheet3 = data.frame(
#    category = c(paste0("rep_", 1:10)),
#    value1 = rnorm(10),
#    value2 = rnorm(10)))
#
#data <- bind_rows(data, .id = "id") %>% 
#  pivot_longer(c(-id, -category))
#
#ggplot(data, aes(x= category, y=name)) +
#  geom_tile(aes(fill = value)) + 
#  facet_wrap(~id) +
#  ggtitle("One title for all")+
#  theme(plot.title = element_text(hjust = 0.5))


fit <- readRDS(here::here("data", "logit_pairs_draws_ordered.rds"))
chain_no <- 1
draw_no <- 1
group_no <- 1
ages <- seq(15 + 1e-5, 50 - 1e-5, length.out = 300) 
K <- 5
min_age <- 15
max_age <- 50


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
  

  p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
    grid_theme() + #labs(title = paste0("Cluster ", k, " weight = ", 
                   #                    signif(weights[k]), 3)) +
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
  grid_theme() + #labs(title = paste("Final mixture", draw_no)) +
  scale_fill_gradient(low = "white", high = "red")
plots[[K + 1]] <- p 

final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 3, 
                        common.legend = TRUE)
#ggsave(filename, final_plot, width = 7, height = 7)

name <- "a"
storage = list()
storage[[name]] <- 1
