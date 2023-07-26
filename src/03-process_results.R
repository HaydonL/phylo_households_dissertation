library(ggplot2)
library(ggpubr)
library(dplyr)
library(purrr)
library(tidyr)
library(bayesplot)
library(posterior)
source(here::here("helper-functions", "dbeta_bivariate.R"))

grid_theme <- function(){
  theme(axis.text = element_blank(),
        axis.title = element_blank())
}

#fit <- readRDS(here::here("data", "rline_1_draws_flex.rds"))
fit <- readRDS(here::here("data", "rline_1_draws_cluster_10.rds"))

# Plot sampled densities

#draws <- rstan::extract(fit, c("weights", "mus", "kappas", "psis"), 
#                        permuted = FALSE)
draws <- fit$draws(c("weights", "mus", "kappas", "psis"))

plot_density <- function(params, filename, K = 5, draw_no = NA){
  x <- seq(0, 1, by = 0.005)
  density_list <- list()
  plots <- list()
  weight_names <- paste0("weights[", 1:K, "]")
  weights <- params[weight_names]
  
  for (k in 1:K){
    grid <- expand.grid(x, x)
    mu_names <-  c(paste0("mus[", k, ",1]"), paste0("mus[", k, ",2]"))
    kappa_names <- c(paste0("kappas[", k, ",1]"), paste0("kappas[", k, ",2]"))
    
    mus <- params[mu_names]
    kappas <- params[kappa_names]
    psi <- params[paste0("psis[", k, "]")]
    grid$density <- apply(grid, 1, dbeta_bivariate, mus = mus, kappas = kappas, 
                          psi = psi)
    density_list[[k]] <- grid$density
    
    p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
      grid_theme() + labs(title = paste0("Cluster ", k, " weight = ", 
                                         signif(weights[k]), 3)) +
      scale_fill_gradient(low = "white", high = "red")
    plots[[k]] <- p
  }
  # Final plot
  full_density <- rep(0, length(grid))
  
  for (k in 1:K){
    full_density <- full_density + weights[k] * density_list[[k]]
  }
  
  grid$density <- full_density
  p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(title = paste("Final mixture", draw_no)) +
    scale_fill_gradient(low = "white", high = "red")
  plots[[K + 1]] <- p 
  
  final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 2)
  ggsave(filename, final_plot, width = 7, height = 7)
  print(final_plot)
}


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
  
for (chain in 1:4){
  draw <- draws[1, chain,]
  filename <- paste0("d1c", chain, "flex.pdf")
  plot_density(draw, filename = filename, draw_no = 1)
  cat("Chain", chain, "done")
}

draw_indices <- seq(1000, 20000, by = 1000)
for (draw_no in draw_indices){
  #draw <- draws[draw_no, 3,]
  draw <- extract_data(draws, 1, )
  filename <- paste0("d", draw_no, "flexc3.pdf")
  plot_density(draw, filename = filename, draw_no = draw_no)
  cat("Draw", draw_no, "done")
}

# Generate scatter plot of data
raw_data <- read.csv(here::here("data", "simulated", "rline_1.csv"))
pdf("rline1_scatter.pdf")
plot(raw_data)
dev.off()

# Generate plot with kde for comparison
set.seed(1001)
d <- raw_data %>% 
  magrittr::set_colnames(c("x", "y")) %>% 
  as_tibble()

kd <- ks::kde(d, compute.cont=TRUE, h=0.2)
get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%", "50%" ,"80%", "90%"), ~get_contour(kd, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
  mutate(z = c(kd$estimate %>% t))

p1 <- ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  geom_path(aes(x, y, group = prob), 
            data=dat_out, colour = I("black")) +
  geom_text(aes(label = prob), data = 
              filter(dat_out, (prob%in% c("10%") & n_val==100 | prob%in% c("20%") & n_val==80) | prob%in% c("50%") & n_val==60 | prob%in% c("80%") & n_val==40 | prob%in% c("90%") & n_val==20),
            colour = I("black"), size =I(3))+
  xlim(0, 1)+
  ylim(0, 1)+
  geom_abline(intercept=0,slope=1,color='black')+
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("kde_rline1.pdf", p1)


plot_normal <- function(params, filename, K = 5, draw_no = NA){
  x <- seq(-8, 8, by = 0.05)
  density_list <- list()
  plots <- list()
  weight_names <- paste0("weights[", 1:K, "]")
  weights <- params[weight_names]
  
  for (k in 1:K){
    grid <- expand.grid(x, x)
    mu_names <-  c(paste0("mus[", k, ",1]"), paste0("mus[", k, ",2]"))
    Sigma_names <- c(paste0("Sigmas[", k, ",1]"), paste0("Sigmas[", k, ",2]"))
    
    mus <- params[mu_names]
    Sigmas <- params[Sigma_names]
    grid$density <- apply(grid, 1, dbeta_bivariate, mus = mus, kappas = kappas, 
                          psi = psi)
    density_list[[k]] <- grid$density
    
    p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) +
      grid_theme() + labs(title = paste0("Cluster ", k, " weight = ", 
                                         signif(weights[k]), 3)) +
      scale_fill_gradient(low = "white", high = "red")
    plots[[k]] <- p
  }
  # Final plot
  full_density <- rep(0, length(grid))
  
  for (k in 1:K){
    full_density <- full_density + weights[k] * density_list[[k]]
  }
  
  grid$density <- full_density
  p <- ggplot(grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(title = paste("Final mixture", draw_no)) +
    scale_fill_gradient(low = "white", high = "red")
  plots[[K + 1]] <- p 
  
  final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 2)
  ggsave(filename, final_plot, width = 7, height = 7)
  print(final_plot)
}