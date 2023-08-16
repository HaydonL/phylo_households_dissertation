# This file processes the output of the stan fits and then prints and saves 
# desired plots used in the thesis as pdfs.
library(data.table)
library(ggplot2)
library(gridExtra)
source(here::here("helper-functions", "plot_normal.R"))
source(here::here("helper-functions", "grid_theme.R"))

# Plot data from sim_1 draws
sim_1 <- read.csv(here::here("data/simulated/sim_1.csv"))
setDT(sim_1)
plots <- list()

for (group_no in 1:4){
  group_data <- sim_1[group == group_no]
  p <- ggplot(group_data) +  geom_point(aes(x = x, y = y)) + 
    grid_theme() + labs(caption = paste("Group:", group_no))
  plots[[group_no]] <- p
}

plots$ncol <- 2
grob <- do.call(arrangeGrob, plots)
final_plot <- grid.arrange(grob, ncol = 2)

fit <- readRDS(here::here("data", "logit_sim_1_draws_ordered.rds"))
chain_no <- 1
draw_no <- 2000
group_no <- 1
ages <- seq(15 + 1e-5, 50 - 1e-5, length.out = 300) 

p <- plot_normal(fit, chain_no, draw_no, group_no, ages)
print(p)
ggsave("logit_sim_1_mixture_C1D2000G1.pdf", p, height = 9.55)
