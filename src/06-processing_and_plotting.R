# This file processes the output of the stan fits and then prints and saves 
# desired plots used in the thesis as pdfs.
library(data.table)
library(ggplot2)
library(gridExtra)
library(Hmisc, include.only = "binconf")
source(here::here("helper-functions", "plot_normal.R"))
source(here::here("helper-functions", "plot_final_mixtures.R"))
source(here::here("helper-functions", "grid_theme.R"))
source(here::here("helper-functions", "sample_intensity.R"))
source(here::here("helper-functions", "pretty_scale.R"))

################################################################################
# Create plots for simulated data --
################################################################################

# Plot data from sim_1 draws
sim_1 <- read.csv(here::here("data", "simulated", "sim_1.csv"))
setDT(sim_1)
plots <- list()

for (group_no in 1:4){
  group_data <- sim_1[group == group_no]
  p <- ggplot(group_data) +  geom_point(aes(x = x, y = y)) + 
    geom_density_2d(aes(x = x, y = y)) +
    grid_theme() + labs(caption = paste("Group:", group_no)) +
    xlim(0, 1) + ylim(0, 1)
  plots[[group_no]] <- p
}

plots$ncol <- 2
grob <- do.call(arrangeGrob, plots)
final_plot <- grid.arrange(grob, ncol = 1)
ggsave("sim_1_scatter.pdf", final_plot, height = 7.2)

# Plot sampled mixture densities for uniform 

fit <- readRDS(here::here("data", "logit_sim_1_draws_ordered.rds"))
chain_no <- 1
draw_no <- 2000
group_no <- 1
ages <- seq(0 + 1e-5, 1 - 1e-5, length.out = 300) 

p <- plot_normal(fit, chain_no, draw_no, group_no, ages, min_age = 0, 
                 max_age = 1)
print(p)
ggsave("logit_sim_1_mixture_C1D2000G1.pdf", p, height = 9.55)

# Plot actual densities vs sample

ages <- seq(15 + 1e-5, 50 - 1e-5, length.out = 300)
captions <- c("Female to male, out-of-household",
              "Female to male, same household",
              "Male to female, out-of-household",
              "Male to female, same household")

# Calculate final mixture densities for each group
mixtures <- lapply(1:4, plot_normal, fit = fit_norm, chain_no = 2,
                   draw_no = 1894, ages = ages, K = 5, min_age = 15,
                   max_age = 50, plot = FALSE)

plots <- list()

for (group in 1:4){
  x_grid <- expand.grid(ages, ages)
  x_grid <- rbind(x_grid, c(100, 100))
  x_grid$density <- c(mixtures[[group]], 1.1)
  
  # Plot heatmap of density
  p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(caption = captions[group]) +
    pretty_scale() + 
    theme(legend.position = "none",
          panel.background = element_blank()) +
    xlim(15, 50) + ylim(15, 50)
  
  plots[[group]] <- p
}

plots$ncol <- 2
grob <- do.call(arrangeGrob, plots)

# Get scale
scale <- c(0, 1e-5, 5e-5, 1e-4, 5e-4, 1e-3, 5e-3, 1.1)
x_grid <- expand.grid(1:3, 1:3)
x_grid <- x_grid[1:8, ]
x_grid$density <- scale

colours <- c(
  "white",
  '#ffffcc',
  "#edf8b1",
  "#c7e9b4",
  "#41b6c4",
  "#1d91c0",
  "#225ea8",
  "#253494"
)

plot_for_legend <- ggplot(x_grid) + 
  geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
  scale_fill_gradientn(breaks = scale, labels = scale, trans = "log10", 
                       colours = colours) +
  theme(legend.position = "right",
        legend.key.height = unit(2, "cm"))

legend <- extract_legend(plot_for_legend)

final_plot <- grid.arrange(grob, legend, ncol = 2, widths = c(8, 1))
print(final_plot)
ggsave("pairs_mixtures.pdf", final_plot, height = 6.55)

################################################################################
# Create plots for pairs data --
################################################################################

# Read in data
pairs_tsi <- read.csv(here::here("data", "pairs_tsi_clean.csv"))
setDT(pairs_tsi)

# Load CmdStanR fits
fit_norm <- readRDS(here::here("data", "logit_pairs_draws_ordered.rds"))

#===============================================================================
# Plot the different mixture densities for the four groups
#===============================================================================

ages <- seq(15 + 1e-5, 50 - 1e-5, length.out = 300)
captions <- c("Female to male, out-of-household",
              "Female to male, same household",
              "Male to female, out-of-household",
              "Male to female, same household")

# Calculate final mixture densities for each group
mixtures <- lapply(1:4, plot_normal, fit = fit_norm, chain_no = 2,
                   draw_no = 1894, ages = ages, K = 5, min_age = 15,
                   max_age = 50, plot = FALSE)

source(here::here("helper-functions", "pretty_scale.R"))

plots <- list()

for (group in 1:4){
  x_grid <- expand.grid(ages, ages)
  x_grid <- rbind(x_grid, c(100, 100))
  x_grid$density <- c(mixtures[[group]], 1.1)
  
  # Plot heatmap of density
  p <- ggplot(x_grid) + geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
    grid_theme() + labs(caption = captions[group]) +
    pretty_scale() + theme(legend.position = "none") +
    xlim(15, 50) + ylim(15, 50)
  
  plots[[group]] <- p
}

plots$ncol <- 2
grob <- do.call(arrangeGrob, plots)

# Get scale
scale <- c(0, 1e-5, 5e-5, 1e-4, 5e-4, 1e-3, 5e-3, 1.1)
x_grid <- expand.grid(1:3, 1:3)
x_grid <- x_grid[1:8, ]
x_grid$density <- scale

colours <- c(
  "white",
  '#ffffcc',
  "#edf8b1",
  "#c7e9b4",
  "#41b6c4",
  "#1d91c0",
  "#225ea8",
  "#253494"
)

plot_for_legend <- ggplot(x_grid) + 
  geom_tile(aes(x = Var1, y = Var2, fill = density)) + 
  scale_fill_gradientn(breaks = scale, labels = scale, trans = "log10", 
                      colours = colours) +
  theme(legend.position = "right",
        legend.key.height = unit(2, "cm"))

legend <- extract_legend(plot_for_legend)
  
final_plot <- grid.arrange(grob, legend, ncol = 2, widths = c(8, 1))
ggsave("pairs_mixtures.pdf", final_plot, height = 6.55)

#===============================================================================
# Plot proportion of HH infections in general population
#===============================================================================

# Extract rates (eta)
eta_draws <- as_draws_matrix(fit_norm$draws("eta"))
eta_sums <- rowSums(eta_draws)

eta_prop <- eta_draws / eta_sums

# Check proportion of HH vs OOH
eta_HH_prop <- rowSums(eta_prop[, c(2,4)])
summary(eta_HH_prop)
HH_quant_bayes <- quantile(eta_HH_prop, c(0.5, 0.025, 0.975)) # 95% CI

# Calculate frequentist CIs assuming binomial data
n_obs <- pairs_tsi[, .N]
n_HH <- pairs_tsi[same_hh == 1, .N]
HH_quant_freq <- binconf(n_HH, n_obs, alpha = 0.05) # 95% CI

plot_data <- data.frame(method = c("Bayesian", "Empirical"),
                  median = c(HH_quant_bayes[1], HH_quant_freq[1]),
                  lower = c(HH_quant_bayes[2], HH_quant_freq[2]),
                  upper = c(HH_quant_bayes[3], HH_quant_freq[3]))

p <- ggplot(plot_data) + geom_bar(aes(x = method, y = median, fill = method), 
                                  stat = "identity") +
  geom_errorbar(aes(x = method, ymin = lower, ymax = upper), 
                width = 0.4, linewidth = 1.3) +
  ylim(0, 0.5) + labs(y = "Proportion", x = NULL) +
  theme(legend.position = "none")
ggsave("HH_prop.pdf", p, width = 4)

#===============================================================================
# Plot proportion of HH infections by community
#===============================================================================

# Load in pairs data with groups for this question
data_Q1_2 <- read.csv(here::here("data", "Q1_2_data.csv"))
setDT(data_Q1_2)

# Load CmdStanR fit
fit_1_2 <- readRDS(here::here("data", "logit_pairs_draws_1-2.rds"))

# Extract rates (eta)
eta_draws <- as_draws_matrix(fit_1_2$draws("eta"))
eta_sums <- rowSums(eta_draws)

eta_prop <- eta_draws / eta_sums

# Check proportion of HH vs OOH for both fishing and inland
eta_HH_fishing <- eta_prop[, 1] / rowSums(eta_prop[, c(1, 4)])
eta_HH_inland <- eta_prop[, 2] / rowSums(eta_prop[, c(2, 5)])

HH_fish_bayes <- quantile(eta_HH_fishing, c(0.5, 0.025, 0.975)) 
HH_inl_bayes <- quantile(eta_HH_inland, c(0.5, 0.025, 0.975))

# Calculate frequentist CIs assuming binomial data
n_fish <- data_Q1_2[group %in% c(1, 4), .N]
n_HH_fish <- data_Q1_2[group == 1, .N]
n_inl <- data_Q1_2[group %in% c(2, 5), .N]
n_HH_inl <- data_Q1_2[group == 2, .N]

HH_fish_freq <- binconf(n_HH_fish, n_fish, alpha = 0.05) # 95% CI
HH_inl_freq <- binconf(n_HH_inl, n_inl, alpha = 0.05) # 95% CI

method <- c("Bayesian", "Bayesian", "Empirical", "Empirical")
median <- c(HH_fish_bayes[1], HH_inl_bayes[1], HH_fish_freq[1], HH_inl_freq[1])
lower <- c(HH_fish_bayes[2], HH_inl_bayes[2], HH_fish_freq[2], HH_inl_freq[2])
upper <- c(HH_fish_bayes[3], HH_inl_bayes[3], HH_fish_freq[3], HH_inl_freq[3])
comm <- c("Fishing", "Inland", "Fishing", "Inland")

plot_data <- data.frame(method = method, median = median, lower = lower,
                        upper = upper, comm = comm)

p <- ggplot(plot_data, aes(x = comm, y = median, fill = method)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.4, linewidth = 1.3, position = position_dodge(0.9)) +
  ylim(0, 0.5) + labs(y = "Proportion", x = NULL)

ggsave("HH_prop_comm.pdf", p, width = 5)

#===============================================================================
# Plot proportion of HH infections by gender
#===============================================================================

# Extract rates (eta)
eta_draws <- as_draws_matrix(fit_norm$draws("eta"))
eta_sums <- rowSums(eta_draws)

eta_prop <- eta_draws / eta_sums

# Check proportion of HH vs OOH for both MF and FM
eta_HH_MF <- eta_prop[, 4] / rowSums(eta_prop[, c(3, 4)])
eta_HH_FM <- eta_prop[, 2] / rowSums(eta_prop[, c(1, 2)])

HH_MF_bayes <- quantile(eta_HH_MF, c(0.5, 0.025, 0.975))
HH_FM_bayes <- quantile(eta_HH_FM, c(0.5, 0.025, 0.975))

# Calculate frequentist CIs assuming binomial data
n_MF <- pairs_tsi[SEX.SOURCE == "M", .N]
n_HH_MF <- pairs_tsi[SEX.SOURCE == "M" & same_hh == 1, .N]
n_FM <- pairs_tsi[SEX.SOURCE == "F", .N]
n_HH_FM <- pairs_tsi[SEX.SOURCE == "F" & same_hh == 1, .N]

HH_MF_freq <- binconf(n_HH_MF, n_MF, alpha = 0.05) # 95% CI
HH_FM_freq <- binconf(n_HH_FM, n_FM, alpha = 0.05) # 95% CI

method <- c("Bayesian", "Bayesian", "Empirical", "Empirical")
median <- c(HH_MF_bayes[1], HH_FM_bayes[1], HH_MF_freq[1], HH_FM_freq[1])
lower <- c(HH_MF_bayes[2], HH_FM_bayes[2], HH_MF_freq[2], HH_FM_freq[2])
upper <- c(HH_MF_bayes[3], HH_FM_bayes[3], HH_MF_freq[3], HH_FM_freq[3])
gender <- c("Male", "Female", "Male", "Female")

plot_data <- data.frame(method = method, median = median, lower = lower,
                        upper = upper, gender = gender)

p <- ggplot(plot_data, aes(x = gender, y = median, fill = method)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.4, linewidth = 1.3, position = position_dodge(0.9)) +
  ylim(0, 0.5) + labs(y = "Proportion", x = NULL)

ggsave("HH_prop_gender.pdf", p, width = 5)

#===============================================================================
# Plot who is infecting young men
#===============================================================================

intensity_FM_OOH <- sample_intensity(fit_norm, 1)
intensity_FM_HH <- sample_intensity(fit_norm, 2)

FM_OOH_CIs <- apply(intensity_FM_OOH, 1, quantile, probs = c(0.5, 0.025, 0.975))
FM_HH_CIs <- apply(intensity_FM_HH, 1, quantile, probs = c(0.5, 0.025, 0.975))

prop_FM_HH <- intensity_FM_HH / (intensity_FM_HH + intensity_FM_OOH)
prop_FM_HH_CIs <- apply(prop_FM_HH, 1, quantile, probs = c(0.5, 0.025, 0.975))

# Create plots
ages <- seq(15.5, 49.5, by = 1)
FM_OOH_CIs <- as.data.frame(t(FM_OOH_CIs))
names(FM_OOH_CIs) <- c("median", "LQ", "UQ")
FM_OOH_CIs$ages <- ages
FM_HH_CIs <- as.data.frame(t(FM_HH_CIs))
names(FM_HH_CIs) <- c("median", "LQ", "UQ")
FM_HH_CIs$ages <- ages

p <- ggplot() + geom_line(aes(x = ages, y = median, color = "Out-of-household"), 
                          data = FM_OOH_CIs, size = 1) +
  geom_ribbon(aes(x = ages, ymin = LQ, ymax = UQ, fill = "Out-of-household"), 
              data = FM_OOH_CIs, alpha = 0.4) +
  geom_line(aes(x = ages, y = median, color = "Within household"), 
            data = FM_HH_CIs, size = 1) +
  geom_ribbon(aes(x = ages, ymin = LQ, ymax = UQ, fill = "Within-household"), 
              data = FM_HH_CIs, alpha = 0.4) +
  labs(x = "Age of source (Female)", y = "Relative intensity",
       title = "FM transmission HH vs non-HH intensity",
       caption = "Among young (16-24) recipients",
       color = NULL)
print(p)
ggsave("FM_HH_age_intensity.pdf", p)

#===============================================================================
# Plot who is infecting young women
#===============================================================================

intensity_MF_OOH <- sample_intensity(fit_norm, 3)
intensity_MF_HH <- sample_intensity(fit_norm, 4)

MF_OOH_CIs <- apply(intensity_MF_OOH, 1, quantile, probs = c(0.5, 0.025, 0.975))
MF_HH_CIs <- apply(intensity_MF_HH, 1, quantile, probs = c(0.5, 0.025, 0.975))

prop_MF_HH <- intensity_MF_HH / (intensity_MF_HH + intensity_MF_OOH)
prop_MF_HH_CIs <- apply(prop_MF_HH, 1, quantile, probs = c(0.5, 0.025, 0.975))

# Create plots
ages <- seq(15.5, 49.5, by = 1)
MF_OOH_CIs <- as.data.frame(t(MF_OOH_CIs))
names(MF_OOH_CIs) <- c("median", "LQ", "UQ")
MF_OOH_CIs$ages <- ages
MF_HH_CIs <- as.data.frame(t(MF_HH_CIs))
names(MF_HH_CIs) <- c("median", "LQ", "UQ")
MF_HH_CIs$ages <- ages

p <- ggplot() + geom_line(aes(x = ages, y = median, color = "Out-of-household"), 
                          data = MF_OOH_CIs) +
  geom_ribbon(aes(x = ages, ymin = LQ, ymax = UQ, fill = "Out-of-household"), 
              data = MF_OOH_CIs, alpha = 0.4) +
  geom_line(aes(x = ages, y = median, color = "Within household"), 
            data = MF_HH_CIs) +
  geom_ribbon(aes(x = ages, ymin = LQ, ymax = UQ, fill = "Within-household"), 
              data = MF_HH_CIs, alpha = 0.4) +
  labs(x = "Age of source (Male)", y = "Relative intensity",
       title = "MF transmission HH vs non-HH intensity",
       caption = "Among young (16-24) recipients",
       color = NULL)
print(p)
ggsave("MF_HH_age_intensity.pdf", p)
