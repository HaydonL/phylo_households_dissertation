library(bayesplot)

fit_sim <- readRDS(here::here("data", "logit_sim_1_draws_ordered_SG.rds"))
fit_norm <- readRDS(here::here("data", "logit_pairs_draws_ordered_SG.rds"))
fit_12 <- readRDS(here::here("data", "logit_pairs_draws_1-2_SG_K4.rds"))


pdf("sim_traceplot.pdf")
mcmc_trace(fit_sim$draws("lp__"))
dev.off()

pdf("pairs_traceplot.pdf")
mcmc_trace(fit_norm$draws("lp__"))
dev.off()

pdf("pairs_12_traceplot.pdf")
mcmc_trace(fit_12$draws("lp__"))
dev.off()