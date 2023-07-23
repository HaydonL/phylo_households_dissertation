
# The models

This folder contains all the stan code necessary to run the models. A
description of the models is as follows:

- `beta_count_2D.stan` : A Poisson Process model with a single bivariate beta 
density for one group on observations.
- `beta_mixture_DP_one_group.stan` : A Poisson Process with a Dirichlet process
mixture of up to $K$ (user-specified in the data input) clusters of bivariate
beta distributions.
- `beta_mixture_SG_one_group.stan` : Same as above but with a Stirling-Gamma
process/prior.
- `logit_gaussian_mixture_DP_one_group` : As above but with a DP mixture of
Gaussian distributions for data transformed by a linear map to (0, 1) and then a
logit transformation. (CODE WIP)

The subfolder named `examples` contains stan code from various sources and used 
as reference material when building my own models.
 
WIP below:
\[
    \begin{aligned}
    a &\sim PP(\eta \pi()) \\
    \eta &\sim U(0, \infty) \\
    \pi() = \sum^K_{k=1} w_k pi_k() \\
    pi_k() = \text{Beta}(\underbrace{\mu_{k,1}, \mu_{k,2}, \kappa_{k,1}, \kappa_{k,2}, \psi_k}_{G_k})
    G_k \sim DP(\alpha, G_0)

    \mu
    \end{aligned}

\]