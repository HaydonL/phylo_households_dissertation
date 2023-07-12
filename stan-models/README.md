Description of different stan models

beta_mixture_DP_one_group.stan
 
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