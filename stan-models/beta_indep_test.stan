functions {
  real beta_bivariate_lpdf(row_vector age, real mu1, real kappa1, real mu2, real kappa2)
  {
  real density;
  density = beta_proportion_lpdf(age[1] | mu1, kappa1) + beta_proportion_lpdf(age[2] | mu2, kappa2); // beta distributions
  // density = density * (1 + psi*(age[1] - mu1) * (age[2] - mu2)); // account for correlation
  return(density);
  }
}
data {
  int<lower=1> N; // total number of observations
  matrix[N, 2] ages; // age of source and recipient
  real<lower=0> min_age;
  real<lower=0> max_age;
}

transformed data {
  matrix[N, 2] scaled_age = (ages - min_age)/(max_age - min_age); // scaled age of source and recipient (0,1)
}
parameters {
  real<lower=0, upper=1> mus[2];
  real<lower=0> kappas[2];
  real<lower=0> betas[2];
}
model {
    mus ~ uniform(0, 1);
    kappas ~ inv_gamma(2, betas);
    betas ~ exponential(1.0/3);
    for (k in 1:N){
      scaled_age[k] ~ beta_bivariate(mus[1], kappas[1], mus[2], kappas[2]);
    }
}
