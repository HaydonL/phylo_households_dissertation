functions {
  real beta_bivariate_lpdf(row_vector age, real mu1, real kappa1, real mu2, real kappa2, real psi)
  {
  real density;
  density = beta_proportion_lpdf(age[1] | mu1, kappa1) + beta_proportion_lpdf(age[2] | mu2, kappa2); // beta distributions
  density = density + log(1 + psi*(age[1] - mu1) * (age[2] - mu2)); // account for correlation
  return(density);
  }
  
  vector calc_psi_bounds(real mu1, real mu2)
  {
    vector[2] bounds;
    array[2] real min_array;
    array[2] real max_array;
    
    min_array[1] = mu1 * mu2;
    min_array[2] = (1 - mu1)*(1 - mu2);
    max_array[1] = mu1 * (mu2 - 1);
    max_array[2] = mu2 * (mu1 - 1);
     
    bounds[1] = -(1/max(min_array));
    bounds[2] = -(1/min(max_array));
    
    return(bounds);
  }
}

data {
  int<lower=1> N; // total number of observations
  int<lower=1> N_group; // total number of groups
  matrix[N, 2] ages; // age of source and recipient
  int<lower=1, upper=N_group> group_nos[N]; // group number for infection (e.g type M -> F)
  real<lower=0> min_age;
  real<lower=0> max_age;
}

transformed data {
  matrix[N, 2] scaled_age = (ages - min_age)/(max_age - min_age); // scaled age of source and recipient (0,1)
  int group_counts[N_group] = rep_array(0, N_group); // number of observations per group
  for (k in 1:N){
    group_counts[group_nos[k]] = group_counts[group_nos[k]] + 1;
  }
}
parameters {
  real<lower=0> eta[N_group]; // rates for PP for each group
  real<lower=0, upper=1> mus[N_group, 2];
  real<lower=0> kappas[N_group, 2];
  real psis[N_group];
}
transformed parameters {
    // real<lower=0, upper=1> weights2 = 1 - weights1; // weighting for second beta distribution
    array[N_group] vector[2] psi_bounds;
    for (group_no in 1:N_group){
        psi_bounds[group_no] = calc_psi_bounds(mus[N_group, 1], mus[N_group, 2]);
    }
}
model {
  eta ~ gamma(50, 1);
  // weights1 ~ uniform(0, 1);
  
  for (group_no in 1:N_group){
    mus[group_no] ~ uniform(0, 1);
    kappas[group_no] ~ inv_gamma(2, 2);
    psis[group_no] ~ uniform(psi_bounds[N_group, 1], psi_bounds[N_group, 2]);
    // group_counts[group_no] ~ poisson(eta[group_no]); // Poisson count likelihood
  }
  group_counts ~ poisson(eta); // Poisson count likelihood
  
  for(k in 1:N){
      target += beta_bivariate_lpdf(scaled_age[k] | mus[group_nos[k], 1], kappas[group_nos[k], 1], mus[group_nos[k], 2], kappas[group_nos[k], 2], psis[N_group]);
  }
}
