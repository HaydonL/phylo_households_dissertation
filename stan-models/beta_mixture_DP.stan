functions {
  // Define bivariate beta density
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
  int<lower=0> K;  // max number of clusters
  int<lower=1> N_group; // total number of groups
  matrix[N, 2] ages; // age of source and recipient
  array[N] int<lower=1, upper=N_group> group_nos; // group number for infection (e.g type M -> F)
  real<lower=0> min_age;
  real<lower=0> max_age;
}
transformed data {
  matrix[N, 2] scaled_age = (ages - min_age)/(max_age - min_age); // scaled age of source and recipient (0,1)
  array[N_group] int group_counts = rep_array(0, N_group); // number of observations per group
  for (k in 1:N){
    group_counts[group_nos[k]] = group_counts[group_nos[k]] + 1;
  }
}
parameters {
  array[N_group] vector<lower=0,upper=1>[K - 1] v;  // stickbreak components
  array[N_group] real<lower=0> alpha;  // hyper prior DP(alpha, base)
  array[N_group] real<lower=0> eta; // rate for PP 
  array[N_group, K, 2] real<lower=0, upper=1> mus;
  array[N_group, K, 2] real<lower=0> kappas;
  array[N_group, K] real psis;
}

transformed parameters {
  array[N_group] simplex[K] weights;
  array[N_group] vector<lower=0,upper=1>[K - 1] cumprod_one_minus_v;
  
  for (group in 1:N_group){
    cumprod_one_minus_v[group] = exp(cumulative_sum(log1m(v[group])));
    weights[group, 1] = v[group, 1];
    weights[group, 2:(K-1)] = v[group, 2:(K-1)] .* cumprod_one_minus_v[group, 1:(K-2)];
    weights[group, K] = cumprod_one_minus_v[group, K - 1];
  }
  
  array[N_group, K] vector[2] psi_bounds;
  for (group in 1:N_group){
    for (k in 1:K){
        psi_bounds[group, k] = calc_psi_bounds(mus[group, k, 1], 
                                                 mus[group, k, 2]);
    }
  }
}

model {
  array[N_group, K] real ps;
  
  group_counts ~ poisson(eta);
  alpha ~ gamma(5, 1);  // mean = a/b = shape/rate 
  
  for (group in 1:N_group){
    
    v[group] ~ beta(1, alpha[group]);
    
    for(i in 1:N){
      if (group == group_nos[i]){
        for(k in 1:K){
          ps[group, k] = log(weights[group,k]) + 
          beta_bivariate_lpdf(scaled_age[i] | mus[group,k,1], kappas[group,k,1], 
                                              mus[group,k,2], kappas[group,k,2], 
                                              psis[group,k]);
      }
      target += log_sum_exp(ps[group]);
    }
  }
  
  for (k in 1:K){
    mus[group, k] ~ uniform(0, 1);
    kappas[group, k] ~ inv_gamma(2, 2);
    psis[group, k] ~ uniform(psi_bounds[group, k, 1], psi_bounds[group, k, 2]);
  }
}
}
// Generated quantities removed for now, check beta_copy version for details