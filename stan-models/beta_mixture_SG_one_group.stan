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
  real SGb1_lpdf(real alpha, int a, int m){
    real density;
    array[m] real a_m;
    array[m - 1] real s;
    
    for (i in 1:m){
      a_m[i] = alpha + i - 1;
    }
    
    for (j in 1:(m - 1)){
      s[j] = (-1)^(a + j) * j^(a - 2) * log(j) / (tgamma(j) * tgamma(m - j));
    }
    
    density = (a - 1) * log(alpha) - sum(log(a_m)) - log(sum(s));
    return(density);
  }
}
data {
  int<lower=1> N; // total number of observations
  int<lower=0> K;  // max number of clusters
  matrix[N, 2] ages; // age of source and recipient
  // int<lower=1, upper=N_group> group_nos[N]; // group number for infection (e.g type M -> F)
  real<lower=0> min_age;
  real<lower=0> max_age;
}
transformed data {
  matrix[N, 2] scaled_age = (ages - min_age)/(max_age - min_age); // scaled age of source and recipient (0,1)
  //int group_counts[N_group] = rep_array(0, N_group); // number of observations per group
  //for (k in 1:N){
  //  group_counts[group_nos[k]] = group_counts[group_nos[k]] + 1;
  //}
}
parameters {
  vector<lower=0,upper=1>[K - 1] v;  // stickbreak components
  real<lower=0> alpha;  // hyper prior DP(alpha, base)
  real<lower=0> eta; // rate for PP 
  array[K, 2] real<lower=0, upper=1> mus;
  array[K, 2] real<lower=0> kappas;
  array[K] real psis;
}

transformed parameters {
  simplex[K] weights;
  vector<lower=0,upper=1>[K - 1] cumprod_one_minus_v;
  cumprod_one_minus_v = exp(cumulative_sum(log1m(v)));
  weights[1] = v[1];
  weights[2:(K-1)] = v[2:(K-1)] .* cumprod_one_minus_v[1:(K-2)];
  weights[K] = cumprod_one_minus_v[K - 1];
  array[K] vector[2] psi_bounds;
    for (k in 1:K){
        psi_bounds[k] = calc_psi_bounds(mus[k, 1], mus[k, 2]);
    }
}

model {
  array[K] real ps;
  
  alpha ~ SGb1(2, 3);  
  v ~ beta(1, alpha);

  for(i in 1:N){
    for(k in 1:K){
      ps[k] = log(weights[k]) + beta_bivariate_lpdf(scaled_age[i] | mus[k,1], kappas[k,1], mus[k,2], kappas[k,2], psis[k]);
    }
    target += log_sum_exp(ps);
  }
  
  for (k in 1:K){
    mus[k] ~ uniform(0, 1);
    kappas[k] ~ inv_gamma(2, 2);
    psis[k] ~ uniform(psi_bounds[k, 1], psi_bounds[k, 2]);
  }
    
  N ~ poisson(eta);
}
generated quantities {
  array[N] int<lower = 1> pred_class_dis;     // posterior predictive prediction for respondent j in latent class c
  simplex[K] pred_class[N];     // posterior probabilities of respondent j in latent class c
  array[K] real lmix;


for (i in 1:N){
  for (k in 1:K){
        lmix[k] = log(weights[k]) + beta_bivariate_lpdf(scaled_age[i] | mus[k,1], kappas[k,1], mus[k,2], kappas[k,2], psis[k]);
  }
  for (k in 1:K){
        pred_class[i][k] = exp((lmix[k])-log_sum_exp(lmix));
  }
    pred_class_dis[i] = categorical_rng(pred_class[i]);
  }
}