data {
  int<lower=1> N; // total number of observations
  int<lower=0> K;  // max number of clusters
  matrix[N, 2] ages; // age of source and recipient
}

parameters {
  array[K, 2] real mus;
  array[K] cov_matrix[2] Sigmas;
  simplex[K] weights;
}
transformed parameters{
  array[K, 2] real ordered_mus;
  ordered_mus = mus[sort_indices_asc(to_vector(mus[, 1])), ];
}
model {
  array[K] real ps;
  
  for(i in 1:N){
    for(k in 1:K){
      ps[k] = log(weights[k]) + multi_normal_lpdf(ages[i] | to_vector(ordered_mus[k]), Sigmas[k]);
    }
    target += log_sum_exp(ps);
  }
  
  for (k in 1:K){
    mus[k] ~ normal(0, 3);
    Sigmas[k] ~ inv_wishart(3, diag_matrix([1, 1]')); // Set priors for MVN params
  }
}

