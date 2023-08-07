functions {
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
}data {
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
  matrix[N, 2] logit_age = logit(scaled_age);
  array[N_group] int group_counts = rep_array(0, N_group); // number of observations per group
  for (k in 1:N){
    group_counts[group_nos[k]] = group_counts[group_nos[k]] + 1;
  }
}
parameters {
  array[N_group] vector<lower=0,upper=1>[K - 1] v;  // stickbreak components
  array[N_group] real<lower=0> alpha;  // hyper prior DP(alpha, base)
  array[N_group] real<lower=0> eta; // rate for PP 
  array[N_group] ordered[K] mus_1; // mean_1 for 2D normal distribution
  array[N_group, K] real mus_2; // mean_2 for 2D normal distribution
  array[N_group, K] cov_matrix[2] Sigmas; // covariance matrices
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
  
}

model {
  array[N_group, K] real ps;
  
  group_counts ~ poisson(eta);
  for (group in 1:N_group){
    alpha[group] ~ SGb1(K, 3);
  }
  
  for (group in 1:N_group){
    v[group] ~ beta(1, alpha[group]);

    for(i in 1:N){ 
      if (group == group_nos[i]){
        for(k in 1:K){
          ps[group, k] = log(weights[group, k]) + 
          multi_normal_lpdf(logit_age[i] | to_vector([mus_1[group, k], mus_2[group, k]]), Sigmas[group, k]);
        }
        target += log_sum_exp(ps[group]);
      }
    }
  
  // Set priors for MVN params
  for(k in 1:K){
    mus_1[group, k] ~ normal(0, 10);
    mus_2[group, k] ~ normal(0, 10);
    Sigmas[group, k] ~ inv_wishart(3, diag_matrix([1, 1]')); 
  }

  }

}
// Generated quantities removed for now, check logit_copy version for details