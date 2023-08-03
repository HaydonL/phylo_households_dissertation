data {
  int<lower=1> N; // total number of observations
  int<lower=0> K;  // max number of clusters
  matrix[N, 2] ages; // age of source and recipient
  real<lower=0> min_age;
  real<lower=0> max_age;
}
transformed data {
  matrix[N, 2] scaled_age = (ages - min_age)/(max_age - min_age); // scaled age of source and recipient (0,1)
  matrix[N, 2] logit_age = logit(scaled_age);
}
parameters {
  vector<lower=0,upper=1>[K - 1] v;  // stickbreak components
  real<lower=0> alpha;  // hyper prior DP(alpha, base)
  real<lower=0> eta; // rate for PP 
  array[K] vector[2] mus;
  array[K] cov_matrix[2] Sigmas;
  array[2] real<lower=0> sigma_mus;
}

transformed parameters {
  simplex[K] weights;
  vector<lower=0,upper=1>[K - 1] cumprod_one_minus_v;
  cumprod_one_minus_v = exp(cumulative_sum(log1m(v)));
  weights[1] = v[1];
  weights[2:(K-1)] = v[2:(K-1)] .* cumprod_one_minus_v[1:(K-2)];
  weights[K] = cumprod_one_minus_v[K - 1];
  
  array[K] vector[2] sorted_mus;
  #K_order = sort_asc()
}

model {
  array[K] real ps;
  
  alpha ~ gamma(5, 1);  // mean = a/b = shape/rate 
  v ~ beta(1, alpha);

  for(i in 1:N){
    for(k in 1:K){
      ps[k] = log(weights[k]) + multi_normal_lpdf(logit_age[i] | mus[k], Sigmas[k]);
    }
    target += log_sum_exp(ps);
  }
  
  for (k in 1:K){
    mus[k] ~ normal(0, sigma_mus);
    Sigmas[k] ~ inv_wishart(3, diag_matrix([1, 1]')); // Set priors for MVN params
  }
  
  sigma_mus ~ cauchy(0, 1);
  N ~ poisson(eta);
}
generated quantities {
  array[N] int<lower = 1> pred_class_dis;     // posterior predictive prediction for respondent j in latent class c
  simplex[K] pred_class[N];     // posterior probabilities of respondent j in latent class c
  array[K] real lmix;


for (i in 1:N){
  for (k in 1:K){
        lmix[k] = log(weights[k]) + multi_normal_lpdf(logit_age[i] | mus[k], Sigmas[k]);
  }
  for (k in 1:K){
        pred_class[i][k] = exp((lmix[k])-log_sum_exp(lmix));
  }
    pred_class_dis[i] = categorical_rng(pred_class[i]);
  }
}