data {
  int<lower=1> N; // total number of observations
  int<lower=1> N_group; // total number of groups
  int<lower=1, upper=N_group> group_nos[N]; // group number for infection (e.g type M -> F)
}

transformed data {
  int group_counts[N_group] = rep_array(0, N_group); // number of observations per group
  for (k in 1:N){
    group_counts[group_nos[k]] = group_counts[group_nos[k]] + 1;
  }
}
parameters {
  real<lower=0> eta[N_group]; // rates for PP for each group
}
model {
  eta ~ gamma(50, 1);
  group_counts ~ poisson(eta); // Poisson count likelihood
}





