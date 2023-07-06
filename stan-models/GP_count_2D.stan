functions {
    matrix kron_mvprod(matrix A, matrix B, matrix V) 
    {
        return transpose(A*transpose(B*V));
    }
    
  matrix gp(int N_rows, int N_columns, real[] rows_idx, real[] columns_index,
            real delta0,
            real alpha_gp, 
            real rho_gp1, real rho_gp2,
            matrix z1)
  {
    
    matrix[N_rows,N_columns] GP;
    
    matrix[N_rows, N_rows] K1;
    matrix[N_rows, N_rows] L_K1;
    
    matrix[N_columns, N_columns] K2;
    matrix[N_columns, N_columns] L_K2;
    
    K1 = cov_exp_quad(rows_idx, alpha_gp, rho_gp1) + diag_matrix(rep_vector(delta0, N_rows));
    K2 = cov_exp_quad(columns_index, alpha_gp, rho_gp2) + diag_matrix(rep_vector(delta0, N_columns));

    L_K1 = cholesky_decompose(K1);
    L_K2 = cholesky_decompose(K2);
    
    GP = kron_mvprod(L_K2, L_K1, z1);

    return(GP);
  }
}

data {
  int<lower=1> N; \\ total number of observations
  int<lower=1> N_group \\ total number of groups
  int age_source[N]; \\ age of source
  int age_recip[N]; \\ age of recipient
  int<lower=1, upper=N_group> group_no[N]; \\ group number for infection (e.g type M -> F)
  real min_age;
  real max_age;
}

transformed data {
  real delta = 1e-9; \\ Offset for numerical stability
  real scaled_age_source[N] = (age_source - min_age)/(max_age - min_age); \\ Scaled age of source (0,1)
  real scaled_age_recip[N] = (age_recip - min_age)/(max_age - min_age); \\ Scaled age of recipient (0,1)
  int group_counts[N_group] = 0; \\ number of observations per group
  for (k in 1:N){
    group_counts[group_no[k]] = group_counts[group_no[k]] + 1 
  }
}
parameters {
  real<lower=0> rho_1;
  real<lower=0> rho_2;
  real<lower=0> alpha_gp;
  real<lower=0> eta[N_group]; \\ rates for PP for each group
  matrix[N,N] zeta;
}
transformed parameters {
  matrix[N,N] f = exp(gp(N, N, scaled_age_source, scaled_age_recip,
                              delta,
                              alpha_gp, 
                              rho_1,  rho_2,
                              zeta));
  matrix[N,N] pi = softmax(f);
}

model {
  rho_1 ~ inv_gamma(2,2);
  rho_2 ~ inv_gamma(2,2);
  alpha_gp ~ cauchy(0,1);
  eta ~ gamma(1, 1)
  
  for(i in 1:n){
    for(j in 1:m){
        zeta[i,j] ~ std_normal(); \\ why not vectorised? what is this for?
    }
  }
  
  for(group_no in 1:N_group){
    group_counts[group_no] ~ poisson(eta[group_no]) \\ Poisson count likelihood
  }
  
  for(k in 1:N){
      y[k] ~ neg_binomial(alpha[coordinates[k,1],coordinates[k,2]], nu_inverse);
  }

  
}

generated quantities {
 real log_lik[N];
  
  for(k in 1:N)
    log_lik[k] = neg_binomial_lpmf(y[k] | alpha[coordinates[k,1],coordinates[k,2]], nu_inverse);

}




