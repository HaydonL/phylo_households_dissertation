data {
int<lower=1> I; // # of items
int<lower=1> J; // # of respondents
int<lower=1> C; // # of classes
int y[J,I]; // response  matrix
}

parameters {
  simplex[C] alpha; // probabilities of being in one group

   real <lower = 0, upper = 1> p[C, I];
}
transformed parameters{
   vector[I] p_prod; 

   /* product of endorsing probs across classes 
    to check convergence up to permutation of class labels */
   for(i in 1:I){
     p_prod[i] = prod(p[, i]); 
   }

}

model {
real lmix[C];
for (j in 1:J){
  for (c in 1: C){
               lmix[c] = log(alpha[c]) + bernoulli_lpmf(y[j, ] | p[c,]);
               }
  target += log_sum_exp(lmix);
  }
}

generated quantities {
  int<lower = 1> pred_class_dis[J];     // posterior predictive prediction for respondent j in latent class c
  simplex[C] pred_class[J];     // posterior probabilities of respondent j in latent class c
  real lmix[C];


for (j in 1:J){
  for (c in 1: C){
               lmix[c] = log(alpha[c]) + bernoulli_lpmf(y[j, ] | p[c,]);
    }               
  for (c in 1: C){
               pred_class[j][c] = exp((lmix[c])-log_sum_exp(lmix));
    }
    pred_class_dis[j] = categorical_rng(pred_class[j]);
  }
}