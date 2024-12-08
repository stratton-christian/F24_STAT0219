data {
  int<lower=1> N; // num time points
  int<lower=1> K; // num groups
  int<lower=1> Xncol; // num regressors
  int<lower=0> P; // AR order
  int<lower=0> Q; // MA order
  matrix[N, K] y; // observed responses in N x K matrix
  array[K] matrix[N, Xncol] X;
}
parameters {
  vector[Xncol] beta; // reg coefs
  matrix[K, P] phi; // autoregression coeff
  matrix[K, Q] theta; // moving avg coeff
  real<lower=0> sigma; // standard deviation
}
transformed parameters {
  array[N, K] real eta;
  array[N, K] real mu;
  array[N, K] real epsilon;
  for (n in 1:N){
    for(j in 1:K){
      // Compute mean mu as linear regression of explanatory variables
      mu[n, j] = X[j][n] * beta;
      // Compute ARMA error terms eta
      eta[n, j] = y[n, j] - mu[n, j];
      epsilon[n, j] = eta[n, j];
      // AR-part
      for (p in 1:P) {
        if (n > p) {
          real ar_p = phi[j, p] * eta[n - p, j];
          mu[n, j] = mu[n, j] + ar_p;
          epsilon[n, j] = epsilon[n, j] - ar_p;
        }
      }
      // MA-part
      for (q in 1:Q) {
        if (n > q) {
          real ma_q = theta[j, q] * epsilon[n - q, j];
          mu[n, j] = mu[n, j] + ma_q;
          epsilon[n, j] = epsilon[n, j] - ma_q;
        }
      }
    }
  }
}
model {
  // likelihood
  for(j in 1:K){
    y[(max(P, Q) + 1):N, j] ~ normal(mu[(max(P, Q) + 1):N, j], sigma);
  }
  // priors
  for(k in 1:K){
    for(order in 1:P){
      phi[k, order] ~ normal(0, .5);
    }
    for(order in 1:Q){
      theta[k, order] ~ normal(0, .5);
    }
  }
  sigma ~ uniform(0, 10);
}

