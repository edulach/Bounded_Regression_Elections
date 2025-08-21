data {
  int<lower=0> N;  // number of observations
  int<lower=0> r;   // number of predictors for mean
  int<lower=0> s;   // number of predictors for precision
  matrix[N, r] X;  // predictor variables
  matrix[N, s] Y;  // predictor variables
  vector<lower=0, upper=1>[N] y;     // response variable
}

parameters {
  real beta_0;
  real epsilon_0;
  vector[r] beta;
  vector[s] epsilon;// regression coefficients
  
}

transformed parameters {
  vector[N] mu;  // mean parameter
  vector[N] phi;  // precision parameter
  vector<lower=0>[N] A;             // parameter for beta distn
  vector<lower=0>[N] B;             // parameter for beta distn
  //real<lower=0, upper=1> phi;  // mean parameter
  //mu = inv_logit(alpha +X * beta);  // Linear predictor transformed to (0, 1) range
  
  for (i in 1:N) {
    mu[i]  = inv_logit(beta_0 + X[i,] * beta);   
    phi[i] = exp(epsilon_0 + Y[i,] * epsilon);
  }

  A = mu .* phi;
  B = (1.0 - mu) .* phi;
}
  
  //mu = inv_logit(alpha+ X * beta);
  //phi = exp(delta+ X * epsilon);


model {
  // Priors
  beta_0 ~ normal(0, 10);
  epsilon_0 ~ normal(0, 10);
  beta ~ normal(0, 10);  // Prior for regression coefficients
  epsilon~ normal(0, 10);
  //phi ~ inv_gamma(2, 2);  // Prior for precision
  
  // Likelihood
  for (n in 1:N) {
    y ~ beta(A, B);
  }
}


generated quantities {
  vector[N] log_lik;           // Log-likelihood for WAIC computation
  
  for (i in 1:N) {
    log_lik[i] = beta_lpdf(y[i] | A[i],B[i]);
  }
  real likel=sum(log_lik);
    real EAIC= -2*likel+2*((r+s)+ 2);
    real EBIC= -2*likel+log(N)*((r+s)+ 2);
}



