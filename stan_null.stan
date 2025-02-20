data {
  int<lower=0> N;  // number of observations
  int<lower=0> K;  // number of predictors
  matrix[N, K] X;  // predictor variables
  vector<lower=0, upper=1>[N] y;     // response variable
}

parameters {
  real alpha;
  real delta;
  //vector[K] beta;
  //vector[K] epsilon;// regression coefficients
  
}

transformed parameters {
  vector[N] mu;  // mean parameter
  vector[N] phi;  // precision parameters
  vector<lower=0>[N] A;             // parameter for beta distn
  vector<lower=0>[N] B;             // parameter for beta distn
  //real<lower=0, upper=1> phi;  // mean parameter
  //mu = inv_logit(alpha +X * beta);  // Linear predictor transformed to (0, 1) range
  
  for (i in 1:N) {
    mu[i]  = inv_logit(alpha);   
    phi[i] = exp(delta);
  }

  A = mu .* phi;
  B = (1.0 - mu) .* phi;
}
  
  //mu = inv_logit(alpha+ X * beta);
  //phi = exp(delta+ X * epsilon);


model {
  // Priors
  alpha ~ normal(0, 100);
  //epsilon ~ normal(0, 10);
  delta~ normal(0, 100);
  //beta ~ normal(0, 10);  // Prior for regression coefficients
 // epsilon ~ normal(0, 10); 
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
    real EAIC= -2*likel+2*(1+1+2);
    real EBIC= -2*likel+log(N)*(1+1+2);
}


