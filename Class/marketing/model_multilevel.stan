data {
  int N;
  int G;
  array[N] real SALES;
  vector[N] VISIT;
  vector[N] LAGSALES;
  array[N] int ID;
  vector[G] AVG;
  vector[G] EXP10;

}

parameters {
  vector[G] beta_0;
  vector[G] beta_VISIT;
  vector[G] beta_LAGSALES;
  real <lower=0> sigma;
  real theta_mu_0;
  real theta_AVG_0;
  real theta_EXP10_0;
  real theta_mu_VISIT;
  real theta_AVG_VISIT;
  real theta_EXP10_VISIT;
  real theta_mu_LAGSALES;
  real theta_AVG_LAGSALES;
  real theta_EXP10_LAGSALES;

  real <lower=0> sigma_beta_0;
  real <lower=0> sigma_beta_VISIT;
  real <lower=0> sigma_beta_LAGSALES;
}

transformed parameters{
  vector[N] yhat;
  for (i in 1:N){
    yhat[i] = beta_0[ID[i]] + beta_VISIT[ID[i]]*VISIT[i] + beta_LAGSALES[ID[i]]*LAGSALES[i] ;
  }
}

model {
  beta_0 ~ normal(theta_mu_0 + theta_AVG_0*AVG + theta_EXP10_0*EXP10,sigma_beta_0);
  beta_VISIT ~ normal(theta_mu_VISIT + theta_AVG_VISIT*AVG + theta_EXP10_VISIT*EXP10,sigma_beta_VISIT);
  beta_LAGSALES ~ normal(theta_mu_LAGSALES + theta_AVG_LAGSALES*AVG  +theta_EXP10_LAGSALES*EXP10,sigma_beta_LAGSALES);
  sigma ~ cauchy(0,10);
  
  theta_mu_0 ~ normal(0,10);
  theta_mu_VISIT ~ normal(0,10);
  theta_mu_LAGSALES ~ normal(0,10);
  theta_AVG_0 ~ normal(0,10);
  theta_AVG_VISIT ~ normal(0,10);
  theta_AVG_LAGSALES ~ normal(0,10);
  theta_EXP10_0 ~ normal(0,10);
  theta_EXP10_VISIT ~ normal(0,10);
  theta_EXP10_LAGSALES ~ normal(0,10);

  sigma_beta_0 ~cauchy(0,10);
  sigma_beta_VISIT ~cauchy(0,10);
  sigma_beta_LAGSALES ~cauchy(0,10);
  
  SALES ~ normal(yhat, sigma);
}

generated quantities{
  array[N] real SALES_pred;
  SALES_pred = normal_rng(yhat, sigma);
}

