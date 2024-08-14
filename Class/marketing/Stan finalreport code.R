library(cmdstanr)
library(bayesplot)
library(gridExtra)

df1 <- read.csv("sales_visit.csv")
df2 <- read.csv("demog.csv")
model <- cmdstan_model("model_multilevel.stan")
stan_data <- list(N=23000, G=1000, SALES=df1$sales_num,VISIT=df1$visit_num,LAGSALES=df1$lagged_sales_num,ID=df1$id,EXP10=df2$exp10,AVG=df2$avg_samp)
result <- model$sample(data=stan_data)

#beta平均値の結果表示
beta_0_samples <- result$draws("beta_0")
beta_VISIT_samples <- result$draws("beta_VISIT")
beta_LAGSALES_samples <- result$draws("beta_LAGSALES")
beta_0_means <- apply(beta_0_samples, 1, function(x) quantile(x,0.975))
beta_VISIT_means <- apply(beta_VISIT_samples, 1, function(x) quantile(x,0.975))
beta_LAGSALES_means <- apply(beta_LAGSALES_samples,1, function(x) quantile(x,0.975))
par(mfrow = c(1, 3))
hist(beta_0_means, xlab = "切片パラメータの上側2.5%点")
hist(beta_VISIT_means, xlab = "VISIT係数の上側2.5%点")
hist(beta_LAGSALES_means, xlab = "LAGSALES係数の上側2.5%点")

#theta結果表示
result$summary("theta_mu_0")
result$summary("theta_AVG_0")
result$summary("theta_EXP10_0")
result$summary("theta_mu_VISIT")
result$summary("theta_AVG_VISIT")
result$summary("theta_EXP10_VISIT")
result$summary("theta_mu_LAGSALES")
result$summary("theta_AVG_LAGSALES")
result$summary("theta_EXP10_LAGSALES")

#sigma結果表示
result$summary("sigma")
result$summary("sigma_beta_0")
result$summary("sigma_beta_VISIT")
result$summary("sigma_beta_LAGSALES")

#トレースプロット
# 個別のプロットを作成→結合
q1 <- mcmc_trace(result$draws("theta_mu_0")) 
q2 <- mcmc_trace(result$draws("theta_AVG_0")) 
q3 <- mcmc_trace(result$draws("theta_EXP10_0")) 
q4 <- mcmc_trace(result$draws("theta_mu_VISIT"))
q5 <- mcmc_trace(result$draws("theta_AVG_VISIT")) 
q6 <- mcmc_trace(result$draws("theta_EXP10_VISIT")) 
q7 <- mcmc_trace(result$draws("theta_mu_LAGSALES")) 
q8 <- mcmc_trace(result$draws("theta_AVG_LAGSALES")) 
q9 <- mcmc_trace(result$draws("theta_EXP10_LAGSALES")) 
grid.arrange(q1, q2, q3, q4, q5, q6, q7, q8, q9, nrow = 3)

s1 <- mcmc_trace(result$draws("sigma"))
s2 <- mcmc_trace(result$draws("sigma_beta_0"))
s3 <- mcmc_trace(result$draws("sigma_beta_VISIT"))
s4 <- mcmc_trace(result$draws("sigma_beta_LAGSALES"))
grid.arrange(s1, s2, s3, s4, nrow = 4)


#smalldataの結果
df3 <- read.csv("sales_visit_small.csv")
df4 <- read.csv("demog_small.csv")
model_small <- cmdstan_model("model_multilevel.stan")
stan_data_small <- list(N=23, G=1, SALES=df3$sales_num,VISIT=df3$visit_num,LAGSALES=df3$lagged_sales_num,ID=df3$id,EXP10=df4$exp10,AVG=df4$avg_samp)
result_small <- model_small$sample(data=stan_data_small)
t1 <- mcmc_trace(result_small$draws("beta_0"))
t2 <- mcmc_trace(result_small$draws("beta_VISIT"))
t3 <- mcmc_trace(result_small$draws("beta_LAGSALES"))
grid.arrange(t1, t2, t3, nrow = 3)


