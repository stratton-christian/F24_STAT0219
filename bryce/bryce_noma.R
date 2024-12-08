# Bryce's question
# check out stan in the future? https://mc-stan.org/docs/stan-users-guide/time-series.html#autoregressive-moving-average-models
## sim data first
rm(list = ls())
library(tidyverse)
sim_bryce <- function(seed, n, k){
  set.seed(seed)
  n <- n
  k <- k
  # storage for the series and ARMA parameters
  mat_ts <- matrix(NA, nrow = n, ncol = k)
  mat_reg <- matrix(NA, nrow = n, ncol = k )
  ar_mat <- matrix(NA, nrow = k, ncol = 3)
  ma_mat <- matrix(NA, nrow = k, ncol = 3)
  
  # create out tibble
  tbl <- tibble(
    group = rep(1:k, each = n) |> factor(),
    t = rep(1:n, k)
  ) %>%
    mutate(x = rnorm(n()))
  X <- model.matrix(~ x + group, tbl)
  beta <- rnorm(ncol(X))
  for(j in 1:ncol(mat_ts)){
    # get parameters
    ar_mat[j,] <- c(
      runif(1, .5, 1), runif(1, -.5, -.1), runif(1, -.25, .25)
    )
    
    # simulate series for the error process
    mat_ts[,j] <- c(arima.sim(
      n = n, 
      model = list(
        ar = c(ar_mat[j,]),
        sd = 1
      )
    ))
  }
  out <- tbl %>%
    mutate(y = c(X %*% beta) + c(mat_ts))
  
  return(
    list(
      tbl = out,
      mat_ts = mat_ts,
      ar = ar_mat,
      X = X,
      beta = beta,
      n= n,
      k = k
    )
  )
}

# sim and visualize data - different PACFs
sim <- sim_bryce(12082024, 100, 9)
sim$tbl %>%
  ggplot(aes(x = x, y = y, group = group)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~ group) +
  theme_bw()

par(mfrow = c(3,3))
fit <- lm(y ~ x, sim$tbl)
for(i in 1:9){
  pacf(resid(fit)[sim$tbl$group == i])
}


################
### OPTION 1 ###
################
library(nlme)

# fit a different gls model to each group
par(mfrow = c(3,3))
ests <- matrix(NA, 9, 7)
colnames(ests) <- c("ar1", "ar2", "ar3", "ma1", "ma2", "ma3", "sigma")
for(i in 1:9){
  gls_fit <- gls(y ~ x, sim$tbl %>% filter(group == i), correlation = corARMA(p = 3, q = 3))
  ests[i,] <- c(gls_fit$modelStruct$corStruct, gls_fit$sigma)
  pacf(resid(gls_fit, "normalized"))
}
ests

# pooling
pooled_fit <- gls(y ~ x + group, sim$tbl, correlation = corARMA(form = ~ 1 | group, p = 3, q = 3))
summary(pooled_fit)
pooled_fit$sigma

################
### OPTION 2 ### nimble
################

# use Bayes :)
library(nimble)

# ARMA(3,0) model
model_code <- nimbleCode({
  # likelihood 
  for(j in 1:k){
    y[1,j] ~ dnorm(mu[1,j], sd = sigma)
    y[2,j] ~ dnorm(mu[2,j], sd = sigma)
    y[3,j] ~ dnorm(mu[3,j], sd = sigma)
  }
  for(i in 4:n){
    for(j in 1:k){
      y[i,j] ~ dnorm(eta[i,j], sd = sigma)
      eta[i,j] <- mu[i,j] + phi[j,1] * (y[i-1,j] - mu[i-1,j]) + 
        phi[j,2] * (y[i-2,j] - mu[i-2,j]) + 
        phi[j,3] * (y[i-3,j] - mu[i-3,j]) 
    }
  }
  for(i in 1:n){
    for(j in 1:k){
      mu[i,j] <- (beta[1:p] %*% X[i, 1:p, j])[1,1]
    }
  }
  
  # calculate resids
  for(i in 1:3){
    for(j in 1:k){
      resid[i,j] <- y[i,j] - mu[i,j]
    }
  }
  for(i in 4:n){
    for(j in 1:k){
      resid[i,j] <- y[i,j] - eta[i,j]
    }
  }
  
  # priors
  for(ndx in 1:p){
    beta[ndx] ~ dnorm(0, sd = 10)
  }
  sigma ~ dunif(0, 10)
  sigma_w ~ dunif(0, 10)
  for(j in 1:k){
    for(order in 1:3){
      phi[j, order] ~ dunif(-1, 1)
    }
  }
})

# fit model in parallel
library(parallel)
this_cluster <- makeCluster(4)
run_MCMC_allcode <- function(seed, data, constants, code, monitors, niter = 10000) {
  library(nimble)
  
  myModel <- nimbleModel(code = code,
                         data = data,
                         constants = constants)
  
  CmyModel <- compileNimble(myModel)
  myMCMC <- buildMCMC(CmyModel, monitors = monitors)
  CmyMCMC <- compileNimble(myMCMC)
  
  results <- runMCMC(CmyMCMC, niter = 10000, setSeed = seed)
  
  return(results)
}
chain_output <- parLapply(
  cl = this_cluster, 
  X = 1:4, 
  fun = run_MCMC_allcode, 
  data = list(
    y = matrix(sim$tbl$y, nrow = sim$n, ncol = sim$k)
  ),
  constants = list(
    X = abind::abind(
      sim$X[1:100,],
      sim$X[101:200,],
      sim$X[201:300,],
      sim$X[301:400,],
      sim$X[401:500,],
      sim$X[501:600,],
      sim$X[601:700,],
      sim$X[701:800,],
      sim$X[801:900,],
      along = 3
    ),
    n = sim$n,
    p = ncol(sim$X),
    k = sim$k
  ),
  code = model_code,
  monitors = c("sigma", "phi", "beta", 'resid'),
  niter = 5000
)
stopCluster(this_cluster)

# summarize
library(coda)
library(rstan)
nimble_summary <- function(fit, warmup = nrow(fit[[1]])/2, thin = 1){
  
  # convert to coda for normal summary
  fit_warmup <- lapply(fit, function(x) x[(warmup+1):nrow(x),])
  coda_samples <- as.mcmc.list(lapply(fit_warmup, function(x) as.mcmc(
    x, start = warmup+1, end = nrow(fit), thin = thin
  )))
  
  sum <- summary(coda_samples)
  params <- dimnames(sum$statistics)[[1]]
  tmp_sum <- cbind(sum$statistics, sum$quantiles)
  
  # get r hat / n_eff
  mat <- matrix(NA, nrow = nrow(tmp_sum), ncol = 3)
  colnames(mat) <- c("Rhat", "ess_bulk", "ess_tail")
  for(i in 1:nrow(tmp_sum)){
    tmp <- sapply(fit, function(x) x[,i])
    mat[i,] <- c(rstan::Rhat(tmp), rstan::ess_bulk(tmp), rstan::ess_tail(tmp))
  }
  
  # out
  out <- cbind(tmp_sum, mat)
  return(out)
}
summary <- nimble_summary(chain_output)

# phi params
tbl <- tibble(
  param = rownames(summary),
  est = c(summary[,1]),
  true = c(
    sim$beta,
    c(sim$ar), 
    rep(NA, sim$n * sim$k),
    1
  ),
  lwr = c(summary[,5]),
  upr = c(summary[,9])
) %>%
  mutate(capture = ifelse(lwr < true & upr > true, 1, 0) %>% factor()) 

tbl %>%
  filter(grepl("phi", param)) %>%
  ggplot() + 
  geom_pointrange(
    aes(y = param, xmin = lwr, xmax = upr, x = est, col = capture)
  ) +
  geom_point(aes(y = param, x = true), col = "blue") +
  theme_bw()

resids <- tbl %>%
  filter(grepl("resid", param)) %>%
  mutate(t = rep(1:sim$n, sim$k)) %>%
  mutate(group = rep(1:sim$k, each = sim$n))

par(mfrow = c(3,3))
for(i in 1:9){
  df <- resids %>% filter(group == i)
  pacf(df$est)
}
