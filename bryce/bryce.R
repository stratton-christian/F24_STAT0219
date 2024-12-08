# Bryce's question - TBD
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
    ma_mat[j,] <- c(
      runif(1, -1, 1), runif(1, -1, 1), runif(1, -.25, .25)
    )
    
    # simulate series for the error process
    mat_ts[,j] <- c(arima.sim(
      n = n, 
      model = list(
        ar = c(ar_mat[j,]),
        ma = c(ma_mat[j,]),
        sd = 1
      )
    ))
  }
  out <- tbl %>%
    mutate(y = rnorm(n(), c(X %*% beta) + c(mat_ts), .5))
  
  return(
    list(
      tbl = out,
      mat_ts = mat_ts,
      ar = ar_mat,
      ma = ma_mat,
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
## down side: less precision on residual standard error
par(mfrow = c(3,3))
for(i in 1:9){
  gls_fit <- gls(y ~ x, sim$tbl %>% filter(group == i), correlation = corARMA(p = 3, q = 3))
  print(gls_fit$modelStruct$corStruct)
  pacf(resid(gls_fit, "normalized"))
}

################
### OPTION 2 ### stan
################

# use Bayes :)
library(rstan)
options(mc.cores = parallel::detectCores() - 1)

# stan model
stan_X <- array(NA, dim = c(9, 100, 10))
stan_X[1,,] <- sim$X[1:100,]
stan_X[2,,] <- sim$X[101:200,]
stan_X[3,,] <- sim$X[201:300,]
stan_X[4,,] <- sim$X[301:400,]
stan_X[5,,] <- sim$X[401:500,]
stan_X[6,,] <- sim$X[501:600,]
stan_X[7,,] <- sim$X[601:700,]
stan_X[8,,] <- sim$X[701:800,]
stan_X[9,,] <- sim$X[801:900,]
stan_data <- list(
  N = sim$n,
  K = sim$k,
  Xncol = ncol(sim$X),
  X = stan_X,
  P = 3,
  Q = 3,
  y = matrix(sim$tbl$y, nrow = sim$n, ncol = sim$k)
) 

# stan model
fit <- stan(
  "arma.stan",
  data = stan_data,
  chains = 4,
  iter = 2000,
  pars = c("beta", "phi", "theta", "sigma")
)
summary(fit)$s



















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
tibble(
  param = rownames(summary),
  est = c(summary[,1]),
  true = c(
    sim$beta,
    c(sim$ar), 
    .5
  ),
  lwr = c(summary[,5]),
  upr = c(summary[,9])
) %>%
  mutate(capture = ifelse(lwr < true & upr > true, 1, 0) %>% factor()) %>%
  filter(grepl("theta", param)) %>%
  ggplot() + 
  geom_pointrange(
    aes(y = param, xmin = lwr, xmax = upr, x = est, col = capture)
  ) +
  geom_point(aes(y = param, x = true), col = "blue") +
  theme_bw()
 
matrix(summary[11:37,1], ncol = 3)
sim$ar

