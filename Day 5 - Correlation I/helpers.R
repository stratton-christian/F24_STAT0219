# helper functions
sim_lm <- function(maxT = 100, beta = c(0, .1, .0025), sigma2 = 1, ndat = 1, period = 12, si, ci, rho = 1, seed = NULL){
  # function to simulate data from linear model
  
  # optionally set seed
  if(!is.null(seed)) set.seed(seed)
  
  # storage
  out <- matrix(NA, maxT, ndat)
  
  # create harmonic seasonal components
  time <- 1:maxT
  harmonic_seasonal <- rowSums(sapply(1:4, function(x) si[x] * sin(2*pi*x*time/period))) +
    rowSums(sapply(1:4, function(x) ci[x] * cos(2*pi*x*time/period)))
  
  for(dat in 1:ndat) {
    # generate noise and initialize response vector
    z <- w <- rnorm(maxT, 0, sqrt(sigma2))
    
    # get autocorrelated errors
    for (t in 2:maxT) z[t] <- rho * z[t - 1] + w[t]
    
    # get trend
    if(length(beta) == 1){
      mt <- beta[1] + harmonic_seasonal
      y <- mt + z
    } else{
      X <- matrix(1, nrow = maxT, ncol = length(beta))
      for(col in 2:ncol(X)){
        tmp <- (1:maxT)^(col-1)
        X[,col] <- tmp
      }
      mt <- X %*% beta + harmonic_seasonal
      y <- mt + z
    }
    
    
    out[,dat] <- y
  }
  
  return(out)
}
multi_rwd_wrapper <- function(mat){
  out <- tibble(
    y = c(mat),
    t = rep(1:nrow(mat), ncol(mat)),
    ndx = rep(1:ncol(mat), each = nrow(mat)) %>% factor
  )
  
  return(out)
}