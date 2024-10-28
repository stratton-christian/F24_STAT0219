generate_ts_reg <- function(
    seed = NULL, n = 144, freq = 12, 
    alpha = sample(c(runif(1, -.999, -.5), runif(1, .5, .999)), size = 1),
    sigma = 1,
    betas = c(rnorm(1), runif(1, -.1, .1), rnorm(2*floor(freq/2), sd = .5))
){
  # function to simulate ts regression model of the form:
  # y[t] = beta0 + beta1*time[t] + beta2*sin(2*pi*t/freq) + beta3*cos(2*pi*t/freq) + e
  # where e = AR(1) with alpha = alpha
  
  # some error checking - not comprehensive
  if(length(betas) != (2 + 2*floor(freq/2))){
    stop("betas should have length 2 + 2*floor(freq/2) - and you should know why!")
  }
  if(n %% freq != 0) {
    stop("n %% freq != 0. Your life will be easier if you simulate complete seasonal cycles.")
  }
  if(freq < 7) stop("freq must be >= 7")
  
  if(!is.null(seed)) set.seed(seed)
  
  # create a table with trend and seasonal effects
  out <- tibble(
    t = 1:n
  )
  cycles <- floor(freq/2)
  sin_mat <- matrix(NA, nrow = n, ncol = cycles)
  cos_mat <- matrix(NA, nrow = n, ncol = cycles)
  for(i in 1:cycles){
    sin_mat[,i] <- sin(2*pi*i*out$t/freq)
    cos_mat[,i] <- cos(2*pi*i*out$t/freq)
  }
  tmp <- suppressMessages({dplyr::bind_cols(out, sin_mat, cos_mat)})
  names(tmp)[2:ncol(tmp)] <- c(
    paste0("sin", rep(1:cycles), "t"), 
    paste0("cos", rep(1:cycles), "t")
  )
  
  # create design matrix for reg
  X <- model.matrix(~ ., tmp)
  mean <- c(X %*% betas)
  
  # construct the error series
  error <- w <- rnorm(n, sd = sigma)
  for(t in 2:n) error[t] <- alpha*error[t-1] + w[t]
  
  tmp$y <- mean + error
  
  return(
    list(
      df = tmp,
      X = X,
      mean = mean,
      params = list(
        betas = betas,
        sigma = sigma, 
        alpha = alpha
      )
    )
  )
}