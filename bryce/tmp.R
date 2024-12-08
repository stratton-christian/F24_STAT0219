# likelihood 
for(t in 1:n){
  for(j in 1:k){
    y[t,k] ~ dnorm(mu[t,k], sd = sigma)
  }
}

# define mean for t = 1, t = 2 (involves latent variables)
for(j in 1:k){
  mu[1,j] <- phi1[j] * y_0[j] + phi2[j] * y_n1[j] + w[1,j] + theta1[j] * w_0[j] + theta2[j]*w_n1[j]
  mu[2,j] <- phi1[j] * y[1,j] + phi2[j] * y_0[j] + w[2,j] + theta1[j] * w[1,j] + theta2[j]*w_0[j]
} 

# define mean for t = 3:n
for(t in 3:n){
  for(j in 1:k){
    mu[t,j] <- phi1[j] + y[t-1,j] + phi2[j] * y[t-2,j] + w[t,j] + theta1[j]*w[t-1,j] + theta2[j]* w[t-2,j]
  }
}

# priors
sigma ~ dunif(0, 10)
sigma_w ~ dunif(0, 10)
for(t in 1:n){
  for(j in 1:k){
    w[t,j] ~ dnorm(0, sd = sigma_w)
  }
}
for(j in 1:k){
  phi1[j] ~ dnorm(0, 1)
  phi2[j] ~ dnorm(0, 1)
  theta1[j] ~ dnorm(0, 1)
  theta2[j] ~ dnorm(0, 1)
}

# latents
for(j in 1:k){
  y_0[j] ~ dnorm(0, sd = sigma)
  y_n1[j] ~ dnorm(0, sd = sigma)
  w_0[j] ~ dnorm(0, sd = sigma_w)
  w_n1[j] ~ dnorm(0, sd = sigma_w)
}