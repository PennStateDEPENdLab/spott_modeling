### Model strings for stan
# mstr10: rho   + epsilon
# mstr30: rho   + epsilon + xi
# mstr40: rho   + epsilon + xi + gobias
# mstr50: rho*2 + epsilon + xi + gobias
# mstr51: multivariate sampling of mstr50
# mstr60: rho*2 + epsilon + xi + gobias + pibias
# mstr61: multivariate sampling of mstr60
# mstr70: theta models
# CSDs:   for multiple groups, fitting different means and SDs

mstr10 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  
  matrix[2,4]   Qi;
}
parameters{
# Hierarchical parameters
  vector[2] X;
  vector<lower=0, upper=20>[2] sdX;

# Subject level parameters
  vector<lower=-10, upper=10>[Nsub] x1; # For Matt trick
  vector<lower=-8,  upper=8>[Nsub] x3;
}
transformed parameters{
  real rho[Nsub];
  real<lower=0, upper=1> epsilon[Nsub];

  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[sx]     <- exp(x1[sx]);
    epsilon[sx] <- inv_logit(x3[sx]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      p0   <- softmax(q);                       # E3 - Probability of action
    
      BGx[(sx-1)*N + tr] <- p0[1];
    
      er                   <- rho[sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(0, 2);
  sdX   ~ cauchy(0, 2);

# Subject level priors
  x1 ~ normal(X[1], sdX[1]);
  x3 ~ normal(X[2], sdX[2]);
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
} 
"

###
mstr20 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
}
parameters{
# Hierarchical parameters
  vector[2] X;
  vector<lower=0, upper=20>[2] sdX;

# Subject level parameters
  vector<lower=-8, upper=8>[Nsub] x1; # For Matt trick
  vector<lower=-8, upper=8>[Nsub] x3;
}
transformed parameters{
  real rho[Nsub];
  real<lower=0, upper=1> epsilon[Nsub];

  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[sx]     <- exp(x1[sx]);
    epsilon[sx] <- inv_logit(x3[sx]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      p0   <- softmax(q);                       # E3 - Probability of action

      BGx[(sx-1)*N + tr] <- p0[1];

      er                   <- rho[sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(0, 3);
  sdX   ~ cauchy(0, 2);
# Subject level priors
  x1 ~ normal(X[1], sdX[1]);
  x3 ~ normal(X[2], sdX[2]);
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"
###
mstr30 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
}
parameters{
# Hierarchical parameters
  vector[3] X;
  vector<lower=0, upper=20>[3] sdX;

# Subject level parameters
  vector<lower=-8, upper=8>[Nsub] x1; # For Matt trick
  vector<lower=-8, upper=8>[Nsub] x3;
  vector<lower=-8, upper=8>[Nsub] x4;
}
transformed parameters{
  real rho[Nsub];
  real<lower=0, upper=1> epsilon[Nsub];
  real<lower=0, upper=1> xi[Nsub];

  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[sx]     <- exp(x1[sx]);
    epsilon[sx] <- inv_logit(x3[sx]);
    xi[sx]      <- inv_logit(x4[sx]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr] <- pxi;
  
      er                   <- rho[sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(0, 3);
  X[3]  ~ normal(3.5,2);
  sdX   ~ cauchy(0, 2);
# Subject level priors
  x1 ~ normal(X[1], sdX[1]);
  x3 ~ normal(X[2], sdX[2]);
  x4 ~ normal(X[3], sdX[3]);
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
} 
"

###
mstr40 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
}
parameters{
# Hierarchical parameters
  vector[4] X;
  vector<lower=0, upper=20>[4] sdX;

# Subject level parameters
  vector<lower=-8, upper=8>[Nsub] x1; # For Matt trick
  vector<lower=-8, upper=8>[Nsub] x3;
  vector<lower=-8, upper=8>[Nsub] x4;
  vector<lower=-8, upper=8>[Nsub] x5;
}
transformed parameters{
  vector[Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;

  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;
  
  vector[N*Nsub] BGx;
  
  for(sx in 1:Nsub){
    rho[sx]     <- exp(x1[sx]);
    epsilon[sx] <- inv_logit(x3[sx]);
    xi[sx]      <- inv_logit(x4[sx]);
  }
  gobias  <- x5;

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx];
      p0     <- softmax(q);                       # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
      BGx[(sx-1)*N + tr] <- pxi;
    
      er                   <- rho[sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(0, 3);
  X[3]  ~ normal(3.5,2);
  X[4]  ~ normal(0, 3);
  sdX   ~ cauchy(0, 2);
# Subject level priors
  x1 ~ normal(X[1], sdX[1]);
  x3 ~ normal(X[2], sdX[2]);
  x4 ~ normal(X[3], sdX[3]);
  x5 ~ normal(X[4], sdX[4]);
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr41 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
}
parameters{
  corr_matrix[4] Omega; # Correlation matrix k = number of parameters
  vector<lower=0, upper=20>[4] tauX; # scale/sd

# Hierarchical parameters
  vector[4] X;

# Subject level parameters
  vector<lower=-10, upper=10>[4] x[Nsub];
}
transformed parameters{
  vector[Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;

  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;
  
  vector[N*Nsub] BGx;
  
  for(sx in 1:Nsub){
    rho[sx]     <- exp(x[sx,1]);
    epsilon[sx] <- inv_logit(x[sx,2]);
    xi[sx]      <- inv_logit(x[sx,3]);
    gobias[sx]  <- x[sx,4];
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx];
      p0     <- softmax(q);                       # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
      BGx[(sx-1)*N + tr]   <- pxi;
    
      er                   <- rho[sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
  matrix[4,4] Sigma;
  matrix[4,4] L_beta;
  Sigma  <- diag_pre_multiply(tauX, diag_post_multiply(Omega, tauX));
  L_beta <- cholesky_decompose(Sigma);
  
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(0, 3);
  tauX  ~ cauchy(0, 2);
  Omega ~ lkj_corr(4);

# Subject level priors
  x ~ multi_normal_cholesky(X, L_beta);
  
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr50 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  K;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
}
parameters{
# Hierarchical parameters
  vector[5] X;
  vector<lower=0, upper=20>[5] sdX;
  
# Subject level parameters
  vector<lower=-8, upper=8>[Nsub] x1;
  vector<lower=-8, upper=8>[Nsub] x2;
  vector<lower=-8, upper=8>[Nsub] x3;
  vector<lower=-8, upper=8>[Nsub] x4;
  vector<lower=-8, upper=8>[Nsub] x5;
}
transformed parameters{
  vector[Nsub] rho[2];
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x1[sx]);
    rho[2, sx]  <- exp(x2[sx]);
    epsilon[sx] <- inv_logit(x3[sx]);
    xi[sx]      <- inv_logit(x4[sx]);
  }
  gobias  <- x5;

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx];
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
  
      BGx[(sx-1)*N + tr] <- pxi;
    
      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(3.5, 2);
  X[5]  ~ normal(0, 3);
  sdX   ~ cauchy(0, 2);
# Subject level priors
  x1 ~ normal(X[1], sdX[1]);
  x2 ~ normal(X[2], sdX[2]);
  x3 ~ normal(X[3], sdX[3]);
  x4 ~ normal(X[4], sdX[4]);
  x5 ~ normal(X[5], sdX[5]);
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr51 = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
}
parameters{
# Correlation matrix and scale
  corr_matrix[K] Omega;
  vector<lower=0, upper=20>[K] tauX;

# Hierarchical parameters
  vector[K] X;
  
# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx];
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr] <- pxi;

      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
    }
  }
}
model{
  matrix[K,K] Sigma;
  matrix[K,K] L_beta;
  Sigma  <- diag_pre_multiply(tauX, diag_post_multiply(Omega, tauX));
  L_beta <- cholesky_decompose(Sigma);

# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(3.5, 2);
  X[5]  ~ normal(0, 3);
  tauX  ~ cauchy(0, 2);
  Omega ~ lkj_corr(5);

# Subject level priors
  x ~ multi_normal_cholesky(X, L_beta);

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr60 = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  K;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
  vector[4]   Vi;
}
parameters{
# Hierarchical parameters
  vector[K] X;
  vector<lower=0, upper=20>[K] sdX;

# Subject level parameters
  vector<lower=-8, upper=8>[Nsub] x[K];
}
transformed parameters{
  vector[Nsub] rho[2];
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  
  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;
  
  vector[N*Nsub] BGx;
  
  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[1, sx]);
    rho[2, sx]  <- exp(x[2, sx]);
    epsilon[sx] <- inv_logit(x[3, sx]);
    xi[sx]      <- inv_logit(x[4, sx]);
    gobias[sx]  <- x[5, sx];
    pibias[sx]  <- exp(x[6, sx]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx] + pibias[sx]*V[s[sx,tr]]; # pibias only on go
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr] <- pxi;
  
      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(3.5, 2);
  X[5]  ~ normal(0, 3);
  X[6]  ~ normal(0, 3);
  sdX   ~ cauchy(0, 2);
# Subject level priors
  for(k in 1:K){
    x[k] ~ normal(X[k], sdX[k]);
  }
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr60CSD = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  NGroup;
  int<lower=1>  group[Nsub];
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Hierarchical parameters
  vector[K] X[NGroup];
  vector<lower=0, upper=20>[K] sdX[NGroup];

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  real rho[2];
  real<lower=0, upper=1> epsilon;
  real<lower=0, upper=1> xi;
  real gobias;
  real pibias;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1]   <- exp(x[sx, 1]);
    rho[2]   <- exp(x[sx, 2]);
    epsilon  <- inv_logit(x[sx, 3]);
    xi       <- inv_logit(x[sx, 4]);
    gobias   <- x[sx, 5];
    pibias   <- exp(x[sx, 6]);
  
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias + pibias*V[s[sx,tr]]; # pibias only on go
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi*p0[1] + (1-xi)/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
      
      BGx[(sx-1)*N + tr] <- pxi;
      
      er                   <- rho[rew[sx,tr]] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  for(gr in 1:NGroup){
    X[gr,1]  ~ normal(2, 3);
    X[gr,2]  ~ normal(2, 3);
    X[gr,3]  ~ normal(0, 3);
    X[gr,4]  ~ normal(3.5, 2);
    X[gr,5]  ~ normal(0, 3);
    X[gr,6]  ~ normal(0, 3);

    sdX[gr]  ~ cauchy(0, 2);
  }
# Subject level priors
  for(sx in 1:Nsub){
    x[sx] ~ normal(X[group[sx]], sdX[group[sx]]);
  }
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

# Can't remember what this one was for???
mstr60CSD2 = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  NGroup;
  int<lower=1>  group[Nsub];
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Hierarchical parameters
  vector[K] X[NGroup];
  vector<lower=0, upper=20>[K] sdX[NGroup];

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  real rho[2];
  real<lower=0, upper=1> epsilon;
  real<lower=0, upper=1> xi;
  real gobias;
  real pibias;
  
  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1]   <- exp(x[sx, 1]);
    rho[2]   <- exp(x[sx, 2]);
    epsilon  <- inv_logit(x[sx, 3]);
    xi       <- inv_logit(x[sx, 4]);
    gobias   <- x[sx, 5];
    pibias   <- x[sx, 6];

    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias + pibias*V[s[sx,tr]]; # pibias only on go
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi*p0[1] + (1-xi)/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
      BGx[(sx-1)*N + tr] <- pxi;
    
      er                   <- rho[rew[sx,tr]] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  for(gr in 1:NGroup){
    X[gr,1]  ~ normal(2, 3);
    X[gr,2]  ~ normal(2, 3);
    X[gr,3]  ~ normal(0, 3);
    X[gr,4]  ~ normal(3.5, 2);
    X[gr,5]  ~ normal(0, 3);
    X[gr,6]  ~ normal(0, 3);

    sdX[gr]  ~ cauchy(0, 2);
  }
# Subject level priors
  for(sx in 1:Nsub){
    x[sx] ~ normal(X[group[sx]], sdX[group[sx]]);
  }
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

# Matt trick - meant to improve sampling for Stan's sampling for hierarchical models...
mstr60CSDmtrick = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  NGroup;
  int<lower=1>  group[Nsub];
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Hierarchical parameters
  vector[K] X[2];
  vector<lower=0, upper=20>[K] sdX[2];

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  real rho[2];
  real<lower=0, upper=1> epsilon;
  real<lower=0, upper=1> xi;
  real gobias;
  real pibias;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;
  
  vector[N*Nsub] BGx;
  
  for(sx in 1:Nsub){
    rho[1]   <- exp(X[group[sx],1]       + x[sx, 1] * sdX[group[sx],1]);
    rho[2]   <- exp(X[group[sx],2]       + x[sx, 2] * sdX[group[sx],2]);
    epsilon  <- inv_logit(X[group[sx],3] + x[sx, 3] * sdX[group[sx],3]);
    xi       <- inv_logit(X[group[sx],4] + x[sx, 4] * sdX[group[sx],4]);
    gobias   <- X[group[sx],5]           + x[sx, 5] * sdX[group[sx],5];
    pibias   <- exp(X[group[sx],6]       + x[sx, 6] * sdX[group[sx],6]);
    
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias + pibias*V[s[sx,tr]]; # pibias only on go???
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi*p0[1] + (1-xi)/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
  
      BGx[(sx-1)*N + tr] <- pxi;

      er                   <- rho[rew[sx,tr]] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  for(gr in 1:NGroup){
    X[gr,1]  ~ normal(2, 3);
    X[gr,2]  ~ normal(2, 3);
    X[gr,3]  ~ normal(0, 3);
    X[gr,4]  ~ normal(3.5, 2);
    X[gr,5]  ~ normal(0, 3);
    X[gr,6]  ~ normal(0, 3);

    sdX[gr]  ~ cauchy(0, 2);
  }
# Subject level priors
  for(sx in 1:Nsub){
    x[sx] ~ normal(0, 1);
    #x[sx] ~ normal(X[group[sx]], sdX[group[sx]]);
  }
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"


mstr61 = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Correlation matrix and scale
  corr_matrix[K] Omega;
  vector<lower=0, upper=20>[K] tauX;

# Hierarchical parameters
  vector[K] X;

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx] + pibias[sx]*V[s[sx,tr]];
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
      
      BGx[(sx-1)*N + tr] <- pxi;

      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
  matrix[K,K] Sigma;
  matrix[K,K] L_beta;
  Sigma  <- diag_pre_multiply(tauX, diag_post_multiply(Omega, tauX));
  L_beta <- cholesky_decompose(Sigma);

# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(0, 3);
  X[5]  ~ normal(3.5, 2);
  X[6]  ~ normal(0, 3);
  tauX  ~ cauchy(0, 2);
  Omega ~ lkj_corr(4);

# Subject level priors
  x ~ multi_normal_cholesky(X, L_beta);

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr70a = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  int<lower=0>  confl[Nsub, N];
  vector[N]     theta[Nsub];

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Scale
  vector<lower=0, upper=20>[K] sdX;

# Hierarchical parameters
  vector[K] X;

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  vector[Nsub] tbeta;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
    tbeta[sx]   <- x[sx,7];
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      if(confl[sx,tr] == 1){
        q[1]   <- q[1] + gobias[sx] + (pibias[sx] + tbeta[sx] * theta[sx,tr]) * V[s[sx,tr]]; # change theta to 0 for non-conflict trials
      } else {
        q[1]   <- q[1] + gobias[sx] + pibias[sx] * V[s[sx,tr]]; # change theta to 0 for non-conflict trials
      }

      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr]   <- pxi;
  
      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity

      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(3.5, 2);
  X[5]  ~ normal(0, 3);
  X[6]  ~ normal(0, 3);
  X[7]  ~ normal(0, 3);
  sdX   ~ cauchy(0, 2);

# Subject level priors
  for(sx in 1:Nsub){
    x[sx] ~ normal(X, sdX);
  }

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr70CSD = "
data{
  int<lower=1>  N;
  int<lower=1>  Nsub;
  int<lower=1>  K;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  int<lower=1>  group[Nsub];

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Hierarchical parameters
  vector[K] X[2];
  vector<lower=0, upper=20>[K] sdX[2];

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  vector[Nsub] rho[2];
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  vector[Nsub] stick;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
    stick[sx]   <- x[sx,7];
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      if(tr == 1){
        q[1]   <- q[1] + gobias[sx] + pibias[sx]*V[s[sx,tr]]; # pibias only on go???
      } else {
        q[1]   <- q[1] + gobias[sx] + pibias[sx]*V[s[sx,tr]] + stick[sx]*ya[sx,tr-1]; # pibias only on go???
        q[2]   <- q[2] + stick[sx] * (ya[sx,tr-1]==0); # pibias only on go???
      }
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr] <- pxi;

      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  for(gr in 1:2){
    X[gr,1]  ~ normal(2, 3);
    X[gr,2]  ~ normal(2, 3);
    X[gr,3]  ~ normal(0, 3);
    X[gr,4]  ~ normal(3.5, 2);
    X[gr,5]  ~ normal(0, 3);
    X[gr,6]  ~ normal(0, 3);
    X[gr,7]  ~ normal(0, 3);
    sdX[gr]  ~ cauchy(0, 2);
  }
# Subject level priors
  for(sx in 1:Nsub){
    x[sx] ~ normal(X[group[sx]], sdX[group[sx]]);
  }
# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr70aCSD = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  int<lower=0>  confl[Nsub, N];
  vector[N]     theta[Nsub];
  int<lower=1>  group[Nsub];

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Scale
  vector<lower=0, upper=20>[K] sdX[2];

# Hierarchical parameters
  vector[K] X[2];

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  vector[Nsub] tbeta;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
    tbeta[sx]   <- x[sx,7];
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx] + (pibias[sx] + tbeta[sx] * theta[sx,tr]) * V[s[sx,tr]];

      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr]   <- pxi;

      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
  
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]]          + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
# Hierarchical priors
  for(gr in 1:2){
    X[gr,1]  ~ normal(2, 3);
    X[gr,2]  ~ normal(2, 3);
    X[gr,3]  ~ normal(0, 3);
    X[gr,4]  ~ normal(3.5, 2);
    X[gr,5]  ~ normal(0, 3);
    X[gr,6]  ~ normal(0, 3);
    X[gr,7]  ~ normal(0, 3);

    sdX[gr]  ~ cauchy(0, 2);
  }

# Subject level priors
  for(sx in 1:Nsub){
    x[sx] ~ normal(X[group[sx]], sdX[group[sx]]);
  }

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"


mstr71a = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  vector[N]     theta[Nsub];

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Correlation matrix and scale
  corr_matrix[K] Omega;
  vector<lower=0, upper=20>[K] tauX;

# Hierarchical parameters
  vector[K] X;

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  vector[Nsub] tbeta;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
    tbeta[sx]   <- exp(x[sx,7]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- q[1] + gobias[sx] + (pibias[sx] + tbeta[sx] * theta[sx,tr]) * V[s[sx,tr]];
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise

      BGx[(sx-1)*N + tr]   <- pxi;

      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity

      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
  matrix[K,K] Sigma;
  matrix[K,K] L_beta;
  Sigma  <- diag_pre_multiply(tauX, diag_post_multiply(Omega, tauX));
  L_beta <- cholesky_decompose(Sigma);

# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(2, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(3.5, 2);
  X[5]  ~ normal(0, 3);
  X[6]  ~ normal(0, 3);
  X[7]  ~ normal(0, 3);
  tauX  ~ cauchy(0, 2);
  Omega ~ lkj_corr(4);
  
# Subject level priors
  x ~ multi_normal_cholesky(X, L_beta);

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr71b = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  vector[N]     theta[Nsub];

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Correlation matrix and scale
  corr_matrix[K] Omega;
  vector<lower=0, upper=20>[K] tauX;

# Hierarchical parameters
  vector[K] X;

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  vector[Nsub] tbeta;

  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
    tbeta[sx]   <- exp(x[sx,7]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- (1 - tbeta[sx] * theta[sx,tr]) * q[1] + gobias[sx] + pibias[sx] * V[s[sx,tr]];
      q[2]   <- (1 - tbeta[sx] * theta[sx,tr]) * q[2];

      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
      
      BGx[(sx-1)*N + tr]   <- pxi;
    
      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity
      
      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
  matrix[K,K] Sigma;
  matrix[K,K] L_beta;
  Sigma  <- diag_pre_multiply(tauX, diag_post_multiply(Omega, tauX));
  L_beta <- cholesky_decompose(Sigma);

# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(0, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(0, 3);
  X[5]  ~ normal(0, 3);
  X[6]  ~ normal(0, 3);
  X[7]  ~ normal(0, 3);
  tauX  ~ cauchy(0, 2);
  Omega ~ lkj_corr(4);
  
# Subject level priors
  x ~ multi_normal_cholesky(X, L_beta);

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

mstr71c = "
data{
  int<lower=1>  N;
  int<lower=1>  K;
  int<lower=1>  Nsub;
  int<lower=1>  s[Nsub, N];
  int<lower=1>  a[Nsub, N];
  int<lower=0>  ya[Nsub, N];
  int<lower=-1> r[Nsub, N];
  int<lower=1>  rew[Nsub, N];
  int<lower=0>  yax[N*Nsub];
  int<lower=1>  tN;
  vector[N]     theta[Nsub];

  matrix[2,4]   Qi;
  vector[4]     Vi;
}
parameters{
# Correlation matrix and scale
  corr_matrix[K] Omega;
  vector<lower=0, upper=20>[K] tauX;
  
# Hierarchical parameters
  vector[K] X;

# Subject level parameters
  vector<lower=-8, upper=8>[K] x[Nsub];
}
transformed parameters{
  matrix[2,Nsub] rho;
  vector<lower=0, upper=1>[Nsub] epsilon;
  vector<lower=0, upper=1>[Nsub] xi;
  vector[Nsub] gobias;
  vector[Nsub] pibias;
  vector[Nsub] tbeta;
  vector[Nsub] W;
  
  vector[4]   V;
  matrix[2,4] Q;
  vector[2]   q;
  vector<lower=0.00000000000000000001, upper=0.99999999999999999999>[2]   p0;
  real        pxi;
  real        er;

  vector[N*Nsub] BGx;

  for(sx in 1:Nsub){
    rho[1, sx]  <- exp(x[sx,1]);
    rho[2, sx]  <- exp(x[sx,2]);
    epsilon[sx] <- inv_logit(x[sx,3]);
    xi[sx]      <- inv_logit(x[sx,4]);
    gobias[sx]  <- x[sx,5];
    pibias[sx]  <- exp(x[sx,6]);
    tbeta[sx]   <- exp(x[sx,7]);
    W[sx]       <- inv_logit(x[sx,8]);
  }

  for(sx in 1:Nsub){
    Q           <- Qi;
    V           <- Vi;
    for(tr in 1:N){
      for(iq in 1:2){
        q[iq]   <- Q[iq , s[sx,tr]];                # E2 - q, expected value, carries over from previous trial
      }
      q[1]   <- (1 - (W + tbeta[sx] * theta[sx,tr])) * q[1] + (W + tbeta[sx] * theta[sx,tr]) * V[s[sx,tr]] + gobias[sx];
      q[2]   <- (1 - (W + tbeta[sx] * theta[sx,tr])) * q[2];
  
      p0     <- softmax(q);                         # E3 - Probability of action
      pxi    <- xi[sx]*p0[1] + (1-xi[sx])/2;        # E3 - M2 irreducible noise 'xi' or 'g': action + noise
  
      BGx[(sx-1)*N + tr]   <- pxi;

      er                   <- rho[rew[sx,tr], sx] * r[sx,tr];      # E2 - Rew/Pun sensitivity

      Q[a[sx,tr],s[sx,tr]] <- Q[a[sx,tr],s[sx,tr]] + epsilon[sx] * (er - Q[a[sx,tr],s[sx,tr]]);
      V[s[sx,tr]]          <- V[s[sx,tr]] + epsilon[sx] * (er - V[s[sx,tr]]);
    }
  }
}
model{
  matrix[K,K] Sigma;
  matrix[K,K] L_beta;
  Sigma  <- diag_pre_multiply(tauX, diag_post_multiply(Omega, tauX));
  L_beta <- cholesky_decompose(Sigma);
  
# Hierarchical priors
  X[1]  ~ normal(2, 3);
  X[2]  ~ normal(0, 3);
  X[3]  ~ normal(0, 3);
  X[4]  ~ normal(0, 3);
  X[5]  ~ normal(0, 3);
  X[6]  ~ normal(0, 3);
  X[7]  ~ normal(0, 3);
  X[8]  ~ normal(0, 3);
  tauX  ~ cauchy(0, 2);
  Omega ~ lkj_corr(4);
  
# Subject level priors
  x ~ multi_normal_cholesky(X, L_beta);

# Likelihood
  yax ~ bernoulli(BGx);
}
generated quantities {
  vector[tN] log_lik; # To calculate WAIC
  for (n in 1:tN){
    log_lik[n] <- bernoulli_log(yax[n], BGx[n]);
  }
}
"

