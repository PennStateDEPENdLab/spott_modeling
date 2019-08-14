M1 <- function(rho, epsilon, Qi, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  Q       <- Qi
  q       <- matrix(0, nrow=N, ncol=2)
  a       <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    
    a[tr]   <- rbinom(1,1,p0[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1), 1, 0)
    
    er      <- rho * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]]);
  }
  list(Q=Q, out=out, a=a)
}

# Model 2
M2 <- function(rho, epsilon, Qi, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  Q <- Qi
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    
    a[tr]   <- rbinom(1, 1, p0[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1), 1, 0)
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0), -1, out[tr])
    
    er      <- rho * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]]);
  }
  list(Q=Q, out=out, a=a)
}

# Model 3
M3 <- function(rho, epsilon, xi, Qi, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  
  Q <- Qi
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    er      <- rho * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]]);
  }
  list(Q=Q, out=out, a=a)
}

# Model 4
M4 <- function(rho, epsilon, xi, gobias, Qi, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  
  Q <- Qi
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    q[tr,1] <- q[tr,1] + gobias
    
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    er      <- rho * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]]);
  }
  list(Q=Q, out=out, a=a)
}

# Model 5
M5 <- function(rho, epsilon, xi, gobias, Qi, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  
  Q <- Qi
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    q[tr,1] <- q[tr,1] + gobias
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    iq      <- ifelse(rew[tr] == 1, 1, 2)
    er      <- rho[iq] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]]);
  }
  list(Q=Q, out=out, a=a)
}


# Model 6
M6 <- function(rho, epsilon, xi, gobias, pibias, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  pibias  <- exp(pibias)
  
  Q <- matrix(0, nrow=2, ncol=4)
  V <- rep(0, 4)
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    q[tr,1] <- q[tr,1] + gobias + pibias * V[s[tr]]
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    p0      <- ifelse(is.na(p0), 1, p0)
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    iq      <- ifelse(rew[tr] == 1, 1, 2)
    er      <- rho[iq] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]])
    V[s[tr]]        <- V[s[tr]] + epsilon * (er - V[s[tr]])
  }
  list(Q=Q, out=out, a=a, V=V)
}

# Model 6 extraction
M6Qex <- function(rho, epsilon, xi, gobias, pibias, s, rew, tt, vv, N, out, a){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  pibias  <- exp(pibias)
  
  Q   <- matrix(0, nrow=2, ncol=4)
  Qup <- matrix(0, nrow=192, ncol=7)
  V   <- rep(0, 4)
  Vup <- matrix(0, nrow=192, ncol=2)
  q   <- matrix(0, nrow=N, ncol=2)

  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    q[tr,1] <- q[tr,1] + gobias + pibias * V[s[tr]]
      
    er      <- rho[rew[tr]] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[a[tr],s[tr]]  <- Q[a[tr],s[tr]] + epsilon * (er - Q[a[tr],s[tr]])
    V[s[tr]]        <- V[s[tr]] + epsilon * (er - V[s[tr]])
    
    Qup[tr,] <- c(Q[a[tr], s[tr]],  s[tr], a[tr], out[tr], rew[tr], er, (er - Q[a[tr],s[tr]]))
    Vup[tr,] <- c(V[s[tr]],         (er - V[s[tr]]))
  }
  colnames(Qup) <- c("Q", "s", "a", "out", "rew", "er", "er-Q")
  colnames(Vup) <- c("V", "er-V")
  QVup=cbind(Qup, Vup)
}


# Model 7
M7 <- function(rho, epsilon, xi, gobias, pibias, stick, s, rew, tt, vv, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  pibias  <- exp(pibias)
  stick   <- stick
  
  Q <- matrix(0, nrow=2, ncol=4)
  V <- rep(0, 4)
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    if(tr == 1){
      q[tr,1] <- q[tr,1] + gobias + pibias * V[s[tr]]
    } else {
      q[tr,1] <- q[tr,1] + gobias + pibias * V[s[tr]] + stick * a[tr-1]
      q[tr,2] <- q[tr,2] + stick * (a[tr-1]==0)
    }
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    p0      <- ifelse(is.na(p0), 1, p0)
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    iq      <- ifelse(rew[tr] == 1, 1, 2)
    er      <- rho[iq] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]])
    V[s[tr]]        <- V[s[tr]] + epsilon * (er - V[s[tr]])
  }
  list(Q=Q, out=out, a=a, V=V)
}

# Model 7a
M7a <- function(rho, epsilon, xi, gobias, pibias, tbeta, s, rew, tt, vv, theta, confl, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  pibias  <- exp(pibias)
  tbeta   <- tbeta
  
  Q <- matrix(0, nrow=2, ncol=4)
  V <- rep(0, 4)
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    
    if(confl[tr] == 1){
      q[1]   <- q[1] + gobias + (pibias + tbeta * theta[tr]) * V[s[tr]]; # change theta to 0 for non-conflict trials
    } else {
      q[1]   <- q[1] + gobias + pibias * V[s[tr]]; # change theta to 0 for non-conflict trials
    }
    
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    iq      <- ifelse(rew[tr] == 1, 1, 2)
    er      <- rho[iq] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]])
    V[s[tr]]        <- V[s[tr]] + epsilon * (er - V[s[tr]])
  } 
  list(Q=Q, out=out, a=a, V=V)
}

# Model 7b
M7b <- function(rho, epsilon, xi, gobias, pibias, tbeta, s, rew, tt, vv, theta, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  pibias  <- exp(pibias)
  tbeta   <- exp(tbeta)
  
  Q <- matrix(0, nrow=2, ncol=4)
  V <- rep(0, 4)
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    q[tr,1] <- (1 - tbeta * theta[tr]) * q[tr,1] + gobias + pibias * V[s[tr]]
    q[tr,2] <- (1 - tbeta * theta[tr]) * q[tr,2]
    
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    iq      <- ifelse(rew[tr] == 1, 1, 2)
    er      <- rho[iq] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]])
    V[s[tr]]        <- V[s[tr]] + epsilon * (er - V[s[tr]])
  }
  list(Q=Q, out=out, a=a, V=V)
}

# Model 7c
M7c <- function(rho, epsilon, xi, gobias, pibias, tbeta, W, s, rew, tt, vv, theta, N){
  rho     <- exp(rho)
  epsilon <- inv.logit(epsilon)
  xi      <- inv.logit(xi)
  gobias  <- gobias
  pibias  <- exp(pibias)
  tbeta   <- exp(tbeta)
  W       <- inv.logit(W)
  
  Q <- matrix(0, nrow=2, ncol=4)
  V <- rep(0, 4)
  q <- matrix(0, nrow=N, ncol=2)
  a <- rep(0, N)
  out <- ar <- a
  for(tr in 1:N){
    q[tr,]  <- Q[,s[tr]]
    q[tr,1] <- (1 - (W + tbeta * theta[tr])) * q[tr,1] + gobias + (W + tbeta * theta[tr]) * V[s[tr]]
    q[tr,2] <- (1 - (W + tbeta * theta[tr])) * q[tr,2]
    
    p0      <- exp(q[tr,])/sum(exp(q[tr,]))
    pxi     <- xi*p0 + (1-xi)/2;                 # E3 - M2 irreducible noise 'xi' or 'g': action + noise
    
    a[tr]   <- rbinom(1,1,pxi[1])
    ar[tr]  <- ifelse(a[tr] == 0, 2, 1)
    
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==1)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==1),
                      1, 0)
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Valid" & rew[tr]==0)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Valid" & rew[tr]==0),
                      -1, out[tr])
    out[tr] <- ifelse((a[tr]==0 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==1)|
                        (a[tr]==1 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==1),
                      1, out[tr])
    out[tr] <- ifelse((a[tr]==1 & tt[tr]=="Go" & vv[tr]=="Invalid" & rew[tr]==0)|
                        (a[tr]==0 & tt[tr]=="No-go" & vv[tr]=="Invalid" & rew[tr]==0),
                      -1, out[tr])
    
    iq      <- ifelse(rew[tr] == 1, 1, 2)
    er      <- rho[iq] * out[tr];                        # E2 - Rew/Pun sensitivity
    
    Q[ar[tr],s[tr]] <- Q[ar[tr],s[tr]] + epsilon * (er - Q[ar[tr],s[tr]])
    V[s[tr]]        <- V[s[tr]] + epsilon * (er - V[s[tr]])
  }
  list(Q=Q, out=out, a=a, V=V)
}

