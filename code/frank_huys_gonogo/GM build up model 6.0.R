## Parameter recovery GM
# Load libraries
require(rstan); require(data.table); require(parallel); require(boot); require(R.matlab)
source("/local/Dropbox/R scripts/REEG/GM/GMfxModel3.R") # To simulate data
source("/local/Dropbox/R scripts/REEG/waic.R")       # To calculate WAIC
source("/local/Dropbox/R scripts/REEG/GM/GM model strings clean.st.R") # Load's stan model strings

# Generate data
Nsub <- 60 
Qi   <- matrix(0, 2, 4)
s    <- rep(1:4, 50)
N    <- length(s)
rew  <- ifelse(s %in% c(2,4), 1, 0)
tt   <- ifelse(s %in% c(1,2), "Go", "No-go")
vv   <- sample(c("Valid", "Invalid"), N, T, c(0.7, 0.3))

# Random parameters for simulation
rho     <- rnorm(Nsub, 1, 2)
rho     <- cbind(rho, rnorm(Nsub, 0, 2))
rho     <- ifelse(rho>6, 6, rho)
epsilon <- rnorm(Nsub, 0, 2)
xi      <- rnorm(Nsub, 0, 2)
gobias  <- rnorm(Nsub, 0, 2)
pibias  <- rnorm(Nsub, 0, 1)

M6out <- list()
for(k in 1:Nsub){
  M6out[[k]] <- M6(rho[k,], epsilon[k], xi[k], gobias[k], pibias[k], s, rew, tt, vv, N)
}
data.table(a=M6out[[1]]$a, out=M6out[[1]]$out, s=s, rew=rew, tt=tt, valid=vv)

#### GM
vvb <- ttb <- rewxb <- rewb <- rb <- sb <- ab <- ya <- matrix(0, nrow=Nsub, ncol=N)
for(sx in 1:Nsub){
  ya[sx,]    = M6out[[sx]]$a
  ab[sx,]    = ifelse(ya[sx,] == 1, 1, 2)
  sb[sx,]    = s
  rb[sx,]    = M6out[[sx]]$out
  rewb[sx,]  = rew
  rewxb[sx,] = ifelse(rewb[sx,] == 1, 1, 2)
  vvb[sx,]   = vv
  ttb[sx,]   = tt
}
Qi   = matrix(0, nrow=2, ncol=4);
Vi   = rep(0, 4)

yax <- matrix(0, Nsub*N) # Generate one long action vector to vectorise for Stan
for(sx in 1:Nsub){
  for(tr in 1:N){
    yax[(sx-1)*N + tr] <- ya[sx, tr]
  }
}

dList <- list(ya=ya, yax=yax[,1], a=ab, s=sb, r=rb, rew=rewxb, N=N, Qi=Qi, Nsub=Nsub, K=6, tN=length(yax), Vi=Vi)

# Output to Matlab to compare
writeMat('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMbehav6.0.mat', ya=ya, a=ab, s=sb, r=rb, rew=rewxb, N=N, Qi=Qi, yax=yax, Nsub=Nsub)

# Fit model
seed = 69

f1 <- stan(model_code = mstr61, data=dList, iter=10, init=0,
           chains=1, seed=seed, chain_id=1, pars=c("X", "Q", "q"))
sflist1 <-
  mclapply(1:4, mc.cores=4, function(i) # parallelised sampling
    f2m <- stan(fit=f1, data=dList, iter=800, chains=4, seed=seed+1,
               pars=c("X", "x", "tauX", "Q", "q", "log_lik"), init=0))
f2 <- sflist2stanfit(sflist1)

# Load matlab fits
E <- readMat('/local/Dropbox/EEG data/MPRC/REEG/Behav/GMBehavOutput6.0.mat')$E

xmat <- data.table(x1=E[1,], x2=E[2,], x3=E[3,], x4=E[4,], x5=E[5,], x6=E[6,])

fmeans  <- colMeans(as.matrix(f2))
xbay <- data.table(x1=fmeans[seq(7, Nsub*6 + 1, 6)],
                   x2=fmeans[seq(7, Nsub*6 + 1, 6) + 1],
                   x3=fmeans[seq(7, Nsub*6 + 1, 6) + 2], 
                   x4=fmeans[seq(7, Nsub*6 + 1, 6) + 3], 
                   x5=fmeans[seq(7, Nsub*6 + 1, 6) + 4],
                   x6=fmeans[seq(7, Nsub*6 + 1, 6) + 5])
df1  <- data.table(mx1 = xmat[, x1], bx1 = xbay[, x1], rho1=rho[,1],
                   mx2 = xmat[, x2], bx2 = xbay[, x2], rho2=rho[,2],
                   mx3 = xmat[, x3], bx3 = xbay[, x3], epsilon=epsilon,
                   mx4 = xmat[, x4], bx4 = xbay[, x4], xi=xi,   
                   mx5 = xmat[, x5], bx5 = xbay[, x5], gobias=gobias,
                   mx6 = xmat[, x6], bx6 = xbay[, x6], pibias=pibias)
df1[, m1 := mx1-rho[,1]]
df1[, b1 := bx1-rho[,1]]
df1[, m2 := mx2-rho[,2]]
df1[, b2 := bx2-rho[,2]]
df1[, m3 := mx3-epsilon]
df1[, b3 := bx3-epsilon]
df1[, m4 := mx4-xi]
df1[, b4 := bx4-xi]
df1[, m5 := mx5-gobias]
df1[, b5 := bx5-gobias]
df1[, m6 := mx6-pibias]
df1[, b6 := bx6-pibias]
apply(df1, 2, function(x) mean(abs(x)))
apply(df1, 2, mean)
apply(df1, 2, sd)


X11(); par(mfrow=c(3,2), mar=c(4,4,2,1))
plot(df1[, rho1], df1[, bx1], ylim=c(-4,6), xlim=c(-4,6), bty="l")
points(df1[, rho1], df1[, mx1], col=2)
abline(0, 1)
legend(-4,6,c("Bayes", "EM"), col=c(1,2), pch=1, bty="n")

plot(df1[, rho2], df1[, bx2], ylim=c(-5,5), xlim=c(-5,5), bty="l")
points(df1[, rho2], df1[, mx2], col=2)
abline(0, 1)

plot(df1[, epsilon], df1[, bx3], ylim=c(-5,5), xlim=c(-5,5), bty="l")
points(df1[, epsilon], df1[, mx3], col=2)
abline(0, 1)

plot(df1[, xi], df1[, bx4], ylim=c(-5,5), xlim=c(-5,5), bty="l")
points(df1[, xi], df1[, mx4], col=2)
abline(0, 1)

plot(df1[, gobias], df1[, bx5], ylim=c(-5,5), xlim=c(-5,5), bty="l")
points(df1[, gobias], df1[, mx5], col=2)
abline(0, 1)

plot(df1[, pibias], df1[, bx6], ylim=c(-3,3), xlim=c(-3,3), bty="l")
points(df1[, pibias], df1[, mx6], col=2)
abline(0, 1)


# Plots....
X11(); par(mfrow=c(7,3), mar=c(2,2,1,1))
st = 4 # 1-Go-Avoid, 2-Go-Win, 3-NoGo-Avoid, 4-NoGo-Win
xlimx = c(0, 50)
for(sx in 1:7){
  plot(-99, -99, type="l", ylim=c(-30, 30), xlim=xlimx, main=paste("subject", sx, "cond", st), bty="l")
  for(k in 1:20){
    M3outM <- M3(c(exp(xmat$x1[sx]), exp(xmat$x2[sx])), inv.logit(xmat$x3[sx]), inv.logit(xmat$x4[sx]), xmat$x5[sx], Qi, s[sx,], rew[sx,], tt[sx,], vv[sx,])$out
    lines(cumsum(M3outM[s[sx,]==st]), col=k)
  }
  plot(-99, -99, type="l", ylim=c(-30, 30), xlim=xlimx, main=paste("subject", sx, "cond", st), bty="l")
  for(k in 1:20){
    M3outB <- M3(c(exp(xbay$x1[sx]), exp(xbay$x1[sx])), inv.logit(xbay$x3[sx]), inv.logit(xbay$x4[sx]), xbay$x5[sx], Qi, s[sx,], rew[sx,], tt[sx,], vv[sx,])$out
    lines(cumsum(M3outB[s[sx,]==st]), col=k)
  }
  plot(cumsum(r[sx,s[sx,]==st]), type="l", ylim=c(-30, 30), xlim=xlimx, main=paste("subject", sx, "cond", st), bty="l")
}
X11()
