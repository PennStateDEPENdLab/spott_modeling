logfunc <- function(x, num, denom) {
  num / (denom + exp(-x))
}

logfunc(15, 1, 1)
logfunc(-15, 1, 1)

logfunc(15, 10, 1)
logfunc(15, 1, 10)


# logfunc <- function(x, tcur, rtlast, beta=100) {
#   #1 / ((1 - exp((-1*(tcur - rtlast))/beta)) + exp(-x))
#   #penalty <- 1 / (1 + exp(-1*(tcur - rtlast)/beta))
#   #penalty <- 1 / (1 + exp(-1*(tcur - rtlast)/beta))
#   # penalty <- exp(+1*(tcur - rtlast)/beta)
#   #penalty <- log(+1*(tcur - rtlast)/beta)
#   #penalty <- exp(+1*(rtlast - tcur)*beta)
#   #penalty <- exp(+1*(1/(tcur - rtlast))*beta)
#   #penalty <- (tcur - rtlast)^-beta
#   
#   #1 / ((1 - penalty) + exp(-x))
#   #1 / ((1 - penalty) + exp(-x))
#   penalty <- 1 - exp(-(tcur - rtlast)/beta)
#   #1 / ((1 - penalty) + exp(-x))
#   #(penalty / (1 + exp(-x)))
#   penalty
# }

logfunc <- function(x, tcur, rtlast, beta=100) {
  penalty <- 1 - exp(-(tcur - rtlast)/beta)
  penalty / (1 + exp(-x))
}

tcur <- 700
rtprev <- seq(25, 650, by=10)
#beta = 50:120
beta = seq(2, 200, by=5)

g <- expand.grid(tcur=tcur, rtprev=rtprev, beta=beta)
g$presp <- NA
for (i in 1:nrow(g)) {
  g[i, "presp"] <- logfunc(15, g[i, "tcur"], g[i, "rtprev"], beta=g[i, "beta"])
}

library(ggplot2)
ggplot(g, aes(x=tcur - rtprev, y=presp)) + geom_line() + facet_wrap(~beta)
