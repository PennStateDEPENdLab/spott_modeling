#this is the switch probability choice function p(switch)
pnew <- function(Q1, Q2, c, kappa) {
  1/(1+exp(-kappa*(Q1 - c - Q2)))
  #1/(1+exp(-kappa*(Q1*c - Q2)))
}

#df <- expand.grid(Q1 = 1:50, Q2 = 1:50)
df <- expand.grid(Q1 = seq(0, 1, .05), Q2 = seq(0, 1, .05))

#pnew(20, 10, 1, 1)

df$pswitch <- NA
for (i in 1:nrow(df)) {
  #df[i, "pswitch"] <- pnew(df[i, "Q1"], df[i, "Q2"], c=0, kappa=0.01)
  df[i, "pswitch"] <- pnew(df[i, "Q1"], df[i, "Q2"], c=0.0, kappa=1)
} 

library(ggplot2)
ggplot(df, aes(x=Q1, y=Q2, fill=pswitch)) + geom_tile()


presp <- function(Q1, Q2, eta, phi=1) {
  Qstar <- (Q1 + Q2)/eta
  phi / (1+exp(-Qstar))
}

#use 2pl-type approach
presp <- function(Q1, Q2, eta=1, phi=1, gamma=2, nu=1) {
  Qstar <- (Q1 + Q2)/eta
  phi / (1+exp(-gamma*(Qstar - nu)))
}



#df <- expand.grid(Q1 = seq(0.01, 1, 0.1), Q2 = seq(0.01, 1, 0.1), eta=seq(0.1, 2, 0.2))
#df <- expand.grid(Q1 = seq(0.01, 1, 0.1), Q2 = seq(0.01, 1, 0.1), eta=seq(0.1,1.3, 0.2), gamma=seq(0.5, 3, 0.5))
df <- expand.grid(Q1 = seq(0.01, 1.01, 0.1), Q2 = seq(0.01, 1.01, 0.1), eta=1, gamma=seq(0.1, 5, 0.5), nu=seq(0.1, 2.4, 0.4))

df$presp <- NA
for (i in 1:nrow(df)) {
  df[i, "presp"] <- presp(df[i, "Q1"], df[i, "Q2"], eta=df[i,"eta"], gamma=df[i,"gamma"], nu=df[i,"nu"])
}

df <- df %>% mutate(sumQ=Q1+Q2)
# ggplot(df, aes(x=Q1, y=Q2, fill=presp)) + geom_tile() + facet_wrap(~eta)

#ggplot(df, aes(x=sumQ, y=presp)) + geom_line() + facet_grid(gamma~eta)
ggplot(df, aes(x=sumQ, y=presp)) + geom_line() + facet_grid(gamma~nu, labeller=label_both)

df %>% group_by(eta) %>% summarize(mean(presp))

