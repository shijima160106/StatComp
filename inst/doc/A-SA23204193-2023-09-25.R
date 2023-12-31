## -----------------------------------------------------------------------------
K = 100
pihat <- rep(0, K)
for (i in 1:K) {
  set.seed(i)
l <- 0.5
d <- 1
n <- 1e6
X <- runif(n,0,d/2)
Y <- runif(n,0,pi/2)
pihat[i] <- 2*l/d/mean(l/2*sin(Y)>X)
}
var(pihat)


## -----------------------------------------------------------------------------
pihat <- rep(0, K)
for (i in 1:K) {
  set.seed(i+100)
l <- 0.8
d <- 1
n <- 1e6
X <- runif(n,0,d/2)
Y <- runif(n,0,pi/2)
pihat[i] <- 2*l/d/mean(l/2*sin(Y)>X)
}
var(pihat)

## -----------------------------------------------------------------------------
pihat <- rep(0, K)
for (i in 1:K) {
  set.seed(i+200)
l <- 1
d <- 1
n <- 1e6
X <- runif(n,0,d/2)
Y <- runif(n,0,pi/2)
pihat[i] <- 2*l/d/mean(l/2*sin(Y)>X)
}
var(pihat)

## -----------------------------------------------------------------------------
(-3*exp(2)+10*exp(1)-5)/2

## -----------------------------------------------------------------------------
(0.2420-0.0078)/0.2420

## -----------------------------------------------------------------------------
m <- 100000
K <- 100
est1 <- rep(0,K)
est2 <- rep(0,K)

set.seed(3)
for (i in 1:K) {
  U <- runif(m)
  T1 <- exp(U) #simple MC
  T2 <- (exp(U) + exp(1-U))/2
  est1[i] <- mean(T1)
  est2[i] <- mean(T2)
}
var(est1)
var(est2)
1-var(est2)/var(est1)

