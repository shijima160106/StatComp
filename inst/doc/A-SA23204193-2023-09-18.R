## -----------------------------------------------------------------------------
library("knitr")
library("kableExtra")
library("ggplot2")
library("rmarkdown") 
library("dplyr")


## -----------------------------------------------------------------------------
my.sample <- function(x, size, prob = NULL){
  # x: vector
  # 放回抽样
  l <- length(x)
  
  inversefunc <- function(u){
    return(x[ceiling(u*l)])
  }
  
  u <- runif(size)
  x <- inversefunc(u)
  
  return(x)
}
my.sample(c(1,3,5,7,9,11,13,15,17,19), 50)

## -----------------------------------------------------------------------------
u <- runif(1000)
x <- rep(0, 1000)
Laplace <- function(x){
  y=x
  for (i in 1:length(x)) {
    if(x[i]<0) y[i]=1/2*exp(x[i])
    else y[i]=1/2*exp(-x[i])
  }
  
  return(y)
}
for (i in 1:1000) {
  if(u[i] < 1/2)
    x[i] = log(2*u[i])
  else
    x[i] = -log(2-2*u[i])
}
hist(x, prob = TRUE)
y <- seq(-6, 6, 0.01)
lines(y, Laplace(y))


## -----------------------------------------------------------------------------
my.beta <- function(a, b, n){
  j=k=0
  y <- numeric(n)
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1) 
    if (x^(a-1) * (1-x)^(b-1) > u) {
      k <- k + 1
      y[k] <- x
    }
  }
return(y)
}
y <- my.beta(3,2,1000)
hist(y, prob = T)
z <- seq(0, 1, 0.005)
lines(z,  12* z^(3-1) * (1-z)^(2-1))

## -----------------------------------------------------------------------------
my.epan <- function(n){
  x <- rep(0, n)
  for (i in 1:n) {
    u1 <- runif(1, -1, 1)
    u2 <- runif(1, -1, 1)
    u3 <- runif(1, -1, 1)
    if(abs(u3)>=abs(u2)&&abs(u3)>=abs(u1)) x[i] <- u2
    else x[i] <- u3
  }
  return(x)
}
x <- my.epan(1000)
hist(x, prob = TRUE)

