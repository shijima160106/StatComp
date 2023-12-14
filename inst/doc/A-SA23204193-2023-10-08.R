## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library("knitr")
library("kableExtra")
library("ggplot2")
library("rmarkdown") 
library("dplyr")


## -----------------------------------------------------------------------------

set.seed(10) 
x <- rexp(10000) + 1

m <- 10000

u <- numeric(m)
u <- x^2 * exp(-(x-1)^2/2) / sqrt(2*pi*exp(1))

mean(u)

## -----------------------------------------------------------------------------
m <- 10000 #number of replicates
k <- 5

theta.hat <- numeric(k)

for (i in 1:k) {
  g <- function(x) {
    exp(-x - log(1+x^2)) * (x > (i-1)/k) * (x < i/k)
  }
  
  u <- runif(m) #f3, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- g(x) / (exp(-x) / (1 - exp(-1)))
  theta.hat[i] <- mean(fg)
}

mean(theta.hat)


## -----------------------------------------------------------------------------
n <- 20
alpha <- 0.05
mu <- 2
set.seed(65)

se_mu <- sqrt(2*mu/n)

m <- 1e5

beta.hat <- numeric(m)
beta.se <- numeric(m)
count=0

for(i in 1:m){
  x <- rchisq(n, mu)
  muhat <- mean(x)
  
  if( mu>=muhat+qt(alpha/2, n-1) * se_mu && mu<=muhat+qt(1-alpha/2, n-1) * se_mu ) count=count+1
}
count/m

## -----------------------------------------------------------------------------
n <- 20
alpha <- 0.05
mu <- 1
set.seed(61)

se_mu <- sqrt(2*mu/n)

m <- 1e5

beta.hat <- numeric(m)
beta.se <- numeric(m)
count=0

for(i in 1:m){
  x <- rchisq(n, mu)
  muhat <- mean(x)
  
  if( mu>=muhat+qt(alpha/2, n-1) * se_mu && mu<=muhat+qt(1-alpha/2, n-1) * se_mu ) count=count+1
}
1-count/m

## -----------------------------------------------------------------------------
n <- 20
alpha <- 0.05
mu <- 1
set.seed(62)

se_mu <- sqrt(1/3/n)

m <- 1e5

beta.hat <- numeric(m)
beta.se <- numeric(m)
count=0

for(i in 1:m){
  x <- runif(n, mu-1, mu+1)
  muhat <- mean(x)
  
  if( mu>=muhat+qt(alpha/2, n-1) * se_mu && mu<=muhat+qt(1-alpha/2, n-1) * se_mu ) count=count+1
}
1-count/m

## -----------------------------------------------------------------------------
n <- 20
alpha <- 0.05
mu <- 1
set.seed(63)

se_mu <- sqrt(1/n)

m <- 1e5

beta.hat <- numeric(m)
beta.se <- numeric(m)
count=0

for(i in 1:m){
  x <- rexp(n, 1/mu)
  muhat <- mean(x)
  
  if( mu>=muhat+qt(alpha/2, n-1) * se_mu && mu<=muhat+qt(1-alpha/2, n-1) * se_mu ) count=count+1
}
1-count/m

