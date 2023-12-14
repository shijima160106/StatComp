## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library("knitr")
library("kableExtra")
library("ggplot2")
library("rmarkdown") 
library("dplyr")


## -----------------------------------------------------------------------------
library(ggplot2)
head(mpg, 6)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

## -----------------------------------------------------------------------------
    n = 50
    set.seed(0)
    x = runif(n, min=-2, max=2)
    y = x^3 + rnorm(n)

## -----------------------------------------------------------------------------
x_t <- x
y_t <- y
for (i1 in 2:n) {
  for (j1 in n : i1) {
    if(x_t[j1] < x_t[j1-1]) {
      tempx <- x_t[j1]  
      tempy <- y_t[j1]
      x_t[j1] <- x_t[j1-1]  
      y_t[j1] <- y_t[j1-1]
      x_t[j1-1] <- tempx  
      y_t[j1-1] <- tempy
    }
  }
}
#冒泡排序
plot(x_t, y_t, type = 'l', xlab = expression(x), ylab = expression(y))
plot(x, y, type = 'p', cex=2, col = 'red')
title(main = expression(y==x^3))



