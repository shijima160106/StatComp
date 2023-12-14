## -----------------------------------------------------------------------------
M <- 1000
m <- 1000
set.seed(41)

FWER <- numeric(M)

FDR1 <- numeric(M)
TPR1 <- numeric(M)

FDR2 <- numeric(M)
TPR2 <- numeric(M)

for (i in 1:M) {
  # 生成p值
  p1 <- runif(0.95*m)
  p2 <- rbeta(0.05*m, 0.1, 1)
  p <- c(p1, p2)
  
  
  
  p.adj1 = p.adjust(p, method='bonferroni')
  p.adj2 = p.adjust(p, method='fdr')
  round(rbind(p.adj1, p.adj2), 3)
  
  
  
  
  FP1 <- as.numeric( table(p.adj1[1:as.integer(0.95*m)] < 0.1)[2] )
  TN1 <- as.numeric( table(p.adj1[1:as.integer(0.95*m)] < 0.1)[1] )
  TP1 <- as.numeric( table(p.adj1[as.integer(0.95*m)+1:m] < 0.1)[2] )
  FN1 <- as.numeric( table(p.adj1[as.integer(0.95*m)+1:m] < 0.1)[1] )
  
  
  FDR1[i] <- FP1/(FP1+TP1)
  TPR1[i] <- TP1/(m*0.05)
  
  FP2 <- as.numeric( table(p.adj2[1:as.integer(0.95*m)] < 0.1)[2] )
  TN2 <- as.numeric( table(p.adj2[1:as.integer(0.95*m)] < 0.1)[1] )
  TP2 <- as.numeric( table(p.adj2[as.integer(0.95*m)+1:m] < 0.1)[2] )
  FN2 <- as.numeric( table(p.adj2[as.integer(0.95*m)+1:m] < 0.1)[1] )
  
  
  FDR2[i] <- FP2/(FP2+TP2)
  TPR2[i] <- TP2/(m*0.05)
  
  
  # FWER
  #FWER1[i] <- as.numeric(table(p.adj1[1:as.integer(0.95*m)] < 0.1)[2] > 0) # 拒绝任一真的原假设
  #FWER2[i] <- as.numeric(table(p.adj2[1:as.integer(0.95*m)] < 0.1)[2] > 0)
  FWER[i] <- 1 - (1 - 0.1/m)^m
}
FDR1[is.na(FDR1)] <- 0
mean(FDR1)
mean(TPR1)

FDR2[is.na(FDR2)] <- 0
mean(FDR2)
mean(TPR2)


grade <- data.frame(
  method = c("Bonf", "B-H"),
  FWER = c(mean(FWER), mean(FWER)),
  FDR = c(mean(FDR1), mean(FDR2)),
  TPR = c(mean(TPR1), mean(TPR2))
)

knitr::kable(grade)

## -----------------------------------------------------------------------------
library("boot")
size <- c(5,10,20)
m <- 1000
B <- 1000
lambda <- 2
for (i in 1:3) {
  n <- size[i]
  bias <- numeric(m)
  se <- numeric(m)
  for (j in 1:m) {
    # 生成样本
    set.seed(42)
    x <- rexp(m, lambda)
    lambdahat <- mean(x) # estimate
    lambdastar <- numeric(B)
    
    for (b in 1:B) {
      xstar <- sample(x,replace=TRUE)
      lambdastar[b] <- mean(xstar)
    }
    # mean bootstrap bias
    bias[j] <- mean(lambdastar) - lambdahat
    se[j] <- sd(lambdastar)
    
    
  }
  hist(bias)
  abline(v= lambda*n/(n-1) ,col='red',lwd=2)
  
  hist(se)
  abline(v= lambda*n/(n-1)/sqrt(n-2) , col='red',lwd=2)
  
}




## -----------------------------------------------------------------------------
library(bootstrap) #for the law data
print(cor(law$LSAT, law$GPA))

print(cor(law82$LSAT, law82$GPA))

#set up the bootstrap
B <- 200 #number of replicates
n <- nrow(law) #sample size
R <- numeric(B) #storage for replicates
#bootstrap estimate of standard error of R
for (b in 1:B) {
#randomly select the indices
i <- sample(1:n, size = n, replace = TRUE)
LSAT <- law$LSAT[i] #i is a vector of indices
GPA <- law$GPA[i]
R[b] <- cor(LSAT, GPA)
}
#output
print(se.R <- sd(R))

hist(R, prob = TRUE)

# real data estimate
Rhat <- mean(cor(law$LSAT, law$GPA))
# replicate of the “t” statistic
t.stats <- (R-Rhat)/se.R
alpha <- 0.05
Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
tstar1 <- as.numeric( Qt[1])
tstar2 <- as.numeric( Qt[2])

print(Rhat+tstar1*se.R)
print(Rhat+tstar2*se.R)
# 所求区间

