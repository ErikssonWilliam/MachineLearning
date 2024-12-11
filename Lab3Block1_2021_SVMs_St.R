# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
tr <- spam[1:3000, ]    #training
va <- spam[3001:3800, ] #validation
trva <- spam[1:3800, ]  #training and validation
te <- spam[3801:4601, ] #test

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,                
                 data=tr,               #x,y training. 
                 kernel="rbfdot",       #Radial Basis Function (Gaussian kernel)
                 kpar=list(sigma=0.05), 
                 C=i,                   #Regularization parameter (for classification problems)
                 scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

filter0 <- ksvm(type~.,
                data=tr,kernel="rbfdot",
                kpar=list(sigma=0.05),
                C=which.min(err_va)*by,
                scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,
                data=tr,kernel="rbfdot",
                kpar=list(sigma=0.05),
                C=which.min(err_va)*by,
                scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,
                data=trva,kernel="rbfdot",
                kpar=list(sigma=0.05),
                C=which.min(err_va)*by,
                scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,
                data=spam,
                kernel="rbfdot",
                kpar=list(sigma=0.05),
                C=which.min(err_va)*by,
                scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?

# 3. Implementation of SVM predictions.

sv = alphaindex(filter3)[[1]] #index (/=0) of support vector
co = coef(filter3)[[1]]       #linear coefficients of support vector
inte = - b(filter3)           #negative intercept of linear combination
k = numeric(10) 
x_sv = spam[sv, -58]

for(i in 1:10){ 
  k2 = 0 
  x_i = spam[i, -58]
  
  for(j in 1:length(sv)){
    k2 = k2 + (co[j] * (exp(-0.05 * sum((x_sv[j,] - x_i)^2))))
  }
   k[i] = k2 + inte
}
k
predict(filter3,spam[1:10,-58], type = "decision")
  
