#Assignment 1

tecator = read.csv("./tecator.csv", header = TRUE)
n = dim(tecator)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = tecator[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1,floor(n*0.5))
test = tecator[id2,]

#Step 1

library(dplyr)
# Regression
df=train%>%select(Channel1:Fat)
m1=lm(Fat~.,df)
summary(m1)

#Training error

Pred=m1$fitted.values
confusion_table_train = table(train$Fat, Pred)
train$Fat
Pred
train_error = (1- sum(diag(confusion_table_train)) / sum(confusion_table_train))
train_error

#Testing error

Pred=m1$fitted.values
confusion_table_test = table(test$Fat, Pred) #TP, TN, FP, FN
test_error = (1- sum(diag(confusion_table_test)) / sum(confusion_table_test)) #FP + FN/ Total predictions
test_error

#Step 2

#See report

#Step 3

library(glmnet)

x_train = as.matrix(train %>% select(starts_with("Channel")))
y_train = train$Fat

lasso = glmnet(x_train, y_train, family = "gaussian", #Gaussian, implies that fat is a continous variables
               alpha = 1) #alpha = 1, lasso.


plot(lasso, xvar = "lambda", label = TRUE)

lambda_seq = lasso$lambda

nonzero_coefs = sapply(lambda_seq, function(l) sum(coef(lasso, s = l) != 0)) 
#sapply applies a function over a vector
#Function(l) specifies that the function is specified within another function
#sum(coeff (lasso, s=l) != 0), countes the number of values indiciting that the coefficient in non zeros 

lambda_three_features = lambda_seq[which(nonzero_coefs == 4)][1]
# Find the lambda that corresponds to exactly three non-zero coefficients, == 4[1] to include the intercept term
lambda_three_features
log(lambda_three_features)

#Step 4

x_train = as.matrix(train %>% select(starts_with("Channel")))
y_train = train$Fat

ridge = glmnet(x_train, y_train, family = "gaussian", #Gaussian, implies that fat is a continous variables
               alpha = 0) #alpha = 0, lasso.

plot(ridge, xvar = "lambda", label = TRUE) 

#Step 5

x_train = as.matrix(train %>% select(starts_with("Channel")))
y_train = train$Fat

lasso_cv = cv.glmnet(x_train, y_train, family = "gaussian", #Gaussian, implies that fat is a continous variables
               alpha = 1)
plot(lasso_cv)

log(lasso_cv$lambda.min) #Optimal lambda
#Which variables are included

num_var = sum(coef(lasso_cv, s="lambda.min") != 0) 
num_var #Exclude intercept term so answer is 8

pred_test = predict(lasso_cv, s = "lambda.min", newx = as.matrix(test %>% select(starts_with("Channel"))))
plot(test$Fat, pred_test, xlab = "Original Test Values", ylab = "Predicted Test Values", main = "Original vs. Predicted Test Values", col = "blue", pch = 19)
abline(0, 1, col = "red", lwd = 2)
