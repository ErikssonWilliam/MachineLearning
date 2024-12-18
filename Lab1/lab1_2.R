#marha614, wiler441, emmst476

#Step 1
park = read.csv("./parkinsons.csv", header = TRUE)
n = dim(park)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.6))
train = park[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1,floor(n*0.4))
test = park[id2,]

library(caret)

params=preProcess(train, method = "scale") #scale data, features have mean of 0 and std of 1
trainS=predict(params, train)
testS=predict(params, test)

# Step 2

library(dplyr)
# Regression
df=trainS%>%select(motor_UPDRS, Jitter...:PPE)
m1=lm(motor_UPDRS~.,df)
coef(m1)

#Training MSE

Preds_train=predict(m1, trainS)
train_MSE=mean((df$motor_UPDRS-Preds_train)^2) #Mean Squared Errors
train_MSE

#Test MSE

Preds_test=predict(m1, testS)
test_MSE=mean((testS$motor_UPDRS-Preds_test)^2)
test_MSE

summary(m1)

#Step 3

#Initialize variabels
sigma = summary(m1)$sigma
lambda = 0.1

coefficients <- summary(m1)$coefficients # Find the start and end indices of the row names you are interested in 
start_index <- which(rownames(coefficients) == "Jitter...") 
end_index <- which(rownames(coefficients) == "PPE") # Select the rows in this range and the "Estimate" column 
theta <- coefficients[start_index:end_index, "Estimate"]
params = c(sigma, theta)

#Log likelihood function, measures how well a statistical model fits the observed data
loglik = function(theta, sigma) {
  
  y = as.matrix(trainS$motor_UPDRS) #dependent variable
  X = trainS %>% select(Jitter...:PPE) %>% as.matrix() #independent variables, from Jitter...:PPE
  n=nrow(trainS)
  #y_hat = X %*% theta # unused?
  log_likelihood = -n / 2 * log(2 * pi * sigma^2) - sum ((y-X %*% theta)^2) / (2 * sigma^2)
  return(log_likelihood)
}
loglik(theta, sigma)

#Ridge function, prevent overfitting by adding a penalty to the size of the coefficents
ridge_function = function(params, lambda) {
  sigma = params[1] 
  theta = params[-1] #Extracts all elements, excluding the first
  return(-loglik(theta, sigma) + lambda * sum(theta^2)) 
}
ridge_function(params, lambda)

#Ridge opt function, optimized the parameters of ta ridge regression model
RidgeOpt <- function(params, lambda) { 
  result <- optim( par = params, # Use optim() with BFGS method, directly referencing ridge_function
                   fn = ridge_function, # Directly call ridge_function 
                   method = "BFGS", 
                   lambda = lambda # Pass additional arguments 
  ) 
  # Extract optimized parameters 
  optimized_theta <- result$par[-1]
  optimized_sigma <- result$par[1] 
  # Return the results as a list 
  return(list(theta = optimized_theta, sigma = optimized_sigma, ridge_value = result$value)) 
}
RidgeOpt(params, lambda)

#DF function

DF <- function(X, lambda) { # Degrees of freedom
  XtX <- t(X) %*% X 
  I <- diag(ncol(X)) # Identity matrix of the same size as number of columns in X 
  S_lambda <- X %*% solve(XtX + lambda * I) %*% t(X) #Smoothing matrix. lambda*I is the regularization term
  return(sum(diag(S_lambda))) #Sum of diagnal elements equals degrees of freedom
}

#Step 4
X_train <- trainS %>% select(Jitter...:PPE) %>% as.matrix() 
X_test <- testS %>% select(Jitter...:PPE) %>% as.matrix() 

#lambda = 1
opt1 = RidgeOpt(params, 1)
pred_train <- X_train %*% opt1$theta 
MSE_train1 <- mean((trainS$motor_UPDRS - pred_train)^2)
MSE_train1
pred_test <- X_test %*% opt1$theta 
MSE_test1 <- mean((testS$motor_UPDRS - pred_test)^2)
MSE_test1

#lambda = 100
opt100 = RidgeOpt(params, 100)
pred_train <- X_train %*% opt100$theta 
MSE_train100 <- mean((trainS$motor_UPDRS - pred_train)^2)
MSE_train100
pred_test <- X_test %*% opt100$theta 
MSE_test100 <- mean((testS$motor_UPDRS - pred_test)^2)
MSE_test100

#lambda = 1000
opt1000 = RidgeOpt(params, 1000)
pred_train <- X_train %*% opt1000$theta 
MSE_train1000 <- mean((trainS$motor_UPDRS - pred_train)^2)
MSE_train1000
pred_test <- X_test %*% opt1000$theta 
MSE_test1000 <- mean((testS$motor_UPDRS - pred_test)^2)
MSE_test1000

DF(X_train, 1)
DF(X_test, 1)

DF(X_train, 100)
DF(X_test, 100)

DF(X_train, 1000)
DF(X_test, 1000)
