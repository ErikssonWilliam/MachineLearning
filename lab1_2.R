#marha614, wiler441, emmst476
#Step 1

park = read.csv("./parkinsons.csv", header = TRUE)

n = dim(park)[1]

set.seed(123)

id = sample(1:n, floor(n*0.6))

train = park[id,]

id1=setdiff(1:n, id)

set.seed(123)

id2=sample(id1,floor(n*0.4))

test = park[id2,]

library(caret)

params=preProcess(train, method = "scale")
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
train_MSE=mean((df$motor_UPDRS-Preds_train)^2)
train_MSE

#Test MSE

Preds_test=predict(m1, testS)
test_MSE=mean((testS$motor_UPDRS-Preds_test)^2)
test_MSE

summary(m1)

#Step 3

#Initialize sigma 
sigma = summary(m1)$sigma

#Initialize theta
coefficients <- summary(m1)$coefficients # Find the start and end indices of the row names you are interested in 
start_index <- which(rownames(coefficients) == "Jitter...") 
end_index <- which(rownames(coefficients) == "PPE") # Select the rows in this range and the "Estimate" column 
theta <- coefficients[start_index:end_index, "Estimate"]

#Log likelihood function

loglik = function(model) {
  
  y = as.matrix(trainS$motor_UPDRS) #dependent variable
  X = trainS %>% select(Jitter...:PPE) %>% as.matrix() #independent variables, from Jitter...:PPE
  n=nrow(trainS)
  
  sigma = model$sigma
  
  coefficients <- model$coefficients # Find the start and end indices of the row names you are interested in 
  start_index <- which(rownames(coefficients) == "Jitter...") 
  end_index <- which(rownames(coefficients) == "PPE") # Select the rows in this range and the "Estimate" column 
  theta <- coefficients[start_index:end_index, "Estimate"]
  
  y_hat = X %*% theta
  
  log_likelihood = -n / 2 * log(2 * pi * sigma^2) - sum ((y-X %*% theta)^2) / (2 * sigma^2)
  return(log_likelihood)
}
loglik(summary(m1))

#Ridge function

ridge_function = function(theta, sigma, lambda, X, Y) { 
  return(-log_likelihood(theta, sigma, X, Y) + lambda * sum(theta^2)) 
}

#Ridge opt function

RidgeOpt <- function(X, Y, theta_init, sigma_init, lambda) { 
  # Combine theta and sigma into a single vector for optimization 
  init_params <- c(theta_init, sigma_init) # Use optim() with BFGS method, directly referencing ridge_function 
  result <- optim( par = init_params, fn = ridge_function, # Directly call ridge_function 
                   method = "BFGS", X = X, Y = Y, lambda = lambda # Pass additional arguments 
  ) 
  # Extract optimized parameters 
  optimized_theta <- result$par[-length(result$par)] # All but last value 
  optimized_sigma <- result$par[length(result$par)] # Last value 
  # Return the results as a list 
  return(list(theta = optimized_theta, sigma = optimized_sigma, ridge_value = result$value)) 
}

#DF function

DF <- function(X, lambda) {
  XtX <- t(X) %*% X 
  I <- diag(ncol(X)) # Identity matrix of the same size as number of columns in X 
  S_lambda <- X %*% solve(XtX + lambda * I) %*% t(X)
  return(sum(diag(S_lamba)))
}