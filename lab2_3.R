rm(list = ls())
com = read.csv("communities.csv", header = TRUE)

#Step 1
library(dplyr)
library(caret)
data = com %>% select(-ViolentCrimesPerPop)
scaled_data = preProcess(data, method = c("scale", "center"))
scaled_data = as.matrix(predict(scaled_data, newdata = data))

S = (1/nrow(scaled_data)) * (t(scaled_data) %*% scaled_data) #covariance matrix denoted as S
which( cumsum(eigen(S)$values/sum(eigen(S)$values)) >= 0.95)[1] #sum of variance should be over 95%
eigen(S)$values[1:2]/sum(eigen(S)$values) #proportion of variance for the first two PCAs

#Step 2
res = princomp(data)
U=res$loadings
plot(U[,1], main="Traceplot, PC1")
top_5_indices = order(abs(U[, 1]), decreasing = TRUE)[1:5]
top_5_values = U[top_5_indices, 1]

pc_scores = res$scores[, 1:2]
pc_scores_with_violent_crimes = cbind(pc_scores, ViolentCrimesPerPop = com$ViolentCrimesPerPop) # Add ViolentCrimesPerPop to the PC scores data
pc_scores_df = as.data.frame(pc_scores_with_violent_crimes)

ggplot(pc_scores_df, aes(x = Comp.1, y = Comp.2, color = ViolentCrimesPerPop)) +
  geom_point() +
  labs(title = "PC Scores (PC1 vs PC2)", x = "PC1", y = "PC2", color = "Violent Crimes Per Pop") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()

#Step 3
n = dim(com)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = com[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1,floor(n*0.5))
test = com[id2,]

#Preprocces train and test to scale the data without target variable
train_data = train %>% select(-ViolentCrimesPerPop)
test_data = test %>% select(-ViolentCrimesPerPop)
scaled_train = preProcess(train_data, method = "scale")
train_scaled = predict(scaled_train, newdata = train_data)
test_scaled = predict(scaled_train, newdata = test_data)

##SHOULD YOU SCALE TARGET VARIABEL SEPERATLY; WITH PREDICTORS; NOT AT ALL
#implemented without scaling for target variable below:
lm_model = lm(train$ViolentCrimesPerPop ~ ., data = data.frame(train_scaled))
train_predictions = predict(lm_model, newdata = data.frame(train_scaled))
test_predictions = predict(lm_model, newdata = data.frame(test_scaled))

mean((train_predictions - train$ViolentCrimesPerPop) ^ 2) #Train mse
mean((test_predictions - test$ViolentCrimesPerPop) ^ 2) #Test mse

#Step 4

cost_function <- function(theta, X, y) {
  n = nrow(X)
  cost = (1 / n) * sum((X %*% theta - y)^2) # I HOPE THIS IS CORRECT?!
  return(cost)
}

theta = rep(0, ncol(train_scaled))

optim = optim(
  par = theta,
  fn = cost_function,
  X = as.matrix(train_scaled),
  y = train$ViolentCrimesPerPop,
  method = "BFGS"
  )
optim$par
