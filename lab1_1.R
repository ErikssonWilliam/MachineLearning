#marha614, wiler441, emmst476
#Step 1

opt = read.csv("./optdigits.csv", header = FALSE)

n = dim(opt)[1]

set.seed(12345) #Set starting point for random no generator. Set seed everytime before something random. Set seed to 12345.

id = sample(1:n, floor(n*0.5)) #By setting set.seed, sample will produce same set of indices each time

train = opt[id,]

id1=setdiff(1:n, id) #Ensures no overlap between sets

set.seed(12345)

id2=sample(id1,floor(n*0.25))

valid =opt[id2,] #Evaluates how well the model generalizes to unseen data, avoid overfitting

id3 = setdiff(id1, id2)

test = opt[id3,] #Evaluate final performance after training and tuning, should only be used for final assessment to avoid any bias in the results
#Bias, Selection Bias: Data used to train is not representative of the real world. Measurement Bias: If there's inaccuracies in the data
#step2

library(kknn)

m1=kknn(as.factor(V65)~., train, test, k=30, kernel="rectangular") #Factor, data structure that can take on limited number of unique values
#kernel = "rectangular", specifies kernel function as unweighted k-NN. Contibution of each k nearest neighbors is same, regardless of distance form query point
#Neighbors vote on what a class shall be. k = 30, the 30 nearest neighbors vote
Pred=m1$fitted.values
confusion_table_test = table(test$V65, Pred) #TP, TN, FP, FN
confusion_table_test
test_error = (1- sum(diag(confusion_table_test)) / sum(confusion_table_test)) #FP + FN/ Total predictions
test_error
#Most errors (8,1) (1,2) (4,7) (5,9), 5,4% is a reasonable error percentage.

m1=kknn(as.factor(V65)~., train, train, k=30, kernel="rectangular")
Pred=m1$fitted.values
confusion_table_train = table(train$V65, Pred)
confusion_table_train
train_error = (1- sum(diag(confusion_table_train)) / sum(confusion_table_train))
train_error

#step3

prob = m1$prob

index <- which(test$V65 == 8)
digit_8= prob[index, 8]

easiest_cases = index[order(digit_8, decreasing =TRUE)[1:2]]
hardest_cases = index[order(digit_8, decreasing =FALSE)[1:3]]

for (i in seq_along(easiest_cases)) {
  X = matrix(as.numeric(test[easiest_cases[i],-c(65)]), nrow=8, ncol =8, byrow = TRUE) #The matrix will be fill the first row from left to right
  heatmap(X, Rowv=NA, Colv=NA, col=heat.colors(12), main = paste("Heat map of test case", easiest_cases[i]))
}

for (i in seq_along(hardest_cases)) {
  X = matrix(as.numeric(test[hardest_cases[i],-c(65)]), nrow=8, ncol =8, byrow =TRUE)
  heatmap(X, Rowv=NA, Colv=NA, col=heat.colors(12), main = paste("Heat map of test case", hardest_cases[i]))
}


#Step 4


numberOfK = 30
valid_errors = numeric(numberOfK) 
train_errors = numeric(numberOfK) 

for (i in 1:numberOfK) {
  # Validation error
  m1_valid = kknn(as.factor(V65)~., train, valid, k=i, kernel="rectangular")
  Pred_valid = m1_valid$fitted.values
  confusion_table_valid = table(valid$V65, Pred_valid)
  valid_errors[i] = (1 - sum(diag(confusion_table_valid)) / 
                       sum(confusion_table_valid))
  
  # Training error
  m1_train = kknn(as.factor(V65)~., train, train, k=i, kernel="rectangular")
  Pred_train = m1_train$fitted.values
  confusion_table_train = table(train$V65, Pred_train)
  train_errors[i] = (1 - sum(diag(confusion_table_train)) / 
                       sum(confusion_table_train))
}

# Plot both training and validation errors
plot(1:numberOfK, valid_errors, type = "o", col = "blue", 
     xlab = "Value of K", 
     ylab = "Misclassification Error", 
     main = "Training and Validation Misclassification Errors for Different K Values")

lines(1:numberOfK, train_errors, type = "o", col = "red")


legend("topleft", legend = c("Validation Error", "Training Error"), 
       col = c("blue", "red"), lty = 1, pch = 1)

valid_errors[3]
train_errors[3]

#Optimal K is the one that gives lowest misclassification error. K*=3
m1=kknn(as.factor(V65)~., train, test, k=3, kernel="rectangular")
Pred=m1$fitted.values
confusion_table = table(test$V65, Pred)
test_error = (1- sum(diag(confusion_table)) / sum(confusion_table))
test_error


# Step 5

numberOfK = 30
valid_errors = numeric(numberOfK) 
train_errors = numeric(numberOfK) 

cross_entropy <- function(true_labels, pred_prob) {
  epsilon = 1e-15
  true_one_hot = model.matrix(~ true_labels - 1) #Creates k dummy variables (convert categorical data into a numerical format), 
  #rows represents individual data points, columns represent each unique class in true_labels
  pred_probs = pmax(pred_prob, epsilon) #Prevents predicted prob being 0, avoiding undefined log operations
  return(-mean(rowSums(true_one_hot * log(pred_probs)))) # Calculate cross-entropy loss, -mean to get the loss(difference between predicted and actual values)
}


valid$V65<- factor(valid$V65)
levels(valid$V65)
for (i in 1:numberOfK) {
  # Validation error
  m1_valid = kknn(as.factor(V65)~., train, valid, k=i, kernel="rectangular")
  pred_prob = m1_valid$prob
  valid_errors[i] = cross_entropy(valid$V65,pred_prob)
 
}

plot(1:numberOfK, valid_errors, type = "o", col = "blue", 
     xlab = "Value of K", 
     ylab = "Misclassification Error", 
     main = "Validation cross entropy errors for Different K Values")

which.min(valid_errors)
