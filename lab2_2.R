#step 1
library(dplyr)
bank = read.csv("./bank-full.csv", header = TRUE, sep = ";")
bank = bank %>% select(-duration)

n = dim(bank)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
train = bank[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1,floor(n*0.3))
valid =bank[id2,] 
id3 = setdiff(id1, id2)
test = bank[id3,]

# step 2
library(tree)
train[] <- lapply(train, function(x) if (is.character(x)) as.factor(x) else x) #make chr columns into factors or tree() fails

tree_default = tree(as.factor(y)~., data = train)
tree_size = tree(as.factor(y)~., data = train, control = tree.control(nobs = nrow(train), minsize = 7000))
tree_deviance = tree(as.factor(y)~., data = train, control = tree.control(nobs = nrow(train), mindev = 0.0005))

summary(tree_default) #used to see missclassification error and terminal nodes 
summary(tree_size)
summary(tree_deviance)

valid[] <- lapply(valid, function(x) if (is.character(x)) as.factor(x) else x)
pred_default_val = predict(tree_default, valid, type = "class")
pred_size_val = predict(tree_size, valid, type = "class")
pred_deviance_val = predict(tree_deviance, valid, type = "class")
mean(pred_default_val != valid$y)
mean(pred_size_val != valid$y)
mean(pred_deviance_val != valid$y)

#step 3
leaves = 2:50
train_deviance = rep(0,50)
validation_deviance = rep(0,50)

for (i in 2:50) {
  pruned = prune.tree(tree_deviance, best = i)
  pred = predict(pruned, valid, type = "tree")
  train_deviance[i] = deviance(pruned)
  validation_deviance[i] = deviance(pred)
}

plot(2:50, train_deviance[2:50], type="b", col="red", ylim = c(8000, 12000))
points(2:50, validation_deviance[2:50], type ="b", col="blue")

best_leaves = which.min(validation_deviance[2:50]) + 1 # number of leaves, outputs 22 
best = prune.tree(tree_deviance, best = best_leaves) 
plot(best)
text(best)
summary(best)
print(best)

#Step 4
test[] = lapply(test, function(x) if (is.character(x)) as.factor(x) else x)
pred = predict(best, test, type = "class")
confusion_matrix = table(test$y, pred)
confusion_matrix
TP = confusion_matrix[2,2]
TN = confusion_matrix[1,1]
FP = confusion_matrix[1,2]
FN = confusion_matrix[2,1]
N = TN + FP
P = FN + TP

(TP + TN)/ (P + N) #Calculates Accuracy

precision = TP / (TP + FP)
recall = TP / P
2 * precision * recall / (precision + recall) # calculates F1

#Step 5

prob = predict(best, test, type = "vector")

# Create the loss matrix

pred = ifelse(prob[,"yes"]/prob[,"no"]>=1/5, "yes", "no")
new_confusion= table(test$y, pred) #something is wrong here
new_confusion

summary(test$y)

TP = new_confusion[2,2]
TN = new_confusion[1,1]
FP = new_confusion[1,2]
FN = new_confusion[2,1]
N = TN + FP
P = FN + TP

(TP + TN)/ (P + N) #Calculates Accuracy

precision = TP / (TP + FP)
precision
recall = TP / P
recall
2 * precision * recall / (precision + recall) # calculates F1

#Step 6,
#optimal_tree

prob = predict(best, newdata = test, type = "vector")


# Create the loss matrix

thresholds = seq(0.05, 0.95, by = 0.05)
TPR_values = numeric(length(thresholds)) 
FPR_values = numeric(length(thresholds))


for (threshold in thresholds) {# Apply threshold to predictions
  
  pred = ifelse(prob[, 2] > threshold, "yes", "no") # Create confusion matrix 
  new_confusion = table(test$y, pred) # Extract confusion matrix values 
  
  transpose = t(new_confusion)
  transpose
  TP <- transpose[4]
  TN <- transpose[1]
  FN = transpose[3]
  FP = transpose[2]
  pos <- transpose[3] + transpose[4]
  neg <- transpose[1] + transpose[2]
  TPR_values <- c(TPR_values, TP / pos) 
  FPR_values <- c(FPR_values, FP / neg)
}
 
  TPR_values
  FPR_values
  
  plot(FPR_values, TPR_values, pch = 5, type = "b")

  # Regression model

  model = glm(y ~ ., data = train, family = "binomial")
  prob = predict(model, newdata = test, type = "response")
  
  
  thresholds = seq(0.05, 0.95, by = 0.05)
  TPR_values = numeric(length(thresholds)) 
  FPR_values = numeric(length(thresholds))
  
  for (threshold in thresholds) {# Apply threshold to predictions
    
    pred = ifelse(prob > threshold, "yes", "no") # Create confusion matrix 
    new_confusion = table(test$y, pred) # Extract confusion matrix values 
    
    transpose = t(new_confusion)
    transpose
    TP <- transpose[4]
    TN <- transpose[1]
    FN = transpose[3]
    FP = transpose[2]
    pos <- transpose[3] + transpose[4]
    neg <- transpose[1] + transpose[2]
    TPR_values <- c(TPR_values, TP / pos) 
    FPR_values <- c(FPR_values, FP / neg)
  }
    TPR_values
    FPR_values
  
  plot(FPR_values, TPR_values, pch = 5, type = "b")
  
  