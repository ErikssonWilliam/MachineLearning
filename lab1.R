#marha614, wiler441, emmst476
#Step 1

opt = read.csv("./optdigits.csv", header = FALSE)

n = dim(opt)[1]

set.seed(123)

id = sample(1:n, floor(n*0.5))

train = opt[id,]

id1=setdiff(1:n, id)

set.seed(123)

id2=sample(id1,floor(n*0.25))

valid =opt[id2,]

id3 = setdiff(id1, id2)

test = opt[id3,]

#step2

library(kknn)

m1=kknn(as.factor(V65)~., train, test, k=30, kernel="rectangular")
Pred=m1$fitted.values
confusion_table_test = table(test$V65, Pred)
confusion_table_test
test_error = (1- sum(diag(confusion_table)) / sum(confusion_table))
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
  X = matrix(as.numeric(test[easiest_cases[i],-c(65)]), nrow=8, ncol =8, byrow = TRUE)
  heatmap(X, Rowv=NA, Colv=NA, col=heat.colors(12), main = paste("Heat map of test case", easiest_cases[i]))
}

for (i in seq_along(hardest_cases)) {
  X = matrix(as.numeric(test[hardest_cases[i],-c(65)]), nrow=8, ncol =8, byrow =TRUE)
  heatmap(X, Rowv=NA, Colv=NA, col=heat.colors(12), main = paste("Heat map of test case", hardest_cases[i]))
}


#Step 4

numberOfK = 30
valid_errors = matrix(numberOfK)

for (i in 1:numberOfK) {
m1=kknn(as.factor(V65)~., train, valid, k=i, kernel="rectangular")
Pred=m1$fitted.values
confusion_table = table(valid$V65, Pred)

valid_errors[i] = (1- sum(diag(confusion_table)) / sum(confusion_table))

}

plot(1:numberOfK, valid_errors, type = "o", col = "blue", xlab = "Value of K", ylab = "Validation Misclassification Error", main = " Validation misclassification Errors for Different K Values")
valid_errors[3]

#Optimal K is the one that gives lowest misclassification error. K*=3
m1=kknn(as.factor(V65)~., train, test, k=3, kernel="rectangular")
Pred=m1$fitted.values
confusion_table = table(test$V65, Pred)
train_error = (1- sum(diag(confusion_table)) / sum(confusion_table))
train_error
valid_errors[3]

