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
which.min(validation_deviance[2:50]) + 1 # number of leaves 

