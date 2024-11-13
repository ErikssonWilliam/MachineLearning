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
confusion_table = table(test$V65, Pred)
confusion_table
train_error = (1- sum(diag(confusion_table)) / sum(confusion_table))
train_error
#Most errors (8,1) (1,2) (4,7) (5,9), 5,4% is a reasonable error percentage.

#step3

prob = m1$prob

index <- which(test$V65 == 8)
digit_8= prob[index, 8]

easiest_cases = index[order(digit_8, decreasing =TRUE)[1:2]]
hardest_cases = index[order(digit_8, decreasing =FALSE)[1:3]]

for (i in 1:2) {
X=as.matrix(test[easiest_cases[i],-c(65)])
heatmap(X, Rowv=NA, Colv=NA, col=heat.colors(12))
}
#Continue here
