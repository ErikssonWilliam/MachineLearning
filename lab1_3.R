#marha614, wiler441, emmst476

#Step1
pima = read.csv("pima-indians-diabetes.csv", header = FALSE)
plot(pima[,8], pima[,2], main = "Plasma glucose concentration on age", xlab = "Age", ylab = "Glucose",  col = ifelse(pima[, 9] == 1, "green", "red"),  # Color by diabetes levels
     pch = 19)
