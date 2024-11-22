#marha614, wiler441, emmst476


#Step1
pima = read.csv("pima-indians-diabetes.csv", header = FALSE)

glucose_conc = pima[,2]
age = pima[,8]
diabetes = pima[,9]

plot(age, glucose_conc, main = "Plasma glucose concentration on age", 
     xlab = "Age", ylab = "Glucose",  
     col = ifelse(diabetes == 1, "green", "red"), # Color by diabetes levels
     pch = 19)


#Step2

library(dplyr)
train=data.frame(glucose_conc, age, diabetes) 
#Selects the columns glucose concentration, age and diabetes

m1 = glm(diabetes~age+glucose_conc, 
         family = "binomial", 
         data = train) #Train the logistic regression model
summary(m1)

#Probablistic equation see page 25 of lecuture 1 d

coefficients = m1$coefficients
logit = coefficients["(Intercept)"] + coefficients["age"]*train$age + coefficients["glucose_conc"]*train$glucose_conc
prob_eq = 1/(1+ exp(-(logit))) #Logistic regression
head(prob_eq) 

#Prediction
prob = predict(m1, type = "response") #Specify that the prediction returns the predicted probabilities
pred = ifelse(prob > 0.5, "diabetes", "no diabetes")
train_confusion_matrix=table(train$diabetes, pred)
train_confusion_matrix

mc_error = sum(diag(train_confusion_matrix)) / length(pred) 
#The incorrect classifications are in the diagonal of the confusion matrix. Length pred represents the total no of predictions made
mc_error

plot(train$age, train$glucose_conc, 
     main="predicted plasma glucose concentration on age",
     xlab="age", 
     ylab="plasma glucose concentration", 
     col = ifelse(pred == "diabetes", "green", "red"), 
     pch=19)

#Step 3
#See page 23 of lecture 1d
#Decision boundary is a curve that separates different classes (Diabetes or no diabetes)
#For a binary model it would be when P(y=1|X) = 0.5 (Probabilistic equation)
#Decision boundary for logistic regression is linear. (y = kx + m) Need to calculate the k and the m
#Decision boundary is where logit is 0 (prob_eq = 0.5)
#=> glucose_conc = (-beta(0)/beta(2) = m value, -beta(1)/beta(2) = k)
k = coefficients["age"]/-coefficients["glucose_conc"]
m = coefficients["(Intercept)"]/-coefficients["glucose_conc"]

#Equation
sprintf("y = %fx + %f", k, m)

#New Plot
plot(train$age,train$glucose_conc, main="predicted plasma glucose concentration on age",
     xlab="age", 
     ylab="plasma glucose concentration", 
     col = ifelse(pred == "diabetes", "green", "red"), 
     pch=19)
abline(a = m, b = k, col = "blue",  lwd = 2)

#Step 4

#r=0.2
pred2 = ifelse(prob > 0.2, "diabetes", "no diabetes")
plot(train$age, train$glucose_conc, 
     main="predicted plasma glucose concentration on age",
     xlab="age", 
     ylab="plasma glucose concentration", 
     col = ifelse(pred2 == "diabetes", "green", "red"), 
     pch=19)

#r=0.8
pred3 = ifelse(prob > 0.8, "diabetes", "no diabetes")
plot(train$age, train$glucose_conc, 
     main="predicted plasma glucose concentration on age",
     xlab="age", 
     ylab="plasma glucose concentration", 
     col = ifelse(pred3 == "diabetes", "green", "red"), 
     pch=19)

#Step 5
pima$Z1 = pima$V2^4
pima$Z2 = pima$V2^3 * pima$V8
pima$Z3 = pima$V2^2 * pima$V8^2
pima$Z4 = pima$V2 * pima$V8^3
pima$Z5 = pima$V8^4

model = glm(V9 ~ V2 + V8 + Z1 + Z2 + Z3 + Z4 + Z5, data = pima, family = binomial)
pred = predict(model, type = "response")
pred_threshold = ifelse(pred >= 0.5, 1, 0)

misclass_error = mean(pred_threshold != pima$V9)
misclass_error

colors = ifelse(pred_threshold == 1, "red", "blue")
plot(pima[,8], pima[,2],
     col = colors,
     main = "Predicted Diabetes based on Age and Plasma Glucose",
     xlab = "Age",
     ylab = "Plasma Glucose Concentration",
     pch = 19)

legend("topright", legend = c("Predicted No Diabetes", "Predicted Diabetes",), 
       col = c("blue", "red"), pch = c(19, 19), lty = c(NA, NA), lwd = c(NA, NA))

