
install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)


Var <- runif(500, 0, 10) #Sample 500 points, generates the dataset
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # 25 points to training
te <- mydata[26:500,] # Rest of points to test

# Random initialization of the weights in the interval [-1, 1]
winit <- runif(10, -1, 1) #Initializes 10 points

#Task 1

#Neuralnet with one hidden layer with 10 hidden units
nn <- neuralnet(Sin ~., data = tr, hidden = 10, startweights = winit, act.fct = "logistic")


# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#Task 2, Customized activation functions

#Linear function (h1(x) = x)

linear_act = function(x) {x}

nn_linear <- neuralnet(Sin ~., data = tr, hidden = 10, startweights = winit, act.fct = linear_act)


# Plot of the training data (black), test data (blue), and predictions for linear model (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_linear,te), col="red", cex=1)

#ReLu function (max(0, x))

relu_act <- function(x) ifelse(x <= 0, 0, x) #Mimic the function since pmax function is not supported

nn_relu <- neuralnet(Sin ~., data = tr, hidden = 10, startweights = winit, act.fct = relu_act)

# Plot of the training data (black), test data (blue), and predictions for linear model (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_relu,te), col="red", cex=1)

#Softplus function ln(1 + exp(x))

softplus_act <- function(x) { log(1 + exp(x)) }

nn_softplus <- neuralnet(Sin ~., data = tr, hidden = 10, startweights = winit, act.fct = softplus_act)

# Plot of the training data (black), test data (blue), and predictions for linear model (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_softplus,te), col="red", cex=1)

#Task 3, 500 new data points

#Sample 500 new points uniformly at random in the interval [0, 50] 
Var_new <- runif(500, 0, 50) 
new_data <- data.frame(Var = Var_new, Sin = sin(Var_new))

summary(new_data)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2, xlim =c(0,50), ylim=c(-7,1)) 
points(te, col = "blue", cex=1)
#new_data[,1] extracts var values.
points(new_data[,1],predict(nn,new_data), col="red", cex=1)

#Task 4
#When Var is larger than 10 the values converge towards -6
nn$weights

#Task 5

Var_new <- runif(500, 0, 10) 
new_data <- data.frame(Var = Var_new, Sin = sin(Var_new))

tr <- mydata[1:500,] #Train all variables

nn <- neuralnet(Sin ~., data = tr, hidden = 10, threshold = 0.1, startweights = winit, act.fct = "logistic")

plot(tr$Sin, tr$Var, cex=2, xlab="Sin", ylab="Var") #Make predictions x from sin(x)
points(tr$Sin, predict(nn,tr), col="red", cex=1)

