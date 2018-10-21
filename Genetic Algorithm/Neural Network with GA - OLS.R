# Genetic Algorithm with Neural Network

############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Neural Network")

############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library("neuralnet")
library(GA)
library(caret)

require(MASS)

mydata <- Boston
attach(mydata)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a Neural Network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



OLS <- function(data, b0){
  
  attach(data, warn.conflicts=F)
  
  Y_hat <- train(medv ~ indus + rm + tax + ptratio + lstat, data = mydata, method = "nnet", 
                 algorithm = 'backprop', learningrate = b0, maxit = 100, hidden = 4, linout = TRUE)
  
  mydata$Prediction <- predict(Y_hat, mydata)
  
  RMSE <- sqrt(mean((mydata$Prediction - mydata$medv)^2))
  
  detach(data)
  
  return(RMSE)
  mydata$Prediction <- NULL
  
}


#### this sets up a real-value GA using 3 parameters all from -100 to 100
#### the parameters use real numbers (so floating decimals) and passes those to
#### the linear regression equation/function
#### the real-value GA requires a min and max
#### this takes a while to run

ga.OLS <- ga(type='real-valued', min=c(0), 
             max=c(1), popSize=100, maxiter=100, names=c('threshold'),
             keepBest=T, fitness = function(b) -OLS(mydata, b[1]))

#### summary of the ga with solution
ga.model <- summary(ga.OLS)
ga.model

#prestige.rmse <- sqrt(mean((prestige.predict - prestige.test$income)^2)) 
#RMSE <- sqrt(mean((mydata$Prediction - mydata$medv)^2))
#RMSE <- sqrt((sum((mydata$Prediction - mydata$medv)^2)) / nrow(mydata))
