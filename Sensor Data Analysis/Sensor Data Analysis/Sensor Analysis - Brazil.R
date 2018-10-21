###################################################################
# Sensor Analysis Tutorial
###################################################################

# Set working Directory 

setwd("C:/Users/dkane/Documents/R Scripts/Sensor Data Analysis/") 

# Load Libraries

library(caret)
library(rpart)
library(C50)
library(rattle)
library(party)
library(partykit)
library(randomForest)
library(ROCR)
library(ggplot2)
library(reshape2)
library(car)
library(corrplot)
library(e1071)



# Load the data

mydata <- read.csv("SensorData.csv")

mydata$z4 <- as.integer(mydata$z4)

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]




#################################################################################
# Model 3. - Random Forest Model
#################################################################################

RandomForestModel <- randomForest(class ~., data=trainData, ntree=10, mtry=5, importance=TRUE)

print(RandomForestModel)
importance(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

plot.new()
varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")


#################################################################################
# Evaluate the primary models performance
#################################################################################

# this is the function which creates the prediction onto the new dataset.

RFResult <- predict(RandomForestModel, testData, type="response")


# This will create the results as a new column on the dataset.


testData$YHat3 <- predict(RandomForestModel, testData, type="response")

# These are threshold parameter setting controls.

Predict3 <- function(t) ifelse(RFResult > t , 1,0) #t is the threshold for which the confusion


confusionMatrix(testData$YHat3, testData$class) # Random Forest
