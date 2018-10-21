###################################################################
# Churn Analysis Tutorial - CARET Package & One Hot Encoding
###################################################################

# Set working Directory 

setwd("C:/Users/derek/Documents/R Scripts/Caret Package & Scoring/") 

# install.packages("caret")

# Load Libraries

library(caret)
library(rpart)
library(C50)
library(party)
library(partykit)
library(randomForest)
library(ROCR)
library(ggplot2)
library(reshape2)
library(car)

# Load the data

mydata <- read.csv("Churn.csv")

##################################################################
# Data Munging
##################################################################

# Reencode variables to get into numeric form.

mydata$Churn <- as.integer(mydata$Churn)
mydata$InternationalPlan <- as.integer(mydata$InternationalPlan)
mydata$VoiceMail <- as.integer(mydata$VoiceMail)


mydata$Churn[mydata$Churn=="1"] <- 0
mydata$Churn[mydata$Churn=="2"] <- 1

mydata$InternationalPlan[mydata$InternationalPlan=="1"] <- 0
mydata$InternationalPlan[mydata$InternationalPlan=="2"] <- 1

mydata$VoiceMail[mydata$VoiceMail=="1"] <- 0
mydata$VoiceMail[mydata$VoiceMail=="2"] <- 1


mydata$Churn <- as.factor(mydata$Churn)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop unneeded variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# mydata$State <- NULL
mydata$AreeaCode <- NULL
mydata$Phone.Number <- NULL


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handling of missing values.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This will remove any observations that are missing from the dataset (NULL)  

na.omit(mydata)



##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]

#################################################################################
# Model 1. - Artifical Neural Netowrk
#################################################################################

# Classification

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5)

my.grid <- expand.grid(.decay = c(0, 0.001, 0.01), .size = c(10, 15, 20))


model <- train(Churn~., data = trainData, 
               method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = FALSE,
               linout = FALSE)



testData$Predict <- predict(model, newdata = testData, type = "raw")
confusionMatrix(testData$Churn, testData$Predict)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Regression

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated 5 times
  repeats = 5)

my.grid <- expand.grid(.decay = c(0, 0.001, 0.01), .size = c(10, 15, 20))


model <- train(TotNightMin~., data = trainData, 
               method = "nnet", maxit = 1000, tuneGrid = my.grid, rControl = fitControl, trace = FALSE,
               linout = TRUE)

# model <- nnet(TotNightMin~., data = trainData, size=10, decay = 0.01, linout=TRUE, maxit =1000, trace=FALSE)

testData$Predict2 <- predict(model, newdata = testData)

# Error Metrics

testData.rmse <- sqrt(mean((testData$Predict2 - testData$TotNightMin)^2)) 

mape <- function(y, yhat)
  mean(abs((y - yhat)/y))

mape(testData$TotNightMin, testData$Predict2)



# You need to use the option linout = TRUE for the nnet function:
# If you do not, a sigmoidal activation function is used and all of the predictions will be constrained to be on [0, 1]


prestige.fit <- train(income ~ prestige + education, data = prestige.train,
                      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)  


model <- train(Churn~., data = trainData, 
               method = "nnet", maxit = 10000, tuneGrid = my.grid, rControl = fitControl,
               linout = TRUE)


#################################################################################
# Model 2. - Artifical Neural Netowrk
#################################################################################


fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

my.grid <- expand.grid(.decay = c(0, 0.001, 0.01), .size = c(10, 15, 20))


model <- train(Churn~., data = trainData, 
               method = "nnet", maxit = 100, tuneGrid = my.grid, trace = FALSE,
               linout = FALSE)



testData$Predict <- predict(model, newdata = testData, type = "raw")
confusionMatrix(testData$Churn, testData$Predict)






RandomForestModel <- randomForest(Churn ~., data=trainData, ntree=500, mtry=5, importance=TRUE)

print(RandomForestModel)
importance(RandomForestModel)


plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

plot.new()
varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

###################################################################
# Knowledge Discovery: Build a decision tree using C5.0 for Churn
###################################################################

# The decision variable class must be converted into a factor
# variable in order for the C50 to process correctly.

mydata$Churn <- as.factor(mydata$Churn)

# Run the c50 algorithm for a decision tree.

c50_tree_result<-C5.0(Churn~.,data=mydata)

# display the summary

summary(c50_tree_result)

C5imp(c50_tree_result,metric='usage')
C5imp(c50_tree_result,metric='splits')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the c50 algorithm and show the decision rules.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c50_rule_result<-C5.0(Churn~.,data=mydata, rules=TRUE)

# display the summary

summary(c50_rule_result)

# Rule 7, Rule 9, & Rule 13 

#################################################################################
# Evaluate the primary models performance
#################################################################################

# this is the function which creates the prediction onto the new dataset.

LogisticModel <- predict(mylogit, testData, type="response")
SVMResult <- predict(SVMModel, testData, type="response")
RFResult <- predict(RandomForestModel, testData, type="response")

# This will create the results as a new column on the dataset.

testData$YHat1 <- predict(mylogit, testData, type="response")
testData$YHat2 <- predict(SVMModel, testData, type="response")
testData$YHat3 <- predict(RandomForestModel, testData, type="response")

# These are threshold parameter setting controls.

Predict <- function(t) ifelse(LogisticModel > t , 1,0) #t is the threshold for which the confusion
Predict2 <- function(t) ifelse(SVMResult > t , 1,0) #t is the threshold for which the confusion
Predict3 <- function(t) ifelse(RFResult > t , 1,0) #t is the threshold for which the confusion


confusionMatrix(Predict(0.5), testData$Churn) # Logistic Regression
confusionMatrix(Predict2(0.5), testData$Churn) # SVM
confusionMatrix(Predict3(0.5), testData$Churn) # Random Forest


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for unpruned model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pred1 <- prediction(testData$YHat1, testData$Churn)
pred2 <- prediction(testData$YHat2, testData$Churn)
pred3 <- prediction(testData$YHat3, testData$Churn)

perf <- performance(pred1, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")
perf3 <- performance(pred3, "tpr", "fpr")

plot.new()
plot(perf, col = "green", lwd = 2.5)
plot(perf2, add = TRUE, col = "blue", lwd = 2.5)
plot(perf3, add = TRUE, col = "orange", lwd = 2.5)
abline(0,1,col="Red", lwd=2.5, lty = 2)

title('ROC Curve')
legend(0.8,0.4,c("Logistic","SVM","RF"),        
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1.4,1.4,1.4),col=c("green","blue", "orange", "yellow")) # gives the legend lines the correct color and width

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# AUC calculation metrics

fit.auc1 <- performance(pred1, "auc")
fit.auc2 <- performance(pred2, "auc")
fit.auc3 <- performance(pred3, "auc")


fit.auc1 # Logistic regression - AUC 0.69
fit.auc2 # SVM - AUC 0.93
fit.auc3 # Random Forest - AUC 0.94

#################################################################################
# Save the model to a file
#################################################################################

save(RandomForestModel, file = "ChurnModel.rda")
