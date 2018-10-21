# Titanic Tutorial from Kaggle competition

# http://vimeo.com/69269984

#########################################################################

# Set the working directory for the analysis.

setwd("C:/Users/dkane/Documents/R Packages/Titanic Tutorial")

##########################################################################

# Random Forest Model

library(randomForest)

Train = read.csv("train data with estimated age.csv")

attach(Train)

# model with some interactions

set.seed(123)

RandomForestModel = randomForest(Survived ~ Pclass + Fare + SibSp + Parch + 
              Sex + Age, data=Train, ntree=500, mtry=3, importance=TRUE)

# + as.Factor(Title)
# modeling pclass as a factor has no bearing on the overall results.
# embarked reduced the model fit.

summary(RandomForestModel)

#########################################################################

# Run some predictions on the Train dataset from the model.

Train$RFPredict <- predict(RandomForestModel, newdata = Train, type = "response")

Train$RFPredict <- round(Train$RFPredict)

#########################################################################

library(caret)

confusionMatrix(Train$RFPredict, Train$Survived)