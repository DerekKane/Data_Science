#################################################################
# Online News Popularity 
#################################################################

setwd("C:/Users/dkane/Documents/R Scripts/Unstructured Analytics/") 

# install.packages("data.table")
# install.packages("bit64")
# install.packages("outliers")

library(caret)
library(ggplot2)
library(randomForest)
library(ROCR)
library(data.table)
library(outliers)


#################################################################
# Load the data
#################################################################

# Import the dataset (using a Big Data style approach) 

require(bit64)

mydata <- fread("C:/Users/dkane/Documents/R Scripts/Unstructured Analytics/OnlineNewsPopularity.csv")

mydata$url <- NULL


# Remove NA's
na.omit(mydata)

summary(mydata)

# Notice that there are outliers in the data. Lets review/remove them first.

boxplot(mydata$shares)
histogram(mydata$shares)


# http://sel-columbia.github.io/formhub.R/demo/RemoveOutliers.html

# Remove the outliers from the analysis
mydata <- subset(mydata,!(mydata$shares > quantile(mydata$shares, probs=c(.01, .99))[2] | mydata$shares < quantile(mydata$shares, probs=c(.01, .91))[1]) ) 


# mydata$shares <- scale(mydata$shares, center = TRUE, scale = TRUE)

mydata$shares <- log(mydata$shares)

# This will convert the shares back to the example.
# mydata$shares <- exp(mydata$shares)

##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]

#################################################################################
# Model - Random Forest Model
#################################################################################

RandomForestModel <- randomForest(shares~., data=trainData, ntree=50, mtry=5, importance=TRUE)

print(RandomForestModel)
RFassessment <- importance(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=490, col="blue")


# Principal Component Analysis

PCA <- princomp(trainData)

summary(PCA)

plot(PCA) # shows a screeplot.
biplot(PCA)


linearmodel <- lm(shares~ PCA$scores[,1] + PCA$scores[,2] + PCA$scores[,3] + PCA$scores[,4] + PCA$scores[,5], data=trainData)

# linearmodel <- lm(shares~ kw_avg_avg + kw_max_avg + self_reference_min_shares + self_reference_avg_sharess, data=trainData)
# linearmodel <- lm(shares~., data=trainData)
summary(linearmodel)

#################################################################################
# Evaluate Model Performance
#################################################################################

# http://stats.stackexchange.com/questions/56302/what-are-good-rmse-values
# http://horicky.blogspot.com/2012/06/predictive-analytics-evaluate-model.html

score <- predict(RandomForestModel, newdata=testData)
actual <- testData$shares


# We need to consider the following when evaluating a models performance using RMSE
# If the Y  variable   ranges from 0 to 1000, an RMSE of 0.7 is small, 
# but if the range goes from 0 to 1, the 0.7 is not that small anymore.

rmse <- (mean((score - actual)^2))^0.5
rmse

# We can compare this RMSE to the training set. If the The RMSE for your training 
# and your test sets should be very similar if you have built 
# a good model. If the RMSE for the test set is much higher 
# than that of the training set, it is likely that you've badly over fit 
# the data, i.e. you've created a model that tests well in sample, 
# but has little predictive value when tested out of sample. 

score2 <- predict(RandomForestModel, newdata=trainData)
actual2 <- trainData$shares

rmse2 <- (mean((score2 - actual2)^2))^0.5
rmse2

summary(mydata$shares)

# These statistics are required to calculate the R2 measure.

mu <- mean(actual)
rse <- mean((score - actual)^2) / mean((mu - actual)^2) 
rse

rsquare <- 1 - rse
rsquare

##########################################################################
# Here is a method to normalize the RMSE
##########################################################################

# RMSE/(max(DV)-min(DV)

NRMSE <- rmse/(max(trainData$shares)-min(trainData$shares))
NRMSE


##########################################################################
# Score the results and return to original data form
##########################################################################

testData$YHat <- exp(predict(RandomForestModel, newdata=testData))
testData$shares <- exp(testData$shares)
