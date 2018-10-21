# Bagging (Bootstrapping Aggregation) tutorial
# http://vikparuchuri.com/blog/build-your-own-bagging-function-in-r/

# Ensemble tutorial
# http://www.r-bloggers.com/an-intro-to-ensemble-learning-in-r/

# This tutorial is designed to give some exposure to ensemble learning.
# An ensemble combines the predictions of multiple modeling techniques into a "super model"
# which is stronger than the individual components.

# This tutorial will focus on creating an ensemble learning method from the following:
# linear regression
#

#---------------------------------------------------------------------------

# This is how I am creating a sample dataset using a bagging technique.
# Notice that X2 and X3 contain distinct nonlinear tendencies

set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))

# This is a multiple linear regression model to see the fit

lm_fit<-lm(y~x1+x2+x3)

# Here is the summary fo the models performance.

summary(lm_fit)

# This section will create the split of the data into training and testing sets.

set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[positions,]
testing<- all_data[-positions,]

# This piece run a predictive model on the testing set and apply the results
# to a training set.

lm_fit<-lm(y~x1+x2+x3,data=training)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Dividing the data into training and testing sets and using a simple linear model
# to make predictions about the testing set yields a root mean squared error of 177.36.

#------------------------------------------------------------------------------------

# This piece will create 5000 simple linear regression models and then average the results
# which we can then compare to the original model example.

# install.packages("foreach")
library(foreach)

length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% 
  {training_positions <- sample(nrow(training), 
  size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
  predict(lm_fit,newdata=testing)}


predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# The average of 5000 linear models produces an error of approx. 177.367.
# This is not much better than running a single linear regression.

#------------------------------------------------------------------------------------

# This is where we will create our first ensemble technique, the random forest.
# We will first test the efficacy of a random forest model before we start to combine models

library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
predictions<-predict(rf_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# This model runs through 500 variations and creates a much lower error value of 135.6399
# This model already can handle the non-linear data more efficiently than the regression model

#------------------------------------------------------------------------------------

# Now we will run the ensemble method with an equal weighting of the two models.
# This is deviding up the dataset into bags like before. 

length_divisor<-6
iterations<-5000

predictions<-foreach(m=1:iterations,.combine=cbind) %do% 
  {training_positions <- sample(nrow(training), 
  size=floor((nrow(training)/length_divisor)))
   train_pos<-1:nrow(training) %in% training_positions
   lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
   predict(lm_fit,newdata=testing)}

lm_predictions<-rowMeans(predictions)

library(randomForest)

rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
rf_predictions<-predict(rf_fit,newdata=testing)

predictions<-(lm_predictions+rf_predictions)/2 # This creates the 50% and 50% weighting

error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# This model shows the error value of 147.8748 which shows the proof of concept.
# Now we have to decide how to proceed. There are 2 main routes which we can take:

# 1. We can combine the predictions of the linear regression and random forest 
#    in different ratios until we get a better overall result.
# 2. We can replace the linear model with the better one.

#------------------------------------------------------------------------------------
# This section will go through option 1.

# Here is the first attempt at changing the ratios 
# with a smaller ratio for the linear regression.


predictions<-(lm_predictions+rf_predictions*9)/10

error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Note that the error is now 135.8281 which is not an improvement
# over the random forest alone.

#------------------------------------------------------------------------------------
# This section will go through option 2.

# We will replace the linear regression model with a support vector machine
# which can be used to pickup non linear tendencies in the data more effectively.

# install.packages("e1071")

library(e1071)

svm_fit<-svm(y~x1+x2+x3,data=training)
svm_predictions<-predict(svm_fit,newdata=testing)
error<-sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error

# This produces a model with an error of 129.8774, the best so far.

# Now we will try using the svm using our bagging function to see if there is an improvement.

length_divisor<-6
iterations<-5000

predictions<-foreach(m=1:iterations,.combine=cbind) %do%
{training_positions <- sample(nrow(training),
                              size=floor((nrow(training)/length_divisor)))
 train_pos<-1:nrow(training) %in% training_positions
 svm_fit<-svm(y~x1+x2+x3,data=training[train_pos,])
 predict(svm_fit,newdata=testing)}

svm2_predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-svm2_predictions)^2))/nrow(testing))
error

# The error after 5000 iterations is equal to 140.9517 which indicates
# that this SVM performs better without using bagging techniques.

# Going forward, we will take the results of the single svm model for the analysis.

#------------------------------------------------------------------------------------
# Lets combine the random forest and SVM models to see the total prediction error rate.

predictions<-(svm_predictions+rf_predictions)/2
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# When we equally combine the models, this gives us an error rate of 128.6069 
# which is far superior than either model alone.

#------------------------------------------------------------------------------------
# Now lets tweak the ratios to emphasis a stronger SVM model and see what we get.

predictions<-(svm_predictions*2+rf_predictions)/3
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Finally, we get a model with an error of 128.1802 which lowered our error even further.
#------------------------------------------------------------------------------------
