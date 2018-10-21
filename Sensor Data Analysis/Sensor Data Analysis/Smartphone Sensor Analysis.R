#################################################################
# Smartphone Sensor Analytics
#################################################################

# Goal:

# Create a prediction for various classification using supervised 
# machine learning.


# 1.How to set up and connect to a local H2O cluster from R.
# 2.How to train a deep neural networks model.
# 3.How to use the model for predictions.

#################################################################
# Set WD

setwd("C:/Users/dkane/Documents/R Scripts/Sensor Data Analysis/") 


# install.packages("h2o")

library(caret)
library(h2o)



#################################################################
# Load the data
#################################################################

## Import Sensor CSV data

traindata<- read.csv("C:/Users/dkane/Documents/R Scripts/Sensor Data Analysis/sensor_train.csv")
testdata<- read.csv("C:/Users/dkane/Documents/R Scripts/Sensor Data Analysis/sensor_test.csv")

traindata$Class <- as.factor(traindata$Class)
testdata$Class <- as.factor(testdata$Class)


# Convert categorical variable to factor

# traindata$X5 <- as.factor(traindata$X5)
# testdata$X7 <- as.factor(testdata$X7)



#################################################################################
# Model  - Artifical Neural Network
#################################################################################


SensorNet <- avNNet(Class~ tGravityAcc.mean...X + tGravityAcc.max...Y + tGravityAcc.energy...X + tGravityAcc.min...X + angle.X.gravityMean. + 
                     tGravityAcc.mean...Y + tGravityAcc.max...X + tGravityAcc.energy...Y + tGravityAcc.min...Y + angle.Y.gravityMean. + angle.Z.gravityMean., 
                   data=traindata, repeats = 25, size=5, decay=0.3, linout=TRUE)


#################################################################################
# Evaluate the primary models performance
#################################################################################

# this is the function which creates the prediction onto the new dataset.

NNResult <- predict(SensorNet, testdata, type="class")

# This will create the results as a new column on the dataset.

testdata$YHat <- predict(SensorNet, testdata, type="class")

confusionMatrix(testdata$YHat, testdata$Class) # ANN



#################################################################
## Deep Learning H2o - Start a local cluster with 1GB RAM
#################################################################

# Load the H2O - The Open Source In-Memory, 
# Prediction Engine for Big Data Science



localH2O = h2o.init(ip = "localhost", port = 54321, 
                    startH2O = TRUE, max_mem_size = '1g')


#################################################################
#  Convert data table into H2O Format
#################################################################

train_h2o <- as.h2o(localH2O, traindata)
test_h2o <- as.h2o(localH2O, testdata)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# train_h2o <- h2o.importFile(localH2O, path = "C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_train.txt")
# test_h2o <- h2o.importFile(localH2O, path = "C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_test.txt")



#################################################################
# Develop the Deep Learning Model
#################################################################


set.seed(123456)

model <- h2o.deeplearning(x = 1:561,  # column numbers for predictors
                          y = 562,   # column number for dependent variable
                          training_frame = train_h2o, # train data in H2O format
                          validation_frame = test_h2o, # test data in H20 Format
                          activation = "TanhWithDropout", # or 'Tanh'
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                          balance_classes = TRUE, 
                          hidden = c(50,50,50), # three layers of 50 nodes
                          epochs = 10) # max. no. of epochs


# examine the performance of the model.
model


#################################################################
# Predicting with the Deep Learning Model
#################################################################

## Using the DNN model for predictions
h2o_Yhat <- h2o.predict(model, test_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_Yhat)

# merge the dataframes for prediction
mydata <- cbind(testdata, df_yhat_test)


confusionMatrix(mydata$predict, mydata$Class) # Deep Learning 

