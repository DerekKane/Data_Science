#################################################################
# Deep Learning - Sensor Analytics
#################################################################

# Goal:

# 1.How to set up and connect to a local H2O cluster from R.
# 2.How to train a deep neural networks model.
# 3.How to use the model for predictions.
# 4.Out-of-bag performance of non-regularized and regularized models.
# 5.How does the memory usage vary over time.

#################################################################

## Set WD

setwd("C:/Users/Derek/Documents/RPackages/Sensor Data/") 

# Load the H2O - The Open Source In-Memory, 
# Prediction Engine for Big Data Science

# install.packages("bitops")

library(caret)
library(h2o)



#################################################################


## Start a local cluster with 1GB RAM

localH2O = h2o.init(ip = "localhost", port = 54321, 
                    startH2O = TRUE, max_mem_size = '1g')

#################################################################
# Load the data
#################################################################

## Import MNIST CSV data as H2O

# traindata<- read.table("C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_train.txt")
# testdata<- read.table("C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_test.txt")

traindata<- read.csv("C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_train.csv")
testdata<- read.csv("C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_test.csv")

traindata$Class <- as.factor(traindata$Class)

# Convert categorical variable to factor

# traindata$X5 <- as.factor(traindata$X5)
# testdata$X7 <- as.factor(testdata$X7)

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

perf <- h2o.performance(model, train_h2o)
h2o.accuracy(perf)

# view the specified parameters of the deep learning model.
model@model$params

# View the variable importance of the model.
model@model$varimp

# Show a confusion matrix of the results
model@model$confusion 

# view the short summary of the deep learning model.
model@model$params

# view the short summary of the deep learning model.
model@model$params



#################################################################
# Predicting with the Deep Learning Model
#################################################################

## Using the DNN model for predictions
h2o_Yhat <- h2o.predict(model, test_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_Yhat)

# merge the dataframes for prediction
mydata <- cbind(testdata, df_yhat_test)


confusionMatrix(mydata$predict, mydata$Class) # Random Forest

# write.csv(mydata, file='C:/Users/Derek/Documents/RPackages/Deep Learning/test1.csv', row.names=F)
#write.csv(df_yhat_test, file='C:/Users/Derek/Documents/RPackages/Deep Learning/test2.csv', row.names=F)




# Test Section 
model <- h2o.deeplearning(x = 1:561,  # column numbers for predictors
                          y = 562,   # column number for label
                          training_frame = train_h2o, # train data in H2O format
                          validation_frame = test_h2o, # test data in H20 Format
                          activation = "RectifierWithDropout",
                          hidden = c(50,50,50), # three layers of 1024,1024,2048 nodes
                          epochs = 12, # max. no. of epochs - 8000
                          l1 = 1e-5, 
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          train_samples_per_iteration = -1,
                          classification_stop = -1) 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(randomForest)
library(caret)

traindata<- read.csv("C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_train.csv")
testdata<- read.csv("C:/Users/Derek/Documents/RPackages/Sensor Data/sensor_test.csv")

traindata$Class <- as.factor(traindata$Class)


RandomForestModel <- randomForest(Class ~., data=traindata, ntree=100, mtry=5, importance=TRUE)

print(RandomForestModel)
importance(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel, n.var=15)

plot.new()
varImpPlot(RandomForestModel, type=1, n.var= 15, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

plot.new()
varImpPlot(RandomForestModel, type=2, n.var= 15, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

#################################################################################
# Evaluate the primary models performance
#################################################################################

# this is the function which creates the prediction onto the new dataset.

RFResult <- predict(RandomForestModel, testdata, type="response")


# This will create the results as a new column on the dataset.

testdata$YHat <- predict(RandomForestModel, testdata, type="response")

# These are threshold parameter setting controls.

confusionMatrix(testdata$YHat, testdata$Class) # Random Forest
