#################################################################
# Deep Learning - MNIST Tutorial
#################################################################

# Goal:

# 1.How to set up and connect to a local H2O cluster from R.
# 2.How to train a deep neural networks model.
# 3.How to use the model for predictions.
# 4.Out-of-bag performance of non-regularized and regularized models.
# 5.How does the memory usage vary over time.

#################################################################

## Set WD

setwd("C:/Users/Derek/Documents/R Scripts/Deep Learning/") 

# Load the H2O - The Open Source In-Memory, 
# Prediction Engine for Big Data Science

# install.packages("h2o")

#################################################################

library(caret)
library(h2o)

## Start a local cluster with 1GB RAM

localH2O = h2o.init(ip = "localhost", port = 54321, 
                    startH2O = TRUE, max_mem_size = '1g')

#################################################################
# Load the data
#################################################################

## Import MNIST CSV data as H2O

traindata<- read.csv("C:/Users/Derek/Documents/R Scripts/Deep Learning/mnist_train.csv")
testdata<- read.csv("C:/Users/Derek/Documents/R Scripts/Deep Learning/mnist_test.csv")

# Convert categorical variable to factor

testdata$X5 <- as.factor(testdata$X5)
traindata$X5 <- as.factor(traindata$X5)
testdata$X7 <- as.factor(testdata$X7)

#################################################################
#  Convert data table into H2O Format
#################################################################

train_h2o <- as.h2o(traindata)
test_h2o <- as.h2o(testdata)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# train_h2o <- h2o.importFile(localH2O, path = "C:/Users/Derek/Documents/R Scripts/Deep Learning/mnist_train.csv")
# test_h2o <- h2o.importFile(localH2O, path = "C:/Users/Derek/Documents/R Scripts/Deep Learning/mnist_test.csv")


#################################################################
# Develop the Deep Learning Model
#################################################################


model <- h2o.deeplearning(x = 2:785,  # column numbers for predictors
                          y = 1,   # column number for dependent variable
                          training_frame = train_h2o, # train data in H2O format
                          validation_frame = test_h2o, # test data in H20 Format
                          activation = "TanhWithDropout", # or 'Tanh'
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                          balance_classes = TRUE, 
                          hidden = c(50,50,50), # three layers of 50 nodes
                          epochs = 5) # max. no. of epochs



# examine the performance of the model.
model

perf <- h2o.performance(model, train_h2o)
# h2o.accuracy(perf)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


mydata <- cbind(traindata, df_yhat_test)


write.csv(mydata, file='C:/Users/Derek/Documents/RPackages/Deep Learning/test1.csv', row.names=F)
write.csv(df_yhat_test, file='C:/Users/Derek/Documents/RPackages/Deep Learning/test2.csv', row.names=F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test Section 

model <- h2o.deeplearning(x = 2:785,  # column numbers for predictors
                          y = 1,   # column number for label
                          training_frame = train_h2o, # train data in H2O format
                          validation_frame = test_h2o, # test data in H20 Format
                          activation = "RectifierWithDropout",
                          hidden = c(50,50,50), # three layers of 1024,1024,2048 nodes
                          epochs = 4, # max. no. of epochs - 8000
                          l1 = 1e-5, 
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          train_samples_per_iteration = -1,
                          classification_stop = -1) 

# http://blenditbayes.blogspot.co.uk/2014/07/things-to-try-after-user-part-1-deep.html
# http://discuss.analyticsvidhya.com/t/script-in-h2o-in-r-to-get-you-into-top-30-percentile-for-the-digit-recognizer-competition/6651?source_topic_id=6634

