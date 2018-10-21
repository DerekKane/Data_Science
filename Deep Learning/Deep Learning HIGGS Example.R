###############################################################
# Deep Learning Algorithm - Higgs Boson
###############################################################

# http://archive.ics.uci.edu/ml/datasets/HIGGS#

#################################################################

## Set WD

setwd("C:/Users/Derek/Documents/RPackages/Deep Learning/") 

# Load the H2O - The Open Source In-Memory, 
# Prediction Engine for Big Data Science

# install.packages("h2o")

library(h2o)

## Start a local cluster with 1GB RAM

localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, Xmx = '1g')

#################################################################
# Load the data
#################################################################

## Import HIGGS CSV data as H2O

# Column 1 is signal
# Column 2-22 are low level features
# Column 23-29 are high level features

# traindata<- read.csv("C:/Users/Derek/Documents/RPackages/Deep Learning/HIGGS.csv")
# train_h2o <- h2o.importFile(localH2O, path = "C:/Users/Derek/Documents/RPackages/Deep Learning/HIGGS.csv")

mydata <- read.table("C:/Users/Derek/Documents/RPackages/Deep Learning/HIGGS.csv", header = TRUE, sep=",", nrows = 10000)

## Convert data frame into H2O object 
 
mydata_h2o <- as.h2o(localH2O, mydata, key = 'dat') 


#################################################################
# Develop the deep learning model
#################################################################

model <- h2o.deeplearning(x = 2:23,  # column numbers for low level predictors
                          y = 1,   # column number for label
                          data = mydata_h2o, # train data in H2O format
                          activation = "RectifierWithDropout",
                          hidden = c(50,50,50), # three layers of 50 nodes
                          epochs = 500, # max. no. of epochs - 500
                          l1 = 1e-5, 
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          train_samples_per_iteration = -1,
                          classification_stop = -1) 

model <- h2o.deeplearning(x = 2:23,  # column numbers for predictors
                          y = 1,   # column number for label
                          data = mydata_h2o, # data in H2O format
                          activation = "RectifierWithDropout", # or 'Tanh'
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                          balance_classes = TRUE, 
                          hidden = c(1000,1000,1000), # three layers of 50 nodes
                          epochs = 200) # max. no. of epochs



model@model$confusion             

#################################################################
# Predicting with the Deep Learning Model
#################################################################

## Using the DNN model for predictions
h2o_Yhat <- h2o.predict(model, test_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_Yhat)

# merge the dataframes for prediction
mydata <- cbind(testdata, df_yhat_test)
