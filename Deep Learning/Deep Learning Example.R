#################################################################
# Deep Learning - Big Data Tutorial
#################################################################

# http://blenditbayes.blogspot.co.uk/2014/07/things-to-try-after-user-part-1-deep.html

# Goal:

# 1.How to set up and connect to a local H2O cluster from R.
# 2.How to train a deep neural networks model.
# 3.How to use the model for predictions.
# 4.Out-of-bag performance of non-regularized and regularized models.
# 5.How does the memory usage vary over time.

#################################################################

# Load the H2O - The Open Source In-Memory, 
# Prediction Engine for Big Data Science

# install.packages("h2o")

library(h2o)

## Start a local cluster with 1GB RAM

localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, Xmx = '1g')

#################################################################
# Load the data
#################################################################

## Import Breast Cancer CSV as H2O

mydata<- read.csv("C:/Users/Derek/Documents/RPackages/Deep Learning/breastcancer.csv")
dat_h2o <- h2o.importFile(localH2O, path = "C:/Users/Derek/Documents/RPackages/Deep Learning/breastcancer.csv")

mydata$Class <- as.factor(mydata$Class)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Convert data table into H2O
# dat <- mydata[, -1]  # remove the ID column
# dat_h2o <- as.h2o(localH2O, dat) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#################################################################
# Develop the deep learning model
#################################################################

model <- h2o.deeplearning(x = 1:9,  # column numbers for predictors
                   y = 10,   # column number for label
                   training_frame = dat_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

#################################################################
# Predicting with the Deep Learning Model
#################################################################

## Using the DNN model for predictions
h2o_Yhat <- h2o.predict(model, dat_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_Yhat)

# merge the dataframes for prediction
mydata <- cbind(mydata, df_yhat_test)
