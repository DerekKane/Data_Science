###############################################################
# XGBoost for Otto Product Classification 
###############################################################

# Set the Working Directory

setwd("C:/Users/Derek/Documents/R Scripts/Kaggle/Otto/")

# Load the Libraries

# install.packages("DiagrammeR")

require(xgboost)
require(methods)
require(data.table)
require(magrittr)
require(ggplot2)
require(Ckmeans.1d.dp)
require(DiagrammeR)

# Load the dataset through fread on data.table

train <- fread('C:/Users/Derek/Documents/R Scripts/Kaggle/Otto/train.csv', header = T, stringsAsFactors = F)
test <- fread('C:/Users/Derek/Documents/R Scripts/Kaggle/Otto/test.csv', header=TRUE, stringsAsFactors = F)

# Lets explore the dataset

# Train dataset dimensions
dim(train)

# Training content
train[1:6,1:5, with =F]

# Test dataset dimensions
dim(train)

# Test content
test[1:6,1:5, with =F]

# Each column represents a feature measured by an integer. Each row is an Otto product.
# Obviously the first column (ID) doesn't contain any useful information.
# To let the algorithm focus on real stuff, we will delete it.

# Delete ID column in training dataset
train[, id := NULL]

# Delete ID column in testing dataset
test[, id := NULL]

# Check the content of the last column
train[1:6, ncol(train), with  = F]

train$target

# Save the name of the last column
nameLastCol <- names(train)[ncol(train)]

# The classes are provided as character string in the 94th column called target. As you may know, XGBoost doesn't support anything else than numbers. So we will convert classes to integers. Moreover, according to the documentation, it should start at 0.
# For that purpose, we will:
  # extract the target column
  # remove "Class_" from each class name
  # convert to integers
  # remove 1 to the new value

# Convert from classes to numbers.

# This will store the column as a vector which can then be utilized within
# the xgboost training algorithm procedure. This is a required step.

y <- train[, nameLastCol, with = F][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}

# Display the first 5 levels

y[1:5]

# We remove label column from training dataset, 
# otherwise XGBoost would use it to guess the labels!

train[, nameLastCol:=NULL, with = F]


#######################################################################
# data.table is an awesome implementation of data.frame, unfortunately 
# it is not a format supported natively by XGBoost. We need to convert 
# both datasets (training and test) in numeric Matrix format.
#######################################################################

trainMatrix <- train[,lapply(.SD,as.numeric)] %>% as.matrix
testMatrix <- test[,lapply(.SD,as.numeric)] %>% as.matrix


#######################################################################
# Model Prediction
#######################################################################

# Before the learning we will use the cross 
# validation to evaluate the our error rate.

# Basically XGBoost will divide the training data in 
# nfold parts, then XGBoost will retain the first part and use 
# it as the test data. Then it will reintegrate the first part 
# to the training dataset and retain the second part, do a training 
# and so on.

numberOfClasses <- max(y) + 1

# y2 <- max(y)

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
                nfold = cv.nfold, nrounds = cv.nround, missing=NaN)

# Lets now train a real model

nround = 100
bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround, missing=NaN)

#####################################################################
# Model Understanding & Feature Importance
#####################################################################

# So far, we have built a model made of 50 trees.

# To build a tree, the dataset is divided recursively several times. 
# At the end of the process, you get groups of observations (here, these observations
# are properties regarding Otto products).

# Each division operation is called a split.

# Each group at each division level is called a branch and the 
# deepest level is called a leaf.

# In the final model, these leafs are supposed to be as pure as 
# possible for each tree, meaning in our case that each leaf should 
# be made of one class of Otto product only (of course it is not true, 
# but that's what we try to achieve in a minimum of splits).

# Not all splits are equally important. Basically the first split of a tree will 
# have more impact on the purity that, for instance, the deepest split. Intuitively, we understand that the first split 
# makes most of the work, and the following splits focus on smaller parts of the dataset which have been missclassified by the first tree.

# In the same way, in Boosting we try to optimize the 
# missclassification at each round (it is called the loss). 
# So the first tree will do the big work and the following trees will focus on the 
# remaining, on the parts not correctly learned by the previous trees.

# The improvement brought by each split can be measured, it is the gain. 
# Each split is done on one feature only at one value.

# Let's see what the model looks like.

model <- xgb.dump(bst, with.stats = T)
model[1:10]


# Clearly, it is not easy to understand what it means.

# Basically each line represents a branch, there is the tree ID, the feature ID, the point where it splits, and 
# information regarding the next branches (left, right, when the row for this feature is N/A).

# Hopefully, XGBoost offers a better representation: feature importance.
# Feature importance is about averaging the gain of each feature for all split and all trees.

# Then we can use the function xgb.plot.importance.

# Get the feature real names
names <- dimnames(trainMatrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

######################################################################
# Interpretation
######################################################################

# In the feature importance above, we can see the first 10 most important features.

# This function gives a color to each bar. 
# Basically a K-means clustering is applied to group each feature by importance.

# From here you can take several actions. For instance you can remove 
# the less important feature (feature selection process), or go deeper 
# in the interaction between the most important features and labels.

# Or you can just reason about why these features are so important 
# (in Otto challenge we can't go this way because there is not enough information).

######################################################################
# Tree Graph
######################################################################

# Feature importance gives you feature weight information but not interaction between features.

# XGBoost R package have another useful function for that. 
# Note that you need to scroll the screen to right to see these trees due to layout of the rmarkdown.

xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)

######################################################################
# Predict Values in the Test Set
######################################################################

ypred <- predict(bst, trainMatrix)


# prepare for output

predMatrix <- data.frame(matrix(ypred, ncol=9, byrow=TRUE))
# colnames(predMatrix) = classnames
# res<-data.frame(id, predMatrix)
# write.csv(res, 'submission.csv', quote = F, row.names = F)

submission <- data.frame(predMatrix)

write.csv(test, file='C:/Users/Derek/Documents/R Scripts/Kaggle/Otto/results.csv', row.names=F)

# https://www.kaggle.com/jasminelily/otto-group-product-classification-challenge/xgboost-r/code
