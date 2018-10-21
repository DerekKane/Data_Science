########################################################################
# Classification Tree Example
########################################################################

# https://www.youtube.com/watch?v=GOJN9SKl_OE

library(ISLR)
library(tree)
library(caret)

attach(Carseats)

Carseats

########################################################################

# Start the data manipulation

range(Sales) # Sales range from 0 to 16

# create a categorical variable based on Sales

High = ifelse(Sales >= 8, "Yes", "No")

length(High)

# Appends High to Carseat dataset, and now our dataset is ready.

Carseats = data.frame(Carseats, High)

# Lets remove the "Sales" variable

Carseats = Carseats[,-1]

########################################################################

### Split the data into training and testing. Code is from caret package.

set.seed(2)
inTrain <- createDataPartition(y = Carseats$High, p = .75,list = FALSE)

# this then creates the actual train and test dataset

training <- Carseats[ inTrain,]
testing <- Carseats[-inTrain,]


########################################################################

# Make a decision tree model.

tree_model<- tree(High~., training)

# show the Decision Tree

plot(tree_model)

# Add the descriptions to the tree

text(tree_model, pretty = 0)

########################################################################

# Determine how the model is performing

tree_pred = predict(tree_model, testing, type="class")

# This function will calculate the error classification rate. This is taking the difference
# between the tree_pred and the testing$High variable

mean(tree_pred != testing$High)

# Create a confusion matrix of the results. Note the difference betwen accuracy
# and the error rate.

confusionMatrix(tree_pred, testing$High)

########################################################################

# This section will help to determine where to prune the decision tree
# in order to maximize performance.

# The first step is to perform a cross validation to find the optimal cutoff
# the FUN command specifies that we want to prune the misclassification error.

set.seed(3)

cv_tree = cv.tree(tree_model, FUN=prune.misclass)
names(cv_tree)

# this will plot the tree size versus the deviance. We are looking for the
# smallest tree size with the lowest dev value.

plot(cv_tree$size, cv_tree$dev, type="b")

# prune the tree. My example showed that the best size was 14.

pruned_model = prune.misclass(tree_model, best = 14)

plot(pruned_model)
text(pruned_model, pretty = 0)

########################################################################

# Lets see how the new model is predicting.

tree_pred = predict(pruned_model, testing, type="class")

# Create a confusion matrix of the results.

confusionMatrix(tree_pred, testing$High)