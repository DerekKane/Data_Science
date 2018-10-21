# Caret Package tutorial

# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

########################################################################

mydata<-iris

library(caret)

#########################################################################

# This section will create the split for training and testing using the caret
# package. The y = component is the variable which the data will be split from.
# The p = 0.75 will create 75% of the records in the training set. 
# The createDataPartition will perform a stratified random sampling.

set.seed(85)
inTrain <- createDataPartition(y = mydata$Species, p = .75,list = FALSE)

# this then creates the actual train and test dataset

training <- mydata[ inTrain,]
testing <- mydata[-inTrain,]

#########################################################################

# We will build a random forest model.

RandomForestModel <- randomForest(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=training, ntree=500, mtry=3, classwt=c(setosa=1,versicolor=3,virginica=1), importance=TRUE)

# Predict on the testing data from the training data

rpartPred <- predict(RandomForestModel, testing, type = "class")

# Create a confusion matrix of the results.

confusionMatrix(rpartPred, testing$Species)
