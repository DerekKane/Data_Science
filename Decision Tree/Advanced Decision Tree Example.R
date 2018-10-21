# Decision Tree Tutorial In R

##################################################################

# http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
# http://www.statmethods.net/advstats/cart.html

##################################################################

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Decision Tree/survival_unemployment.csv")
attach(mydata)

## view the first few rows of the data
head(mydata)

# this allows us to look at the classes of all variables.
str(mydata)

# We need to change the event variable from int to factor for the decision tree.
# one approach it to index with the $ sign and the as.factor function
mydata$event <- as.factor(mydata$event)


##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here.

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]

##################################################################

myFormula <- event ~ ui + married + age + child + bluecoll + nonwhite

library(rpart)
library(party)

rpart1 <- rpart(myFormula, data=trainData, control=rpart.control(maxdepth=2))
rpart1

library(partykit)
rpart1a <- as.party(rpart1)

plot(rpart1a)

##################################################################

# By default, rpart will conduct as many splits as possible, then use 10-fold
# cross-validation to prune the tree.

# Specifically, the "one SE" rule is used: estimate the standard error of
# performance for each tree size then choose the simplest tree within one
# standard error of the absolute best tree size.


rpartFull <- rpart(myFormula, data=trainData)
rpartFull

# this will create the set of test results.

library(caret)

rpartPred <- predict(rpartFull, testData)
confusionMatrix(rpartPred, testData$event) # requires 2 factor vectors






myFormula <- event ~ ui + married + age + child + bluecoll + nonwhite
Train_Decision_Tree <- ctree(myFormula, data=trainData, controls=ctree_control(mincriterion=0.95))

# check the prediction
table(predict(Train_Decision_Tree), trainData$event, )

# We can look at the decision tree rules.
print(Train_Decision_Tree)

plot(Train_Decision_Tree)
plot(Train_Decision_Tree, type="simple")

# predict on test data
testPred <- predict(Train_Decision_Tree, newdata = testData)
table(testPred, testData$event)