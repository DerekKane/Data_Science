# Decision Tree Tutorial In R

##################################################################

# http://www.rdatamining.com/examples/decision-tree
# http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf

##################################################################

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Decision Tree/survival_unemployment.csv")
attach(mydata)

## view the first few rows of the data
head(mydata)

##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here.

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]

##################################################################

library(party)
myFormula <- event ~ ui + married + age + child + bluecoll + nonwhite
Train_Decision_Tree <- ctree(myFormula, data=trainData, controls=ctree_control(mincriterion=0.95))

# check the prediction
table(predict(Train_Decision_Tree, trainData$event, ))

# We can look at the decision tree rules.
print(Train_Decision_Tree)

plot(Train_Decision_Tree)
plot(Train_Decision_Tree, type="simple")

# predict on test data
testPred <- predict(Train_Decision_Tree, newdata = testData)
table(testPred, testData$event)

