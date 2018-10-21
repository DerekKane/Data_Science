# Predictions off of multiple clusters Tutorial

#########################################################################

# Bring in the iris dataset

mydata<-iris

# Remove the species variable

mydata$Species <- NULL

#########################################################################

# Load in the clustering packages

library(cluster)
library(mclust)

#########################################################################

# Segment and determine clusters based on the characteristics.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternative method for determining clusters - kmeans approach
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Determine number of clusters by creating an scree plot

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", main= "Scree Plot", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Look for an "elbow" in the scree plot. The interior node at 
# which the angle formed by the 'arms' is the smallest. 
# This scree-plot get either 2 or 3 as the options available. 
# Suppose we go with 3 (because we know there are 3 species of iris).

# Use optimal no. of clusters in k-means #

k1=3

# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) 

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# rename fit.cluster to classif

classif <- fit$cluster

# append cluster assignment
mydata <- data.frame(mydata, classif)


#########################################################################

# Make the classif variable as a factor for further analysis.

mydata$classif = as.factor(mydata$classif)


# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(12)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]


#########################################################################

# This section will create 3 seperate prediction models to be used later.

library(party)

DTModel <- ctree(classif ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=trainData, controls=ctree_control(mincriterion=0.95))

# We can look at the decision tree rules.
print(DTModel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(randomForest)

RFModel <- randomForest(classif ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, 
                        data = trainData, ntree = 3000, proximity = TRUE, importance = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(e1071)

SVMModel <- svm(classif ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = trainData)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This code will split the dataset into only those which have classif = 1,2,3

testData1 <- subset(testData, !(classif %in% c("2","3"))) # Only 1
testData2 <- subset(testData, !(classif %in% c("1","3"))) # Only 2
testData3 <- subset(testData, !(classif %in% c("1","2"))) # Only 3


# This section will use a different model for each cluster to create a prediction.

testData1$Pred1 <- predict(DTModel, newdata=testData1) 
testData2$Pred1 <- predict(RFModel, newdata=testData2)
testData3$Pred1 <- predict(SVMModel, newdata=testData3)


# This function will merge the 3 datasets back into a single dataset

testDataFull = rbind(testData1, testData2, testData3)


#########################################################################


# testData$Pred1 <- predict(DTModel, newdata=testData[testData$classif==1,])