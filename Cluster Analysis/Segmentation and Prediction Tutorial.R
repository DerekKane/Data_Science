# Segmentation and Prediction example

# Based off http://marketing-yogi.blogspot.com/2012/12/targeting-in-r-session-6-pda-caselet.html
#########################################################################

# Bring in the iris dataset

mydata<-iris

# Remove the species variable

mydata$Species <- NULL

#########################################################################

# Load in the clustering packages

library(cluster)
library(mclust)

# Help function

# ?mclust

#########################################################################

# Segment and determine clusters based on the characteristics.

# Run the Mclust procedure. 
# This is a model based cluster and is the preferred technique to use.
# robust to distributional and linkage assumptions and because it 
# penalizes for surplus complexity (resolves the fit-complexity tradeoff in an objective way).

# My thumb-rule is: When in doubt, use model based clustering. And yes, mclust is available *only* on R to my knowledge.

fit <- Mclust(mydata) 

# View the Result
fit 

# Define the classification variable

classif = fit$classification 

# print cluster sizes

for (i1 in 1:max(classif)){print(sum(classif==i1))}

# Cluster Plot against 1st 2 principal components

require(cluster)
fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0) 

# Apply the Clusters to the mydata

mydata$Cluster <- fit$classification

#------------------------------------------------------------------------

# The relative probabilities for each of the records' cluster membership.

mydata$variance<-fit$parameters


# determine the cluster centers
fit$centers

# calculate distances between objects and cluster centers
centers <- fit$centers[fit$cluster, ]
distances <- sqrt(rowSums((mydata - centers)^2))

# Apply distances as a new variable in mydata

mydata = cbind(mydata, distances)


# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]

# who are outliers
print(outliers)


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

#------------------------------------------------------------------------

# This section will show how to find outliers within the kmeans clusters

# determine the cluster centers
fit$centers

# calculate distances between objects and cluster centers
centers <- fit$centers[fit$cluster, ]
distances <- sqrt(rowSums((mydata - centers)^2))

# Apply distances as a new variable in mydata

mydata = cbind(mydata, distances)


# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]

# who are outliers
print(outliers)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph

library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)


#########################################################################

# This function will create the split between the test and training samples 
# at a 66% to 33% split. this also sets up the RF algorithm to predict in a
# single function.

# run split.data func on our data
mydata = cbind(mydata, classif)
mydata$classif = as.factor(mydata$classif)


# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(12)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]

#########################################################################

library(randomForest)

RFModel <- randomForest(classif ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, 
                        data = trainData, xtest=testData[,names(testData)!='classif'], ytest = testData[,'classif'],
                        ntree = 3000, proximity = TRUE, importance = TRUE)

# the xtest and ytest code needs to be included in order to ensure that the
# training and test set is being compared properly. I know, its a strange code...
# explanation: http://stackoverflow.com/questions/15318670/error-x-and-xtest-must-have-the-same-number-of-columns-when-using-randomforest

print(RFModel)


#########################################################################