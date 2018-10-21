#################################################################
# Statistical Evaluation - Cluster Analysis
#################################################################

setwd("C:/Users/dkane/Documents/R Scripts/Statistics/") 

library(caret)
library(ggplot2)

#################################################################
# Load the data
#################################################################

# Import the Iris dataset 

mydata <- iris

mydata$Species <- NULL


#################################################################
# Design & Evaluate the Cluster - Scaling
#################################################################

# standardize the variables so they dont over influence the cluster
clusterdata <- mydata

# Single Variable
clusterdata$Sepal.Length <- scale(clusterdata$Sepal.Length, center = TRUE, scale = TRUE)

# Entire dataframe
clusterdata <- data.frame(lapply(clusterdata, function(x) scale(x, center = TRUE, scale = TRUE)))


#################################################################
# Design & Evaluate the Cluster
#################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Determine number of clusters using Kmeans
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wss <- (nrow(clusterdata)-1)*sum(apply(clusterdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clusterdata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Ideal KMEAN cluster  = 3

# K-Means Cluster Analysis
kmfit <- kmeans(clusterdata, 3) # 3 cluster solution

# get cluster means 
aggregate(clusterdata,by=list(kmfit$cluster),FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, kmfit$cluster)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model Based Clustering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(mclust)

mcfit <- Mclust(clusterdata)
plot(mcfit) # plot results 
summary(mcfit) # display the best model


mydata <- data.frame(mydata, mcfit$classification)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate the Kmean clusters through principal component analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph

library(cluster) 
clusplot(mydata, kmfit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions

library(fpc)
plotcluster(mydata, kmfit$cluster)

                          
#################################################################################
# Density Plots - KMeans example
#################################################################################

plot.new()

plotexample <-ggplot(mydata, aes(x=mydata$Sepal.Width)) + geom_density(alpha=0.3)

plotexample


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create 2 new datasets with cluster 1 and all species

allspecies <- mydata

allspecies$kmfit.cluster <- as.character(allspecies$kmfit.cluster) 
allspecies$kmfit.cluster <- 'population' 

# Focus on Cluster 1

clustdata <- subset(mydata, mydata$kmfit.cluster=='1')

newdata <- rbind(clustdata, allspecies)


# plot sepal.width density of cluster 1 versus total population using ggplot2

plot.new()

plotexample <-ggplot(newdata, aes(x=newdata$Sepal.Width, fill=newdata$kmfit.cluster)) + geom_density(alpha=0.3)

plotexample 

########################################################################################
# Function to automate the cluster evaluation
########################################################################################

# Loop through columns the dataframe
 
for (i in 1:newdata[-5]){
    plot.new()
    plotexample <-ggplot(newdata, aes(x=newdata[,i], fill=newdata$kmfit.cluster)) + geom_density(alpha=0.3)
    plotexample}
 
plotexample

check <- names(newdata[-5])

ncol(newdata[-5])

names(newdata[-5])
