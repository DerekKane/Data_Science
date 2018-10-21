# Cluster Analysis Tutorial

##############################################################

# Set the working directory for the analysis.

setwd("C:/Users/dkane/Documents/R Packages/Cluster Analysis")

##############################################################


# load the Iris dataset

mydata<-iris
mydataoriginal<-iris

attach(mydata)


##############################################################
# Remove the values in the Species column.   

mydata$Species<-NULL

# perform the KMeans clsuter for X number of clusters.
# there are other techniques to identify the potential number of clusters
# however, for this example, we will use 3.

results<-kmeans(mydata, 3)

results

# the available components can be used to see the results of the algorithm.
# This one will show each cluster outlined in the iris dataset.

results$cluster

# this is how we can compare the results betwen the k means cluster
# from mydata and the mydataoriginal sets.

table(mydataoriginal$Species, results$cluster)

# this will plot the original dataset with the coloring of the kmeans
# cluster results.

plot(mydataoriginal[c("Petal.Length", "Petal.Width")], col= results$cluster)

# compare this to the original results.

plot(mydataoriginal[c("Petal.Length", "Petal.Width")], col= mydataoriginal$Species)

##############################################################

# Bind the results back to the original dataset for further review.

mydataoriginal$Cluster <-results$cluster 

##############################################################
# this is how to create a dendrogram for the cluster analysis
# we want the 5th column in the dataset to be clustered so we use [-5]

mydataclust<-hclust(dist(mydataoriginal[-5]))

# plot the results
plot(mydataclust, labels=mydataoriginal$Species)

# this will identify 3 clusters on the dendrogram
rect.hclust(mydataclust, 3)

# Alternatively you can cut the dendrogram at a specific height by adding the h argument.

plot(mydataclust, label=mydataoriginal$Species) 
rect.hclust(mydataclust, h=2)

# plot(mydataclust)

# This is To save the cluster numbers to a new 
# variable in the dataset, use the cutree function

mydataoriginal$Cluster<-cutree(mydataclust, 3)