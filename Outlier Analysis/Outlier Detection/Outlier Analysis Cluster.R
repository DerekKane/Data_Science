###################################################################
# Outlier Analysis - Clustering
###################################################################



# remove species from the data to cluster
iris2 <- iris[,1:4]


# Create an outlier for detection

iris2$Sepal.Length[10] <- 10
iris2$Sepal.Width[10] <- 10
iris2$Petal.Length[10] <- 10
iris2$Petal.Width[10] <- 10


# Prepare a KMeans Cluster Analysis 

kmeans.result <- kmeans(iris2, centers=3)

# cluster centers
kmeans.result$centers

# calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((iris2 - centers)^2))

# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]

# who are outliers
print(outliers)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# But how can I use the relative instead of the absolute distance to find outliers?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# calculate mean distances by cluster:
m <- tapply(distances, kmeans.result$cluster,mean)

# divide each distance by the mean for its cluster:
d <- distances/(m[kmeans.result$cluster])

# Your outliers:

d[order(d, decreasing=TRUE)][1:5]

