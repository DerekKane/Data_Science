###################################################################
# Customer Segmentation Analysis
###################################################################

# https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html


# Set working Directory 

setwd("C:/Users/dkane/Documents/R Scripts/Clustering/") 


# Read in File

offers<-read.table('offers.csv', sep = ',', header=T)


# Read in File - number 2

transactions<-read.table('transactions.csv', sep = ',', header=T)

# Create transaction matrix (a pivot table like in Excel way!)

# install.packages("reshape")
library(reshape)

pivot<-melt(transactions[1:2])

## Using CustomerLastName as id variables

pivot<-(cast(pivot,value~CustomerLastName,fill=0,fun.aggregate=function(x) length(x)))
pivot<-cbind(offers,pivot[-1])

# write.csv(file="pivot.csv",pivot) # to save your data

cluster.data<-pivot[,8:length(pivot)]
cluster.data<-t(cluster.data)

head(cluster.data)

# Step 2 - Distances and Clusters


# install.packages("cluster")

library(cluster)
D=daisy(cluster.data, metric='gower')

H.fit <- hclust(D, method="ward")


plot(H.fit) # display dendrogram

groups <- cutree(H.fit, k=4) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters
rect.hclust(H.fit, k=4, border="red") 


# 2D representation of the Segmentation:
clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')


# Merge Data

cluster.deals<-merge(transactions[1:2],groups,by.x = "CustomerLastName", by.y = "row.names")

colnames(cluster.deals)<-c("Name","Offer","Cluster")
head(cluster.deals)

# Get top deals by cluster
cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
cluster.topDeals<-cbind(offers,cluster.pivot[-1])
head(cluster.topDeals)