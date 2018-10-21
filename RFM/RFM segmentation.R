###################################################################
# RFM Customer Segmentation Analysis
###################################################################

# Clear the workspace

rm(list = ls())

# Set working Directory 

setwd("C:/Users/Derek/Documents/R Scripts/RFM") 

# Load the data

mydata <- read.csv("RFMCustomer.csv", stringsAsFactor=FALSE, header=FALSE)

# Load Libraries for Analysis

# install.packages("data.table")

library(lubridate)
library(data.table)
library(ggplot2)
library(grid)

# Create the names for the different columns

names(mydata) <- c("Date", "OrderNumber", "CustomerId","ProductId", "Value")

# Parse the dates into the dataset

mydata$ParsedDate <- fast_strptime(mydata$Date, "%m/%d/%Y")
#mydata$ParsedDate <- as.Date(mydata$Date, "%m/%d/%Y")

NOW <- fast_strptime("2015-10-20", "%Y-%m-%d")

# Calculate the three attributes separately and 
# then combine into an RFM Table by merging on Customer Id. 
# Recency is calculated in two steps. First calculate the maximum Order 
# Date for each customer and then, separately, the difference between that date and NOW. 
# Frequency and Monetary value is computed similarly with an aggregation. 

# All three attributes are then joined using two merge operations.

R_table <- aggregate(ParsedDate ~ CustomerId, mydata, FUN=max) # Calculate R
R_table$R <- as.numeric(NOW - R_table$ParsedDate)

F_table <- aggregate(OrderNumber ~ CustomerId, mydata, FUN=length) # Calculate F

M_table <- aggregate(Value ~ CustomerId, mydata, FUN=sum) # Calculate M

RFM_table <- merge(R_table,F_table,by.x="CustomerId", by.y="CustomerId") # Merge R with F
RFM_table <- merge(RFM_table,M_table, by.x="CustomerId", by.y="CustomerId") # Merge M into RF

RFM_table$ParsedDate <- NULL # Remove unnecessary column

names(RFM_table) <- c("CustomerId", "R", "F", "M") # And change names

##################################################################################
# Create the K-Means algorithm for clustering segmentation
##################################################################################

# First, we will standardize and scale the data for clustering

df <- scale(RFM_table[-1])
df <- as.data.frame(df)

# Function to determine the correct number of clusters



# Determine number of clusters
wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:5) wss[i] <- sum(kmeans(df,
                                     centers=i)$withinss)

# Create the scree plot for analysis

plot(1:5, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Determine the number of clusters

cl <- kmeans(df,3)

clusternum <- cl$cluster

# Plot the results
plot(df$R,df$F, col=clusternum+1) 


# Bind Cluster results back to the main data frame

RFM_table$Cluster <- cl$cluster


##################################################################################
# Additional Plots for evaluating the Cluster
##################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create 2 new datasets with cluster 1 and all species

RFM_table2 <- RFM_table

mydata2 <- RFM_table2

RFM_table2$Cluster <- as.character(RFM_table2$Cluster) 
RFM_table2$Cluster <- 'population' 

# Focus on Cluster 1

clustdata <- subset(mydata2, mydata2$Cluster=='2')

newdata <- rbind(clustdata, RFM_table2)


# plot Recency density of cluster 1 versus total population using ggplot2

plot.new()

plotexample <-ggplot(newdata, aes(x=newdata$R, fill=newdata$Cluster)) + geom_density(alpha=0.3)

plotexample 



########################################################################################
# Function to automate the cluster evaluation
########################################################################################

# Loop through columns the dataframe

# newdata$CustomerId <- NULL


R_density <- ggplot(newdata, aes(x=newdata$R, fill=newdata$Cluster)) + geom_density(alpha=0.3)
F_density <- ggplot(newdata, aes(x=newdata$F, fill=newdata$Cluster)) + geom_density(alpha=0.3)
M_density <- ggplot(newdata, aes(x=newdata$M, fill=newdata$Cluster)) + geom_density(alpha=0.3)

# Must Run the multiplot function below.

multiplot(R_density, F_density, M_density, cols = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~









for (i in names(newdata[-4])){
  par(mfrow=c(2,2))
  plot.new()
  plotexample <-ggplot(newdata, aes(x=newdata[,i], fill=newdata$Cluster)) + geom_density(alpha=0.3)
  plotexample}

plotexample



check <- names(newdata[-4])

ncol(newdata[-4])

names(newdata[-4])















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional Considerations with RFM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# http://stackoverflow.com/questions/16714020/loop-through-data-frame-and-variable-names

# In cases when your file is large and does not fit into memory in the standard data.frame, 
# try using the awesome data.table package. Note that there are some 
# significant differences in working with data in this format 
# (please read the manual or my previous article on data.table).

# The input file is imported using fread instead of read.csv and column names should 
# be set with setNames.

# mydata <- fread("RFMCustomer.csv", sep="|", stringsAsFactor=FALSE)
# setNames(mydata, c("Date", "OrderNumber", "CustomerId","ProductId", "Value"))
# mydata[,ParsedDate := fast_strptime(Date, "%Y-%m-%d")]
# NOW <- fast_strptime("2014-03-02", "%Y-%m-%d")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The actual RFM Table calculation can be combined 
# into a single command thanks to data.frame's aggregation syntax. 
# Note the by="CustomerId" parameter which specifies the aggregation column.

# RFM_table <- mydata[,list(
#  R = as.numeric(NOW - max(ParsedDate)),
#  F = length(OrderNumber),
#  M = sum(Value)),
#  by="CustomerId"
#  ]

# http://www.marketingdistillery.com/2014/11/02/rfm-customer-segmentation-in-r-pandas-and-apache-spark/
