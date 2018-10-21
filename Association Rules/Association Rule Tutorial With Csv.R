# Association Rule tutorial #2

#####################################################################
# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/derek/Documents/R Scripts/Association Rules")

mydata <- read.csv("Transaction.csv")

mydata

#####################################################################

library(arules)
library(arulesViz)

# we must first reshape the data by flipping it on its side.

# this will cross tabluate the inital table

mydata2 <- table(mydata$Groupings, mydata$Items)

mydata_flip <- as.data.frame.matrix(mydata2) 

mydata_flip2 <- as(as.matrix(mydata_flip), "transactions")

#####################################################################

# this will create the association Rules.


rules = apriori(mydata_flip2, parameter=list(supp=0.95, conf=0.95, target="rules")) 

inspect(rules)