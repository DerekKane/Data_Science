# Feature Selection for Predictive Analysis.
# http://cran.r-project.org/web/packages/FSelector/FSelector.pdf

# This package allows for parellel processing on the computer to speed up
# model deployment. If we have 8 cores, then 6 cores allows for multitasking.

library(doParallel)
cl <- makeCluster(6) 
registerDoParallel(cl)

##############################################################

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Survivial Analysis/survival_unemployment.csv")
attach(mydata)

# The FSelector Package contains many different techniques for identifying
# variables to include within a model.

#install.packages("FSelector")
library(FSelector)

#This will identify the variables based upon the chi.squared values.

weights <- chi.squared(event~., mydata)
print(weights)
subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "event")
print(f)

# This algorithm finds attribute subset using correlation and entropy measures 
# for continous and discrete data.

subset <- cfs(event~., mydata)
f <- as.simple.formula(subset, "event")
print(f)

# The algorithm finds attribute subset using consistency measure for continous and discrete data.

subset <- consistency(event~., mydata)
f <- as.simple.formula(subset, "event")
print(f)

# The algorithm finds weights of continous attributes basing on their correlation with continous class
# attribute.


#library(mlbench)
#data(BostonHousing)

# This formula brings in only numeric variables
d=mydata[-4] 

# this is for linear correlation.

weights <- linear.correlation(event~., d)
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "event")
print(f)

# This is for correlation ranking.

weights <- rank.correlation(medv~., d)
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "medv")
print(f)

