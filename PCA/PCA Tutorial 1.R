# Principal Component Analysis Tutorial

# http://yatani.jp/HCIstats/PCA

###########################################################################

# Set the working directory for the analysis.

setwd("C:/Users/Derek/Documents/RPackages/PCA")

# Import the data from csv file

mydata<- read.csv("C:/Users/Derek/Documents/RPackages/PCA/PCA.csv")
attach(mydata)

mydata2 <- mydata

# remove the participant field

mydata$Participant <- NULL
mydata$OS<- NULL 


###########################################################################

# Principal Component Analysis

# the first step is to decide what technique we would like to use 
# I would advocate using the princomp which looks at correlation rather
# than covariates

# the columns in the dataframe need to all be the same type in order to reduce
# the a lower dimension feature set. We may need to standardize and clean the variables

pca <- princomp(mydata, cor=T)

# View the summary of the PCA

summary(pca, loadings=T)

# the formula for Comp.1 is as follows:
# Comp.1 = -0.523 * Price - 0.177 * Software + 0.597 * Aesthetics + 0.583 * Brand

# Also, the first 2 components accounts for 84.7% of the total variation.
# generally, 80% is the amount which describes the data well.

# Lets take a look at the scores of the PCA.

plot(pca$scores[,1])
barplot(pca$scores[,1])

# With the graphs, you can see Participant 1 - 8 get negative values, 
# and the other participants get positive values. It seems that this 
# new variable indicates whether a user cares about Price and Software or Aesthetics and Brand for her computer. So, we probably can name this variable as "Feature/Fashion index" or something. There is no definitive answer for this part of PCA. 
# You need to go through your data and make sense what the new variables mean by yourself.



###########################################################################
# PCA and logistic Regression
###########################################################################

# Fit the model with the first two principal components.

model <- glm(OS ~ pca$scores[,1] + pca$scores[,2],
             data=mydata2, family=binomial)

summary(model)
