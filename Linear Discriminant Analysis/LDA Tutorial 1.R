# Linear Discriminant Analysis Tutorial

# https://www.youtube.com/watch?v=s8pvp2Ctxfc

###########################################################################
# Load the MASS library

library(MASS)


# Bring in the iris dataset

mydata<-iris

###########################################################################

# Run the LDA analysis

LDAModel <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=mydata)

mydata$class <- predict(LDAModel, newdata=mydata)$class

###########################################################################

# Determine the classification Matrix

table(mydata$Species, mydata$class)

###########################################################################

# Cross Validation of Data

LDAModel2 <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, 
                 data=mydata,
                 CV = TRUE)

# Look at the assigned classes from the validation 

table(LDAModel2$class, mydata$Species)
