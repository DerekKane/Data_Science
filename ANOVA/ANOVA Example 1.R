# ANOVA Example

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/R Scripts/ANOVA")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata <- read.csv("diet.csv")
attach(mydata)

# Review the data

summary(mydata)

# This an instructable on how to do an Analysis of Variance test, commonly called ANOVA, in the statistics software R.
# ANOVA is a quick, easy way to rule out un-needed variables that contribute little to the explanation of a dependent variable. 
#It is acessable and applicable to people outside of the statistics field.

# Perform ANOVA on a single variable

aov.mydata <- aov(WeightLoss~Diet)
summary(aov.mydata)


# Perform ANOVA on a two variables

aov.mydata2 <- aov(WeightLoss~Diet + Exercise)
summary(aov.mydata2)


# Perform ANOVA on a two variables and interaction term

aov.mydata3 <- aov(WeightLoss~Diet + Exercise + Diet*Exercise)
summary(aov.mydata3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternative method for interaction terms
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

aov.mydata3 <- aov(WeightLoss~Diet*Exercise)
summary(aov.mydata3)



# Prepare an interaction plot of diet and exercise.

interaction.plot(mydata$Diet,mydata$Exercise,mydata$WeightLoss,type="l",xlab="Diet",ylab="Mean",trace.label="Exercise")
