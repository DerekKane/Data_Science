# Here is the assignment code in R

# Assign data visualization libraries for the analysis

library(ggplot2)
library(forecast)


# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/kane.de/Documents/RPackages/Projects")
#-------------------------------------------------------

# Load the data set into R

read.csv("ABCsocial.csv") -> ABCData

attach(ABCData)

# This is a check to see the first 10 lines of the data.

head(ABCData, n=10)


# --------------EDA---------------------------------------

# Lets create a scatter plot matrix of the variables

pairs(~ ie.ci2 + ie.smi2 + ie.ad, data=ABCData)



# --------------Model Building----------------------------


# Now I will create a basic linear regression model for customer inquiries (ie.ci2) which we will call "results1"
# If I add a + 0 to the end it will make the intercept = 0

results1 <- lm(ie.ci2 ~ ie.smi2 + ie.ad, data=ABCData)


# This will create a summary of the regression model diagnostics 

summary(results1)


# --------------Model Building----------------------------
# Now I need to try and remove





# --------------ARIMA Forecast----------------------------

#fit <- auto.arima(ABCData)


# Customize the confidence intervals
#forecast(fit, level=c(80, 95, 99), h=3)