# Survival Analysis tutorial 2 in R
#############################################################


# install.packages("survival")
library(survival)
library(ggplot2)
library(GGally)

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Survivial Analysis/survival_unemployment.csv")
attach(mydata)

# Define variables 
time <- spell
event <- event
X <- cbind(logwage, ui, age)
group <- ui

# Descriptive statistics
summary(time)
summary(event)
summary(X)
summary(group)


#############################################################
# We will first fit a lognormal model.

LogNormalReg1 <- survreg(formula = Surv(time,event) ~ X, data=mydata, na.action=na.omit, dist="lognormal")
summary(LogNormalReg1)

# Then a Weibull Model

WeibullReg1 <- survreg(formula = Surv(time,event) ~ X, data=mydata, na.action=na.omit, dist="weibull")
summary(WeibullReg1)

# finally, a Log-Logistic Model

LogLogisticReg1 <- survreg(formula = Surv(time,event) ~ X, data=mydata, na.action=na.omit, dist="loglogistic")
summary(LogLogisticReg1)

#############################################################
# We will run ANOVA to determine which of these models is the preffered one.
# The Lowest AIC (Ex. -2*LL) will be our preffered model. 

anova(WeibullReg1,LogNormalReg1,LogLogisticReg1)

#############################################################
# this will now discuss some helpful ways to interpret the values of the covariates.

# If the variable in the model is a 0 or 1, we can take the parameter
# value, X, and tranform it by e^X. For example, if logwage parameter estimate is
# -0.42811 will give us e^-0.42811 or 0.6514 (inverse is 0.3486)

# The interpretation of this is that, controlling for other covariates,
# the expected time to arrest for those who recieve logwage is 34.8% less than for those
# who did not recieve logwage.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If the variable in the model is a quantitative, we can take the parameter
# value, X, and tranform it by 100(e^X - 1). For example, if age parameter estimate is
# 0.00948 will give us 100(e^0.00948 - 1) or 0.929.

# The interpretation of this is that, controlling for other covariates,
# the expected time to arrest for each year they age is 0.92% increase than for those
# who did not age a year.




