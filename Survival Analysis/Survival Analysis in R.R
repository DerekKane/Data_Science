# Survival Analysis in R
#############################################################
# http://www.r-statistics.com/2013/07/creating-good-looking-survival-curves-the-ggsurv-function/

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

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

# nicer looking plot from GGPlot2 and GGally
# censored observations have the red crosses

# ggsurv(kmsurvival)

#############################################################

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")

# nicer looking plot from GGPlot2 and GGally

pl2 <- ggsurv(kmsurvival1)

# this next section allows for us to add descriptions into the plot 

(pl2 <- pl2 + guides(linetype = F) + 
   scale_colour_discrete(name = 'Outcome', breaks = c(1,2), labels=c('Recidivism', 'No Recidivism')))


#############################################################


# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")

#############################################################

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)

#############################################################

# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS

exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)
