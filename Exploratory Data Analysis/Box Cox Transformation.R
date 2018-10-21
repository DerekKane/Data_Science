#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Exploratory Data Analysis")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata <- read.csv("EDA.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot X versus Y
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
chart1 <- plot(mydata$X,mydata$Y)
lm1 <- lm(Y~X, data=mydata)


abline(coefficients(lm1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run a Box-Cox Analysis to determine the ideal transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(MASS)

boxcox(lm1)

# we are looking for the largest log-likelihood.
# Since the lambda is betwen 0 and 1 lets review this in 0.10 increments


boxcox(lm1, lambda=seq(0,1,0.1))

lm2 <- boxcox(lm1, lambda=seq(0,1,0.1))
summary(lm2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform variable from X^2 to reciprical
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata$Y2 <- mydata$Y^(1/2)

#-------------------------------------------------------------------------------------------


chart2 <- plot(mydata$X,mydata$Y2)
reg1 <- lm(Y2 ~ X, data=mydata)
abline(coefficients(reg1))

summary(reg1)



# This will produce the summary information for the model

summary(reg1)

# After running the regression, We also need to create the predicted values and residuals for the reg1 model.

prestige_hat<-fitted(reg1) # predicted values
as.data.frame(prestige_hat)

prestige_resid<-residuals(reg1) # residuals
as.data.frame(prestige_resid)







# This section will go over some different techniques for the regression diagnostics.
# We will run the reg1 model. (even though it has already been saved into memory)


reg1 <- lm(prestige ~ education + log2(income) + women, data=Prestige)

# This will create a residual plot of reg1. *Notice that the log2(income) variable shows some patterns indicating heteroscadesticity.

residualPlots(reg1)

# Here are some other diagnostic options. We can convert this as needed to address whatever scenario we need.

residualPlots(reg1, ~ 1, fitted=TRUE) #Residuals vs fitted only
residualPlots(reg1, ~ education, fitted=FALSE) # Residuals vs education only

#-------------------------------------------------------------------------------------------

# This section will be used to help identify the influential variables and create added variable plots.

# Graphs outcome vs predictor variables holding the rest constant (also called partial-regression plots)
# Help identify the effect(or influence) of an observation on the regression coefficient of the predictor variable


reg4 <-lm(prestige ~ education + income + type, data = Prestige)

# This will create the plots and include some descriptions for review.
# id.n = id most influential observations
# id.cex = font size for id.


avPlots(reg4, id.n=2, id.cex=0.7)

#-------------------------------------------------------------------------------------------
# This will help to create QQ Plots to detect outliers.

reg4 <-lm(prestige ~ education + income + type, data = Prestige)

qqPlot(reg4, id.n=3)

# [1] "medical.technicians" [2] "electronic.workers" [3] "service.station.attendant"
# id.n = id observations with high residuals

#-------------------------------------------------------------------------------------------
# The Bonferonni test is another diagnostic used to identify outliers.

reg4 <-lm(prestige ~ education + income + type, data = Prestige)

outlierTest(reg4)


#-------------------------------------------------------------------------------------------
# This section will help to identify high leverage (hat) points.

reg4 <-lm(prestige ~ education + income + type, data = Prestige)

influenceIndexPlot(reg1, id.n=3)

# Cook's distance measures how much an observation influences the overall model or predicted values
# Studentizidedresiduals are the residuals divided by their estimated standard deviation as a way to standardized
# Bonferronitest to identify outliers
# Hat-points identify influential observations (have a high impact on the predictor variables)

# NOTE: If an observation is an outlier and influential (high leverage) then that observation can change the fit of the linear model, 
# it is advisable to remove it. To remove a case(s) type
# reg1a <-update(prestige.reg4, subset=rownames(Prestige) != "general.managers")
# reg1b <-update(prestige.reg4, subset= !(rownames(Prestige) %in% c("general.managers","medical.technicians")))


#-------------------------------------------------------------------------------------------
# This section will create an influence bubble plot.

reg4 <-lm(prestige ~ education + income + type, data = Prestige)

influencePlot(reg1, id.n=3)

# Creates a bubble-plot combining the display of Studentized residuals, hat-values, and Cook's distance (represented in the circles).


#-------------------------------------------------------------------------------------------
# This section will create another QQ Plot without the descriptions. This is to test for normality.


reg4 <-lm(prestige ~ education + income + type, data = Prestige)

qqPlot(reg4)

# Look for the tails, points should be close to the line or within the confidence intervals.
# Quantileplots compare the Studentizedresiduals vsa t-distribution
# Other tests:shapiro.test(), mshapiro.test() in library(mvnormtest)-library(ts)


#-------------------------------------------------------------------------------------------
# This section will test for heteroscadestisity.

reg4 <-lm(prestige ~ education + income + type, data = Prestige)

ncvTest(reg4)

# Breush/Pagan and Cook/Weisberg score test for non-constant error variance. Null is constant variance
# See also residualPlots(reg4).


#-------------------------------------------------------------------------------------------
# This section will test for multicollinearity.

reg4 <-lm(prestige ~ education + income + type, data = Prestige)

summary(reg4)
vif(reg4)

# A gvif> 4 suggests collinearity.

# “When there are strong linear relationships among the predictors in a regression analysis, 
# the precision of the estimated regression coefficients in linear models declines compared 
# to what it would have been were the predictors uncorrelated with each other” (Fox:359)

Prestige$Prestige2 <- predict(reg4, newdata = Prestige, type = "response")

Prestige