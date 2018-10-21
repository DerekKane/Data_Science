# Logistic Regression In R

##################################################################

# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# http://www.unc.edu/courses/2010fall/ecol/563/001/docs/lectures/lecture21.htm
# http://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/

##################################################################

library(aod)
library(ggplot2)
library(reshape2)
library(car)

##################################################################

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Survivial Analysis/binary.csv")
attach(mydata)

## view the first few rows of the data
head(mydata)

summary(mydata)

# First we must convert rank to a factor to indicate that it should be used
# as a categorical variable.

mydata$rank <- factor(mydata$rank)


##################################################################

#This will provide the standard deviations in the data 
sapply(mydata, sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

# Correlation Matrix

cor(mydata)

# Heatmap of correlations

qplot(x=Var1, y=Var2, data=melt(cor(mydata, use="p")), 
      fill=value, geom="tile") +   scale_fill_gradient2(limits=c(-1, 1))

# scatterplot for all variables

plot(mydata)
plot(gre~gpa)

# ggplot2 version of scatterplot

plotmatrix(mydata[,1:4], colour="gray20") +
  geom_smooth(method="lm")

#Histogram for GRE

hist(gre)

# GGPlot2 of Histogram
# overlay histogram, empirical density, and normal density

p0 = qplot(gre, geom = 'blank') + geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + stat_function(fun = dnorm, aes(colour = 'Normal')) + geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = 'Density', values = c('red', 'blue')) + opts(legend.position= c(0.85,0.85))

print(p0)


##################################################################

# Now lets produce a Logistic Regression Model

##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here.

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]


##################################################################
# Forward, Backward, Stepwise Selection methods. Lower AIC indicates a better model.


# Backward Elimination
step(glm(admit ~ gre + gpa + rank, data = trainData), direction="backward")

# Forward Elimination
step(glm(admit~1, data = trainData), direction="forward", scope=~gre + gpa + rank)

# Stepwise Method
step(glm(admit ~ gre + gpa + rank, data = trainData), direction="both")

##################################################################

# Here is the logistic regression model based off of the results.

mylogit <- glm(admit ~ gre + gpa + rank, data = trainData, family = "binomial")
summary(mylogit)

##################################################################

# this section will evaluate the models fit and performance.

# Residual Plot
residualPlots(mylogit, layout=c(1, 3))

# influence Plot
influenceIndexPlot(mylogit, vars=c("Cook", "hat"), id.n=3)

#VIF
vif(mylogit)

##################################################################

## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

## Put the coefficients and CI in a format onto a useful scale.

exp(mylogit$coefficients)
exp(confint(mylogit))

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

##################################################################
# This section will perform the Wald Test for the variable rank.
# The Terms 4:6 indicate the 4th term in the model, rank2 through rank4

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

# The chi-squared test statistic of 20.9, with three degrees of freedom is associated 
# with a p-value of 0.00011 indicating that the overall effect of rank is statistically significant.

##################################################################
# We need to utilize ANOVA to diagnose the strength of the models parameters.


anova(mylogit, test="Chisq")


##################################################################

# This section will calculate the predictive performance of the model.

# Use the "predict" function to get fits to logits
# and  then convert to predictions of P["success"]

predicted=predict(mylogit, na.action=na.exclude)

# The predicted formula above is the predicted log odds. 

prob=1/(1+exp(-predicted))

# confusion matrix with a probility of 0.5 as a threshold. 
# This probability can be adjusted to different parameters that will
# influence the confusion matrix outcome.

precision<-function(c) {
  
  tab1 <- table(fitted(mylogit)>c, trainData$admit)
  
  out <- diag(tab1)/apply(tab1, 2, sum)
  
  names(out) <- c('specificity', 'sensitivity')
  
  list(tab1, out)
  
}

precision(.5)

##################################################################

# How can we determine the proper precision threshold that balances the 
# FP and Fn? We set the default to 0.5 but what should it be?

library(ROCR)
pred1 <- prediction(fitted(mylogit), trainData$admit)

stats1 <- performance(pred1 ,'tpr', 'tnr')

# stats1@alpha.values[[1]]
# If the first value is infinite, we need to change this to a 1
# the code below will make this change.

stats1@alpha.values[[1]][1] <- 1

# This code plots specificity and sensitivity against the cutoffs.
# The MDT and MST are different measures for performance. We must choose one. 

plot(stats1@alpha.values[[1]], stats1@x.values[[1]], xlab=stats1@alpha.name, ylab='Classification rate', type='s')
lines(stats1@alpha.values[[1]], stats1@y.values[[1]], type='s', col=2)
legend('right', c('specificity', 'sensitivity', 'MDT', 'MST'), col=c(1,2,4,'seagreen'), lty=c(1,1,2,3), lwd=c(1,1,1,2), bty='n', cex=.9)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate the MDT  or minimum difference threshold.

which.min(abs(stats1@x.values[[1]]-stats1@y.values[[1]]))

# Use the value from the function above to determine the threshold. Note this is 173 
stats1@alpha.values[[1]][173]

# Add these pieces to the graph
stats1@x.values[[1]][173]
stats1@y.values[[1]][173]
abline(v=stats1@alpha.values[[1]][173], lty=2, col=4)

# Calculate the new confusion matrix.
precision(stats1@alpha.values[[1]][173])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the MST or Maximised Sum Threshold.

which.max(stats1@x.values[[1]]+stats1@y.values[[1]])

# Use the value from the function above to determine the threshold. Note this is 137
stats1@alpha.values[[1]][137]

# Add these pieces to the graph

stats1@x.values[[1]][137]
stats1@y.values[[1]][137]
abline(v=stats1@alpha.values[[1]][137], lty=3, col='seagreen', lwd=2)
  
# Calculate the new confusion matrix.
precision(stats1@alpha.values[[1]][137])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is how we can create an ROC Curve using ROCR.

stats1<-performance(pred1,'tpr','fpr')
plot(stats1@x.values[[1]],stats1@y.values[[1]],type='s',ylab=stats1@y.name,xlab=stats1@x.name)


#Calculate area under the curve. This is showing a value of 0.693
stats1<-performance(pred1,'auc')
str(stats1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This section will perform a 10 fold cross validation of the model.

library(DAAG)
cv.binary(mylogit)

# The value of 0.702 indicates the level of predictive accuracy. 
# We can compare this to the accuracy value of the confusion matrix.


#################################################################
# We may want to check the quality of the overall model not just the covariates.

# Determine the test Chi-Square statistic.
with(mylogit, null.deviance - deviance)

# Determine the degrees of freedom.
with(mylogit, df.null - df.residual)

# Finally, we can determine the p-value.
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of less than 0.001 
# tells us that our model as a whole fits significantly better than an empty model. 
# This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood). 

# To see the model's log likelihood, we type:
logLik(mylogit)


##########################################################

#This section will show how to use the data for predictions
# We must first create a new dataset called "newdata1"

# newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

## view data frame
# newdata1

#This will allow for us to apply the model from earlier to make the prediction.

testData$rankP <- predict(mylogit, newdata = testData, type = "response")

# View the dataframe
testData




