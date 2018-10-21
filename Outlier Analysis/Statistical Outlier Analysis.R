###################################################################
# Outlier Analysis - Advanced
###################################################################


# install.packages("outliers")

library("outliers")


testout <- data.frame(X1=rnorm(50,mean=50,sd=10),X2=rnorm(50,mean=5,sd=1.5), Y=rnorm(50,mean=200,sd=25))

#Taint the data

testout$X1[10] <- 5
testout$X2[10] <- 5
testout$Y[10] <- 530

testout

# It is often more useful to view the data graphically

# Use the boxplot to review the data

boxplot(testout$X1, ylab="X1")
boxplot(testout$X2, ylab="X2")
boxplot(testout$Y, ylab="Y")

# Use an outlier test to remove the data

# testout$newX1<-ifelse(testout$X1==outlier(testout$X1),NA,testout$X1)
testout$newX1 <- ifelse(testout$X1==outlier(testout$X1),'Yes','No')


# Or for more complicated examples, you can use stats to calculate critical cut off values, here using the Lund Test
# (See Lund, R. E. 1975, "Tables for An Approximate Test for Outliers in Linear Models", 
# Technometrics, vol. 17, no. 4, pp. 473-476. and Prescott, P. 1975, "An Approximate 
# Test for Outliers in Linear Models", Technometrics, vol. 17, no. 1, pp. 129-132.)


#Alternative approach using Lund Test
  
lundcrit<-function(a, n, q) {
    # Calculates a Critical value for Outlier Test according to Lund
    # See Lund, R. E. 1975, "Tables for An Approximate Test for Outliers in Linear Models", Technometrics, vol. 17, no. 4, pp. 473-476.
    # and Prescott, P. 1975, "An Approximate Test for Outliers in Linear Models", Technometrics, vol. 17, no. 1, pp. 129-132.
      # a = alpha
      # n = Number of data elements
      # q = Number of independent Variables (including intercept)
      F<-qf(c(1-(a/n)),df1=1,df2=n-q-1,lower.tail=TRUE)
      crit<-((n-q)*F/(n-q-1+F))^0.5
      crit
      }


# Build a Regression Model for Prediction
testoutlm<-lm(Y~X1+X2,data=testout)

# Create Fitted value column

testout$fitted<-fitted(testoutlm)

# Create Residuals & Studentized Residuals for Analysis

testout$residual<-residuals(testoutlm)

testout$standardresid<-rstandard(testoutlm)

# Define parameters for the Lund Test

n<-nrow(testout)

q<-length(testoutlm$coefficients)

crit<-lundcrit(0.1,n,q)

# Detect the outliers based upon the outlier test

testout$Ynew<-ifelse(abs(testout$standardresid)>crit,NA,testout$Y)

testout

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random Forest Model: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library("randomForest")


# Create the dataset

testout <- data.frame(X1=rnorm(50,mean=50,sd=10),X2=rnorm(50,mean=5,sd=1.5), Y=rnorm(50,mean=200,sd=25))

# Taint the data

testout$X1[10] <- 5
testout$X2[10] <- 5
testout$Y[10] <- 530

testout

RFModel <- randomForest(Y ~ X1+X2, data=testout, ntree=25, mtry=2, importance=TRUE)


# Calculate the Residual

residual <- testout$Y - predict(RFModel)
testout$residual <- testout$Y - predict(RFModel)

# Mean of the residual

meanresid <- mean(residual)

# Standard Deviation

SDeviation <- sd(residual)

# Define the SD to consider the variable an outlier

OutlierThreshold <- 2


SD2 <- OutlierThreshold*SDeviation


# Make testout$Outlier 1 if a residual is 2 st. deviations from the mean, 0 otherwise
testout$Outlier<-ifelse(abs(testout$residual)>SD2, 1, 0)


# Plot this, note that DF$Outs is used to set the color of the points.
plot(testout$residual, col=testout$Outlier+1, pch=16,ylim=c(-500,500))


# Make a new data.frame with no outliers
testout2<-testout[!testout$Outlier,]
nrow(testout2)

#49    Was 50 before, 1 outliers removed

# Plot new data
plot(testout2$residual, col=testout2$Outlier+1, pch=16,ylim=c(-500,500))
