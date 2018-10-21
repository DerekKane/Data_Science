#####################################################################################
# Ridge Regression Tutorial.
#####################################################################################

# install.packages("ridge")

library(lasso2)
library(colorspace)
library(ridge)

data(Prostate)
mydata <- Prostate


# Using R's automatic selection methods to select the biasing constant:
# R calls this constant "lambda"

select(lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
                data=mydata, lambda = seq(0,1,0.001)))

# The generalized cross-validation (GCV) criterion says
# the optimal biasing constant is 1. The value of 1 here is telling us that we need
# to extend the lambda sequence to a larger range.

select(lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
                data=mydata, lambda = seq(0,10,0.1)))


# now we have a GVC = 6.5 specified for lambda in this case which falls in the 
# lambda range. This is good.

#####################################################################################
# Ridge Model
#####################################################################################

# We will create a train model with a range of values for lambda instead of 
# an exact value. This will help us create our diagnostic plots.
# Later we will specify the lambda in the model.


ridge.train <- lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                      data=mydata, lambda = seq(0,10,0.1))




#####################################################################################
# Ridge GVC Plot
#####################################################################################

plot(seq(0,10,0.1), ridge.train$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

lambda.ridge <- seq(0,10,0.1)[which.min(ridge.train$GCV)]

# Lets add the line to our plot
abline(v=lambda.ridge, lty=2, col="red")


# Now lets take a look at the Ridge from our train model.

colors <- rainbow_hcl(8, c = 65, l = 65) # 8 is the number or ind. variables

plot.new()
matplot(seq(0,10,0.1), coef(ridge.train)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
text(rep(10, 9), coef(ridge.train)[length(seq(0,10,0.1)),-1], colnames(mydata)[-9], pos=4, col=colors)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is a graph that is more visually friendly for novice ridge users.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

threshold <- seq(0,100,0.1)

ridge.train2 <- lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                        data=mydata, lambda = threshold)



colors <- rainbow_hcl(8, c = 65, l = 65) # 8 is the number or ind. variables

plot.new()
matplot(threshold, coef(ridge.train2)[,-1], xlim=c(0,105), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
text(rep(10, 9), coef(ridge.train2)[length(threshold),-1], colnames(mydata)[-9], pos=4, col=colors)



#####################################################################################
# Final Ridge Model
#####################################################################################

# now we have a GVC = 6.5 specified for lambda in this case which falls in the 
# lambda range. This is good.

ridge.reg <- lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                      data=mydata, lambda = lambda.ridge)

# Printing the ridge-regression coefficient estimates for this problem:

ridge.reg




#####################################################################################
# Predict Results 
#####################################################################################

# There's no predict() method for "ridgelm" objects. We have to build the formula by
# hand. 

pred.ridge <- coef(ridge.reg)[1] + coef(ridge.reg)[2]*mydata[,1] + 
  coef(ridge.reg)[3]*mydata[,2] + coef(ridge.reg)[4]*mydata[,3] + 
  coef(ridge.reg)[5]*mydata[,4] + coef(ridge.reg)[6]*mydata[,5] + 
  coef(ridge.reg)[7]*mydata[,6] + coef(ridge.reg)[8]*mydata[,7] +
  coef(ridge.reg)[9]*mydata[,8]

# or we can predict a variable.

mydata$YHat <- coef(ridge.reg)[1] + coef(ridge.reg)[2]*mydata[,1] + 
  coef(ridge.reg)[3]*mydata[,2] + coef(ridge.reg)[4]*mydata[,3] + 
  coef(ridge.reg)[5]*mydata[,4] + coef(ridge.reg)[6]*mydata[,5] + 
  coef(ridge.reg)[7]*mydata[,6] + coef(ridge.reg)[8]*mydata[,7] +
  coef(ridge.reg)[9]*mydata[,8]
  
MSE.Ridge<-(sum((mydata$lpsa-pred.ridge)^2))/nrow(mydata)
# RMSE.Ridge<-sqrt((sum((mydata$lpsa-pred.ridge)^2))/nrow(mydata))

MSE.Ridge

#####################################################################################
# Additional Plots
#####################################################################################

# These are calculated using the ridge package.





#####################################################################################
# Additional Performance Metrics
#####################################################################################

# Performance Metrics

beta.ridge <- coef(ridge.reg)*lambda.ridge
resid.ridge <- mydata$lpsa - beta.ridge[1] - as.matrix(mydata[,1:8])%*%beta.ridge[2:9]

# To find df
d <- svd(as.matrix(mydata[,1:8]))$d
df <- nrow(mydata) - sum(d^2/(lambda.ridge+d^2))

# Here is the calcualtion of errors

rss.ridge <- sum(resid.ridge^2)/df

rss.ridge

