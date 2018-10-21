###############################################################################
# Lasso and Elastic Net Tutorial.
###############################################################################

# http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html



library(glmnet)
library(lasso2)
library(colorspace)
library(lars)
library(elasticnet)

data(Prostate)
mydata <- Prostate

# Convert the dataset into a matrix for further processing.

y <- as.numeric(mydata[,9])
x <- as.matrix(mydata[,1:8])



###############################################################################
# build a lasso model using LARS
###############################################################################

model.lasso <- lars(x, y, type="lasso")
lambda.lasso <- c(model.lasso$lambda,0)
beta <- coef(model.lasso)

# set the colors for the output.

colors <- rainbow_hcl(8, c = 65, l = 65) # 8 is the number or ind. variables

# Plot the lasso

matplot(lambda.lasso, beta, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors)
text(rep(-0, 9), beta[9,], colnames(x), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

# It may help visualization if you plot using the scaled X

beta.scale <- attr(model.lasso$beta, "scaled:scale")
beta.rescaled <- beta
for(j in 1:9){
  beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale
}

# replot based on the scaled values.

matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors)
text(rep(-0, 9), beta.rescaled[9,], colnames(x), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)


###############################################################################
# Build a Lasso using Glmnet
###############################################################################


lassoreg.cv <- cv.glmnet(x,y, alpha=1)

# Plot the fit

plot(lassoreg.cv)

# calculate the minimum lambda

lassoreg.cv$lambda.min

# The lambda value is 0.0328

###############################################################################

# Compute the lasso predictions

lassofits <- glmnet(x,y,alpha=1, nlambda=100)

# plot the fits

plot(lassofits)
plot(lassofits, xvar = "lambda", label = TRUE)
plot(lassofits, xvar = "dev", label = TRUE)

# Calculate the predictions with the minimal lambda

lassopred <- predict(lassofits,x,s= lassoreg.cv$lambda.min)
# mydata$yhat <- predict(lassofits,x,s= lassoreg.cv$lambda.min)


# Here is how we find the coefficients for the Lasso.

lassocoef <- predict(lassofits,x,s=lassoreg.cv$lambda.min, type="coefficients")

lassocoef

# notice that the lcp was removed by the lasso (it has a value of 0)

# Lets calculate the prediction error.

sum1 =0
tt<-nrow(mydata) # testset

for (i in 1:tt){
  sum1 <- sum1 + (lassopred[i]-mydata[i,9])^2
}

sumg <- sum1/tt
sumg

# Therefore the error of the lasso is 0.46818


###############################################################################
# Build an elastic net using glmnet
###############################################################################

# alpha between 0 and 1 are examples of the elastic net.
# in this example we will use 0.2

elasticreg.cv <- cv.glmnet(x,y, alpha=0.2) 

# Plot the fit

plot(elasticreg.cv)


# determine the minimum lambda

elasticreg.cv$lambda.min


###############################################################################

# Compute the elastic net predictions

elasticfits <- glmnet(x,y,alpha=0.2, nlambda=100)


# plot the fits

plot(elasticfits)
plot(elasticfits, xvar = "lambda", label = TRUE)
plot(elasticfits, xvar = "dev", label = TRUE)

# Calculate the predictions with the minimal lambda


elasticpred <- predict(elasticfits,x,s= elasticreg.cv$lambda.min)
# mydata$elasticpred <- predict(elasticfits,x,s= elasticreg.cv$lambda.min)


# Here is how we find the coefficients for the Elastic.

elasticcoef <- predict(elasticfits,x,s=elasticreg.cv$lambda.min, type="coefficients")

elasticcoef

# notice that the lcp was removed by the elastic net (it has a value of 0)

# Lets calculate the prediction error.

sum1 =0
tt<-nrow(mydata) # testset

for (i in 1:tt){
  sum1 <- sum1 + (lassopred[i]-mydata[i,9])^2
}

sumg <- sum1/tt
sumg



###############################################################################
# Build a Lasso and Elastic Net using elasticnet package
###############################################################################

# Build a Lasso

obj.lasso <- enet(x,y, lambda=0)
plot(obj.lasso, use.color=TRUE)

# Build a Elastic Net

obj.enet <- enet(x,y, lambda=0.5)
plot(obj.enet, use.color=TRUE)


# How to choose a tuning parameter.

# for a sequence of lambda, find the s that is a minimzer of the CV prediction error
# and then find the lambda that minimizes the CV prediction error.
# note that we used 0.5 based on this chart.

obj.cv <- cv.enet(x,y, lambda=0.5, s=seq(0,1, length=100),
                  mode = "fraction", trace = FALSE, max.steps=80)



###############################################################################
# Notes
###############################################################################

library(elasticnet)

data(diabetes)
attach(diabetes)

object <- enet(x,y,lambda=0.1)

# make predictions in each value of X , at each steps produced in object.

fits <- predict.enet(object, x,type="fit")

## extract the coefficient vector with L1 norm = 2000

coef2000 <- predict(object, s=2000, type="coef", mode="norm")

### extract the coefficient vector with L1 norm fraction=0.45

coef.45 <- predict(object, s=0.45, type="coef", mode="fraction")

detach(diabetes)


