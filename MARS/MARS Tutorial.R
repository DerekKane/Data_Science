############################################################################
# Multivariate Adaptive Regresssion Splines Tutorial (MARS)
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/MARS")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(MASS)
library(earth)
data(Boston)

mydata <- Boston

mydata$logmedv <- log(mydata$medv) 


############################################################################
# Build a MARS model
############################################################################

# These variables are from the housing tutorial 

model <- earth(logmedv ~ rm + lstat + crim + zn + chas + dis, data=mydata)

summary(model)

# variable importance
evimp(model, trim=TRUE, sqrt.=TRUE)

# plot an eearth object
plot(model)


############################################################################
# Predict from the model
############################################################################

mydata$predict <- predict(model)












############################################################################

#  Example from:
# http://www.r-bloggers.com/fit-and-visualize-a-mars-model/


library(MASS)
library(mda)
data(Boston)

# FIT AN ADDITIVE MARS MODEL
mars.fit <- mars(Boston[, -14], Boston[14], degree = 1, prune = TRUE, forward.step = TRUE)

# SHOW CUT POINTS OF MARS
cuts <- mars.fit$cuts[mars.fit$selected.terms, ]
dimnames(cuts) <- list(NULL, names(Boston)[-14])
print(cuts)

factor <- mars.fit$factor[mars.fit$selected.terms, ]
dimnames(factor) <- list(NULL, names(Boston)[-14])
print(factor)


# EXAMINE THE FITTED FUNCTION BETWEEN EACH IV AND DV

par(mfrow = c(3, 5), mar=c(2, 2, 2, 2), pty="s")
for (i in 1:13)
{
  xp <- matrix(sapply(Boston[1:13], mean), nrow(Boston), ncol(Boston) - 1, byrow = TRUE)
  xr <- sapply(Boston, range)
  xp[, i] <- seq(xr[1, i], xr[2, i], len=nrow(Boston))
  xf <- predict(mars.fit, xp)
  plot(xp[, i], xf, xlab = names(Boston)[i], ylab = "", type = "l")
}


                 

