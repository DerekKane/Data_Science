# STAT 849, Theory and Applied Regression and ANOVA I
#  Fifteenth discussion, 12-14-2012
#  TA: Guilherme Ludwig
#
# Do yourself a favor and buy this book (Amazon has it on sale every now and then):
# http://www-stat.stanford.edu/~tibs/ElemStatLearn/
#
# The following dataset is from Hastie, Tibshirani and Friedman (2009), from a study 
# by Stamey et al. (1989) of prostate cancer, measuring the correlation between the level 
# of a prostate-specific antigen and some covariates. The covariates are
#
# * lcavol  : log-cancer volume
# * lweight : log-prostate weight
# * age     : age of patient
# * lbhp    : log-amount of benign hyperplasia
# * svi     : seminal vesicle invasion
# * lcp     : log-capsular penetration
# * gleason : Gleason Score, check http://en.wikipedia.org/wiki/Gleason_Grading_System
# * pgg45   : percent of Gleason scores 4 or 5
#
# And lpsa is the response variable, log-psa.

url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
str(pcancer <- read.table(url, header=TRUE))

# There's a training sub-dataset that we will focus on. Later, we will try to predict
# the values of the remaining observations.

train <- pcancer[which(pcancer$train),1:9]
calibrate <- pcancer[-which(pcancer$train),1:9]

# The data looks like this

plot(train)

# Of course, given that this is a biological dataset, the covariates are strongly related

round(cor(train),3)

# We fit a linear model and now focus on fixing multicollinearity

model.ls <- lm(lpsa ~ ., data=train)
rss.ls <- sum(model.ls$resid^2)/model.ls$df.residual

# #######################
# # STEPWISE REGRESSION # 
# #######################
#
# This method was shown weeks ago, but I'll reproduce it here for 
# comparison. 

model.backward <- step(model.ls, direction="backward")
rss.backward <- sum(model.backward$resid^2)/model.backward$df.residual

# So backward selection using AIC drops "gleason" from the model. The final AIC
# is -39.103.
#
# Note that for scope, the formula ~. means "as current model". Forward selection
# is poorly explained in R... 

scope <- list(upper=~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, lower=~.)
model.forward <- step(lm(lpsa ~ 1, data=train), scope, direction="forward")
rss.forward <- sum(model.forward$resid^2)/model.forward$df.residual

# So we conclude that forward selection using AIC keeps lcavol, lweight, svi and lbph.
# The AIC is -37.825.
#
# Note that the paths are arbitrary and such model selection procedures are called 
# "myopic" sometimes, unlike Lasso Regression which has the "Oracle" property. For example,
# we may compute r^2 for all possible subsets.

r2 <- list()
AICs <- list()
for(i in 1:8){
  indexes <- combn(8,i)
  currentr2 <- NULL
  currentAIC <- NULL
  for(j in 1:dim(indexes)[2]){
    temp.model <- lm(lpsa ~ ., data=train[,c(indexes[,j], 9)])
    currentr2[j] <- summary(temp.model)$r.squared
    currentAIC[j] <- AIC(temp.model)
  }
  r2[[i]] <- currentr2
  AICs[[i]] <- currentAIC
}

# let me find the corresponding r^2 and AIC entries for the paths chosen by
# backward and forward elimination... this code is a bit clumsy but it gets
# what we need.

compare <- function(set){
  s <- length(set)
  temp <- combn(8,s)
  check <- NULL
  for(i in 1:dim(temp)[2]){
    check[i] <- all(temp[,i]==set)
  }
  return(which(check))
}

backward <- compare(c(1:6,8))
forward <- c(compare(1), compare(1:2), compare(c(1,2,5)), compare(c(1,2,4,5)))

r2.b <- c(r2[[7]][backward], r2[[8]])
r2.f <- c(r2[[1]][forward[1]], r2[[2]][forward[2]], r2[[3]][forward[3]], r2[[4]][forward[4]])
AICs.b <- c(AICs[[7]][backward], AICs[[8]])
AICs.f <- c(AICs[[1]][forward[1]], AICs[[2]][forward[2]], AICs[[3]][forward[3]], AICs[[4]][forward[4]])

# We now can take a look at how backward/forward performs!

x11(width=10, height=5)
layout(matrix(1:2, ncol=2))
plot(0, xlim=c(0,9), ylim=c(0,0.8), type="n", ylab=expression(r^2), main="Fitting criteria")
for(i in 1:8){
  points(rep(i, length(r2[[i]])), r2[[i]], pch=21, bg="Grey")
}
points(7:8, r2.b, bg="Red", col="Red", pch=21, type="o")
points(1:4, r2.f, bg="Blue", col="Blue", pch=21, type="o")
plot(0, xlim=c(0,9), ylim=c(153,217), type="n", ylab="AIC", main="AIC")
for(i in 1:8){
  points(rep(i, length(AICs[[i]])), AICs[[i]], pch=21, bg="Grey")
}
points(7:8, AICs.b, bg="Red", col="Red", pch=21, type="o")
points(1:4, AICs.f, bg="Blue", col="Blue", pch=21, type="o")

# ####################
# # RIDGE REGRESSION # 
# ####################
#
# This method was shown in the previous discussion, but I'll reproduce it here for 
# comparison. 

library(car)
model.ridge <- lm.ridge(lpsa ~ ., data=train, lambda = seq(0,10,0.1))

plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]

# We can plot the coefficients and see how they vary as a function of lambda

colors <- rainbow(8)

matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train)[-9], pos=4, col=colors)

beta.ridge <- coef(model.ridge)[which.min(model.ridge$GCV),]
resid.ridge <- train$lpsa - beta.ridge[1] - as.matrix(train[,1:8])%*%beta.ridge[2:9]

# To find df
d <- svd(as.matrix(train[,1:8]))$d
df <- 67 - sum(d^2/(lambda.ridge+d^2))

rss.ridge <- sum(resid.ridge^2)/df

# ####################
# # LASSO REGRESSION # 
# ####################
#
# In Lasso Regression, the coefficients are penalized by the L1 norm. The 
# optimal value for lambda is chosen by cross-validation.
#
# If necessary,
# install.packages("lars")

library(lars)
y <- as.numeric(train[,9])
x <- as.matrix(train[,1:8])
model.lasso <- lars(x, y, type="lasso")
lambda.lasso <- c(model.lasso$lambda,0)
beta <- coef(model.lasso)

# If you want fancier colors, try
#
# library(colorspace)
# colors <- rainbow_hcl(8, c = 65, l = 65)
#
# I'm not kidding, presenting your data clearly very important.

colors <- rainbow(8)

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

matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors)
  text(rep(-0, 9), beta.rescaled[9,], colnames(x), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

# I'll keep the lambda=1.7305 betas

beta.lasso <- beta[4,]
resid.lasso <- train$lpsa - predict(model.lasso, as.matrix(train[,1:8]), s=4, type="fit")$fit
rss.lasso <- sum(resid.lasso^2)/(67-4)

# #########################
# # PARTIAL LEAST SQUARES #
# #########################
#
# Partial Least Squares is pretty much like principal components regression, but
# we use information from Y to select weights for the principal components of X.


library(pls)
model.pls <- plsr(lpsa ~ ., 8, data = train, method = "oscorespls", validation = "CV")
summary(model.pls)

# I'm eyeballing CV here, but fitting 4 components should be enough. So I'll update
# the model

model.pls <- plsr(lpsa ~ .,4, data = train, method = "oscorespls")
summary(model.pls)

beta.pls <- drop(coef(model.pls))
resid.pls <- drop(model.pls$resid)[,4]
rss.pls <- sum(resid.pls^2)/(67-4)

# #########################
# # COMPARISON OF FITTING #
# #########################
#
# This is as straightforward as it gets:

rss.ls
rss.backward
rss.forward
rss.ridge
rss.lasso
rss.pls

# ############################
# # COMPARISON OF PREDICTION #
# ############################
#
# We can also compare with the predicition dataset we saved from before. In this case

y.new <- calibrate$lpsa

pss.ls <- sum((y.new - predict(model.ls, calibrate[,1:8]))^2)
pss.backward <- sum((y.new - predict(model.backward, calibrate[,1:8]))^2)
pss.forward <- sum((y.new - predict(model.forward, calibrate[,1:8]))^2)
pss.ridge <- sum((y.new - beta.ridge[1] - as.matrix(calibrate[,1:8])%*%beta.ridge[2:9])^2)
pss.lasso <- sum((y.new - predict(model.lasso, as.matrix(calibrate[,1:8]), s=4, type="fit")$fit)^2)
pss.pls <- sum((y.new - drop(predict(model.pls, calibrate[,1:8], 4)))^2)

pss.ls
pss.backward
pss.forward
pss.ridge
pss.lasso
pss.pls

# In this case Forward AIC-stepwise Regression did the best job at predicting, followed by Lasso, then 
# Ridge regression.
#
# ##################
# # FINAL PROJECT! #
# ##################
#
# Remember:
#
# * Provide means of model comparison (e.g. Prediction Error), and be careful to rescale the response to 
#   its original unit.
#
# * Provide confidence intervals for the parameters, use bootstrap when unsure of normality.
#
# * If you make a claim, provide graphics and tables to justify it. 
#
# * Don't show unnecessary code.
#
# * Label your graphics properly, make axes comparable, use color when possible.
#
# * Reference and explain graphics in text, don't throw graphics and expect the reader to know what
#   they're for.
#
# * Try to use graphics instead of tables when dimensions of the tables are too large. On the other hand
#   don't use graphics to summarize simple things (e.g. no pie-charts of sex showing male 48% female 52%),
#   use tables or explain it in text.
#
# * Only show as much digits as you need to compare things, depending on scale.
#
# * Write down your model and assumptions. Explain the covariates. Justify your choice of methods.
#
# * Anticipate the reader questions when writing. The presentation will give you some idea of what 
#   those questions can be.
