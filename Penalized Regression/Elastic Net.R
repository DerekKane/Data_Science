# Lasso Tutorial

# http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
# http://web.stanford.edu/~hastie/TALKS/enet_talk.pdf


# install.packages("glmnet")

library(glmnet)
`%ni%`<-Negate(`%ni%')

data(mtcars)

x<-model.matrix(mpg~.,data=mtcars)
x=x[,-1]

glmnet1<-cv.glmnet(x=x,y=mtcars$mpg,type.measure='mse',nfolds=5,alpha=.5)

c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
