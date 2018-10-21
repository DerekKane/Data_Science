# Polynomial Regression
#################################################################

install.packages("rrcov")
library(rrcov)

data(fish)

mydata <- fish

mydata$Length = log(mydata$Length3)


fit <- lm(Weight~poly(Length3, degree=2), data=mydata)

summary(fit)

plot(mydata$Weight~mydata$Length)

#################################################################

new.data <- data.frame(length=seq(25,60,len=100))

y=predict(fit,newdata=newdata)

lines(newdata$len,y)