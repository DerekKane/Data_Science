#################################################################
# Statistical Evaluation - Cluster Analysis
#################################################################

setwd("C:/Users/dkane/Documents/R Scripts/Statistics/") 

library(caret)
library(ggplot2)



#################################################################
# Load the data
#################################################################

# Import the Iris dataset 

mydata <- iris

mydata$Species <- NULL


#################################################################################
# Density Plots
#################################################################################

plot.new()

plot(density(mydata$Sepal.Width, col="red", ylim=c(0,3.5), xlim=c(-1,2)))
lines (density(mydata$Sepal.Length), col="green")
lines (density(mydata$Petal.Length), col="blue")


# This code will generate the axis lengths dynamically

ranges <-  apply(mydata, 2,
                 function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })

plot(density(mydata$Sepal.Width), col="red",
     xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ])) 

lines(density(mydata$Sepal.Length), col="green") 
lines(density(mydata$Petal.Length), col="blue")


# Here is the density plot for each species type against the population as a whole.

plot.new()

plot(density(mydata$Sepal.Width, col="red", ylim=c(0,3.5), xlim=c(-1,2)))
lines(density(mydata$Sepal.Width[mydata$Species=='versicolor']), col="green")



######################################################################################
# Plot Using GGplot2
######################################################################################

plot.new()

plotexample <-ggplot(mydata, aes(x=mydata$Sepal.Width)) + geom_density(alpha=0.3)

plotexample


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create 2 new datasets with versicolor and all species

allspecies <- mydata
allspecies$Species <- 'Population' 

versdata <- subset(mydata, mydata$Species=='versicolor')

newdata <- rbind(versdata, allspecies)


# plot using ggplot2

plot.new()

plotexample <-ggplot(newdata, aes(x=newdata$Sepal.Width, fill=newdata$Species)) + geom_density(alpha=0.3)

plotexample 

########################################################################################
# Function to automate the evaluation
########################################################################################













########################################################################################
########################################################################################
# Test Section
########################################################################################
########################################################################################


plot.new()

plotexample2 <-ggplot(mydata, aes(x=mydata$Sepal.Width, fill=mydata$Species)) + geom_density(alpha=0.3)

plotexample2 



p <- ggplot() +
  # blue plot
  geom_point(data=visual1, aes(x=ISSUE_DATE, y=COUNTED)) + 
  geom_smooth(data=visual1, aes(x=ISSUE_DATE, y=COUNTED), fill="blue",
              colour="darkblue", size=1) +
  # red plot
  geom_point(data=visual2, aes(x=ISSUE_DATE, y=COUNTED)) + 
  geom_smooth(data=visual2, aes(x=ISSUE_DATE, y=COUNTED), fill="red",
              colour="red", size=1)







subset(mydata, mydata$Species=='versicolor')


with(mydata[mydata$Species=='versicolor',], lines(density(mydata$Sepal.Width), col="green"))


# GGPLOT2 Tutorial




library(ggplot2)
library(gridExtra)
set.seed(10005)

xvar <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
yvar <- c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
zvar <- as.factor(c(rep(1, 1500), rep(2, 1500)))
xy <- data.frame(xvar, yvar, zvar)



p1 <- ggplot(data = mydata, aes(x=Sepal.Width))+
  geom_density() +
  # Change the fill colour to differentiate it
  geom_density(data=hh2005, fill="purple") +
  labs(title = "Distribution of income for 2010")+
  labs(y="Density")+
  labs(x="Household Income")



p3
p4<-ggplot(xy, aes(xvar) + aes(xvar, fill = zvar)) + geom_density(alpha = 0.2)
