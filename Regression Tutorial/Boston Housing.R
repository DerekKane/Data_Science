############################################################################
# Multiple Linear Regression Example
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Regression Tutorial")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(MASS)

mydata <- Boston
attach(mydata)

# Load R Libraries for the analysis

library(aod)
library(ggplot2)
library(reshape2)
library(car)
library(corrplot)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Correlation Matrix

mcor<-cor(mydata)
round(mcor, digits=2)

# Corrplot library of correlations

corrplot(mcor)

# scatterplot for all variables

plot(mydata)

# Histogram of medv

hist(mydata$medv)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model building
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata$logmedv <- log(mydata$medv) 


reg1 <-lm(medv ~ indus + rm + tax + ptratio + lstat, data = mydata)

summary(reg1)


# revised model selection method.

# Forward Method
step(glm(logmedv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = mydata), direction="both")

reg2 <-lm(logmedv ~ rm + lstat + crim + zn + chas + dis, data = mydata)

summary(reg2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Regression Diagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

residualPlots(reg2)
# Here are some other diagnostic options. We can convert this as needed to address whatever scenario we need.

avPlots(reg2, id.n=2, id.cex=0.7)
qqPlot(reg2)
outlierTest(reg2)
influenceIndexPlot(reg2)
influencePlot(reg2)

ncvTest(reg2)

vif(reg2)
vif(reg4)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write Data File in xlsx
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(xlsx)
write.xlsx(mydata, "C:/Users/Derek/Documents/RPackages/Regression Tutorial/ModelExport.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
