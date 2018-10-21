# This is how to set a working directory in R.

setwd ("C:/Users/Derek/Documents/RPackages/Predictive Model Example")

# install.packages('XLConnect')

# Load the excel file into R
library(XLConnect)

require(XLConnect)
wb = loadWorkbook("Iris Data.xlsx")
mydata = readWorksheet(wb, sheet = "Sheet1", header = TRUE)

mydata2 <- mydata
mydata2$Classification <- NULL

# print first 10 rows of mydata
head(mydata, n=10)

# Load Decision Tree package
# install.packages('party')

str(mydata)

# Correlation Matrix

mcor<-cor(mydata2)
round(mcor, digits=2)

# Corrplot library of correlations
library(aod)
library(ggplot2)
library(reshape2)
library(car)
library(corrplot)

corrplot(mcor)

# Heatmap of correlations

qplot(x=Var1, y=Var2, data=melt(cor(mydata2, use="p")), 
      fill=value, geom="tile") +   scale_fill_gradient2(limits=c(-1, 1))

# scatterplot for all variables

plot(mydata2)
plot(mydata2$PW ~mydata2$SL)

plotmatrix(mydata2, colour="turquoise2") + geom_smooth(method="lm")


# Extend the regression lines beyond the domain of the data
ggplot(mydata, aes(x=mydata$PW, y=mydata$SL, color=mydata$Classification)) + geom_point(shape=5) +
  scale_colour_hue(l=50)
  + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=T) # Extend regression lines



# Define the formula for the decision tree.

frmla = Classification ~ SL + SW + PL + PW

library(party)

df_ctree <- ctree(frmla, data=df)