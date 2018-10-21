# Outlier Detection Tutorial in R
###################################################################

library(car)

mydata <- Prestige
mydata

R <-lm(prestige ~ education + log2(income) + women, data=mydata)

summary(R)

###################################################################
# this will show a single plot at a time

plot(R)


# This will put the following 4 graphs into a single data frame
par(mfrow=c(2,2))
plot(R)

# this will show the cooks distance

cooks.distance(R)

# this will apply the cooks distance to the dataset under variable Cook

mydata$Cook <- cooks.distance(R)
mydata

# this will compute the Standardized Residuals to the dataset
# Residuals greater than +/- 2 are considered to be outliers.

mydata$StdResidual <- rstandard(R)

# This opens up a seperate window so we can manually identify points
# by clicking on them.

windows()
with(mydata, plot(income, cooks.distance(R)))

# here is the command that turns on the clicking function.

identify(mydata$income, cooks.distance(R))

#############################################################
# this will plot the residuals into a chart and add a line at 0.

plot(mydata$income, mydata$StdResidual,ylab="Standardized Residuals",xlab="Income",main="Prestige Graph by Income") 

abline(0, 0)                  # the horizon 

# This allows for us to identify the outlier points in this chart as well.

identify(mydata$income, mydata$StdResidual)

###############################################################

# This is how to create a subset of the data. 
# We will use StdResidual less than 2 and greater than -2.

mydata_sub <-subset(mydata, StdResidual<2.0 & StdResidual>-2.0)

# If we want to look at a single value like row.names equals 'chemist"
# this ihow we do it: row.names==chemists

mydata_sub

###############################################################

# Lets make some values equal to NA to remove them from the dataset
# This command will replace the values in Income greater than $20,000
# with a value that is called NA.

mydata_sub$income[mydata_sub$income>20000] <- NA


# This will remove all NA from the dataset. Ex. Income and prof

mydata_clean<-na.omit(mydata_sub)

# This will select all NA from the dataset in the income column

mydata_clean<-subset(mydata_sub, is.na(income))

# This will reshape the dataset to only select those values with a 
# proper income that is not NA, leaving all other NA in tact 

mydata_clean<- mydata_sub[!is.na(mydata_sub$income),]