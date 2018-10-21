# Running a saved model - Automation example

########################################################################

# We will first create the model to be saved and run later

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set the working directory for the analysis.

setwd("C:/Users/dkane/Documents/R Packages/Marion County")

# Import the data from csv file

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Marion County/recidivism4e2.csv")
attach(mydata)


########################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]

########################################################################

# This will build the model we will want use for later.

mylogit <- glm(FTA ~ FelonyIndicator + PersonalCharge + ChargeClass + DrugCharge + 
                 Gender + PropertyCharge + NumofMisdmr + Age + 
                 NumofCaseCharge, data = trainData, family = "binomial")


setwd("C:/Users/dkane/Documents/R Packages/Automation Example")

# Save the model to a file

save(mylogit, file = "my_model1.rda")

########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########################################################################

# This next section will then describe an event where we are a month 
# later and we have new data available.

setwd("C:/Users/dkane/Documents/R Packages/Marion County")

# Import the data from csv file

mydata<- read.csv("C:/Users/dkane/Documents/R Packages/Marion County/recidivism4e2.csv")
attach(mydata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We then call up the saved script and execute the model.

setwd("C:/Users/dkane/Documents/R Packages/Automation Example")

## load the model
load("my_model1.rda")

## Run the model on the new mydata dataset.

mydata$FTAProbability <- predict(mylogit, newdata = mydata, type = "response")
