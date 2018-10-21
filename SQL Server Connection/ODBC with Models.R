#######################################################################
# ABC Bank Corporation - Customer Attrition Analysis
#######################################################################

# Set Working Directory.

setwd("C:/Users/derek/Documents/R Scripts/Northwestern")

# Load Libraries to fetch the data.

library(RODBC)
library(RMySQL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MySQL Connection Section
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connect to MySQL and bring in the abc_clustercustomer dataset.

# MySQLServer = dbConnect(MySQL(), user='root', password='1234', dbname='datascience', host='localhost')

# Save as a results set
# mydata <- dbReadTable(MySQLServer, "abc_clustercustomer")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SQL Server Connection Section
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connect to SQL Server 2014 and bring in the abc_clustercustomer dataset.

SQLServerDS <- odbcConnect(dsn="SQLServer - DataScience", uid="derek", pwd="xxxxxx")


# Save as a results set
mydata1 <- sqlFetch(SQLServerDS, "abc_attrition")

# Load Libraries for Analysis

library(caret)
library(rpart)
library(C50)
library(rattle)
library(party)
library(partykit)
library(randomForest)
library(ROCR)
library(ggplot2)
library(reshape2)
library(car)
library(corrplot)
library(e1071)


##################################################################
# Data Munging
##################################################################

# Reencode variables to get into numeric form.

mydata1$Attrition <- as.factor(mydata1$Attrition)

# mydata$SEX <- as.factor(mydata$SEX)
# mydata$MARRIED <- as.factor(mydata$MARRIED)
# mydata$EDUCATION <- as.factor(mydata$EDUCATION)
# mydata$JOB <- as.factor(mydata$JOB)
# mydata$REGION <- as.factor(mydata$REGION)
# mydata$OWN_RENT <- as.factor(mydata$OWN_RENT)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Take a Random Sample of the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Random sample of 100,000 rows.

mydata <- mydata1[sample(nrow(mydata1), 100000), ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop unneeded variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# mydata$CUST_ID <- NULL

mydata$SEX <- NULL
mydata$MARRIED <- NULL

# mydata$EDUCATION <- NULL
# mydata$JOB <- NULL
# mydata$REGION <- NULL
# mydata$OWN_RENT <- NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handling of missing values.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This will remove any observations that are missing from the dataset (NULL)  

na.omit(mydata)


##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData1 <- mydata[ind==1,]
testData1 <- mydata[ind==2,]

#################################################################################
# Parallel R Model Building
#################################################################################

# set up the packages for parallel processing.

library(foreach)  
library(doSNOW)  

cl<-makeCluster(4) #change the 4 to your number of CPU cores  
registerDoSNOW(cl) 

# Now, we have the groundwork for our parallel foreach loop, 
# but we need to find a way to split the data up in order to perform predictions on small sets of data in parallel.

# This will create a numeric vector that can be 
# used to split the trainData data frame into 4 parts.

num_splits<-4  
split_testing<-sort(rank(1:nrow(trainData))%%4)

# I suggest setting num_splits to some multiple of your number of
# CPU cores in order to execute the below foreach loop as quickly as possible. 

# Now that we have a way to split the data up, we can go ahead
# and create a loop that will generate predictions in parallel.

# Here is the code for launching a SVM in parallel.

svm_predictions<-foreach(i=unique(split_testing),  
                         .combine=c,.packages=c("e1071")) %dopar% {  
                           as.numeric(predict(svm_fit,newdata=testing[split_testing==i,]))  
                         }  
stopCluster(c1)  

# http://www.vikparuchuri.com/blog/parallel-r-model-prediction-building/

#################################################################################
# Model 1. - Random Forest Model on Random Subset Model
#################################################################################

trainData1$CUST_ID <- NULL
testData1$CUST_ID <- NULL

RandomForestModel <- randomForest(y=trainData1$Attrition, x=trainData1[,-47], ntree=100, mtry=7, importance=TRUE)

print(RandomForestModel)
importance(RandomForestModel)


plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=34, col="blue")

plot.new()
varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

# Variables to keep from the model

var <- c("BAL_SAVINGS","INCOME", "BAL_RETIREMENT", "AGE",
         "YOJ", "BAL_CREDIT_CARD", "BAL_HELOC", "Attrition")

newdf <- trainData[,var]


###############################################################################
# 2. Penalized Regression using Lasso.
###############################################################################

# install.packages("elasticnet")

library(glmnet)
library(lasso2)
library(colorspace)
library(lars)
library(elasticnet)

##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

mydata1$SEX <- NULL
mydata1$MARRIED <- NULL

set.seed(1234)
ind <- sample(2, nrow(mydata1), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata1[ind==1,]
testData <- mydata1[ind==2,]



# Convert the dataset into a matrix for further processing.

y <- as.numeric(trainData[,48])
x <- as.matrix(trainData[,2:47])

x1 <- as.matrix(testData[,2:47])

# y <- as.factor(newdf[,8])
# x <- as.matrix(newdf[,1:7])



###############################################################################
# Compute the lasso predictions for Logistic Regression
###############################################################################

lassofits <- glmnet(x,y,alpha=1, family='binomial', standardize = TRUE)
lassoreg.cv <- cv.glmnet(x,y, alpha=1, nfolds=10)

# plot the fits

plot(lassofits)
plot(lassofits, xvar = "lambda", label = TRUE)
plot(lassofits, xvar = "dev", label = TRUE)


coef(lassofits)[,10]

# Calculate the predictions to a new variable

lassopred <- predict(lassofits,x1,type="response", s=lassoreg.cv$lambda.min)

# Append the prediction results back to the main dataset

testData$YHat <- c(lassopred)


# trainData$yhat <- predict(lassofits,x,type="response")
# summary(trainData$yhat)

# Here is how we find the coefficients for the Lasso.
# lassocoef <- predict(lassofits,x,s=lassoreg.cv$lambda.min, type="coefficients")
# lassocoef

# plot.new()
# hist(testData$YHat)

#################################################################################
# Evaluate the primary models performance
#################################################################################

# These are threshold parameter setting controls.

Predict <- function(t) ifelse(lassopred > t , 1,0) #t is the threshold for which the confusion

confusionMatrix(Predict(0.01), testData$Attrition) # LASSO


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for unpruned model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pred1 <- prediction(testData$YHat, testData$Attrition)

perf <- performance(pred1, "tpr", "fpr" )

plot.new()
plot(perf, col = "green", lwd = 2.5)
abline(0,1,col="Red", lwd=2.5, lty = 2)

title('ROC Curve')
legend(0.8,0.4,c("Logistic","SVM","RF"),        
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1.4,1.4,1.4),col=c("green","blue", "orange", "yellow")) # gives the legend lines the correct color and width

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# AUC calculation metrics

fit.auc1 <- performance(pred1, "auc")
fit.auc1 # Logistic regression - AUC 0.5253




#################################################################################
# Save the model to a file
#################################################################################

save(lassofits, file = "AttritionModel.rda")
save(lassoreg.cv, file = "LASSO_CV.rda")


#################################################################################
# Write the Dataframe back to SQL. 
#################################################################################

abc_r_attrition <- as.data.frame(mydata1b[, c(8,9)])

write.table(abc_r_attrition,"C:\\Users\\derek\\Documents\\R Scripts\\Northwestern\\abc_r_cluster.txt",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE,append=FALSE);

sqlQuery(SQLServerDS,"BULK
         INSERT DataScience.dbo.abc_r_attrition
         FROM 'C:\\Users\\derek\\Documents\\R Scripts\\Northwestern\\abc_r_attrition.txt'
         WITH
         (
         FIELDTERMINATOR = ',',
         ROWTERMINATOR = '\\n'
         )");


# Close the connection to the SQL server

odbcClose(SQLServerDS)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Appendix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#################################################################################
# Model 1. - Logistic Regression Model
#################################################################################

# Select the variables to use based off of the forward selection procedure.
# Lower AIC indicates a better model.

# Forward Elimination

forwardtest <- step(glm(Churn~1, data = trainData), direction="forward", scope= ~ Accountlength + InternationalPlan + VoiceMail + NumVoiceMail + TotDayMin + TotDayCall + 
                      TotDayCharge + TotEveMin + TotEveCall + TotEveCharge + TotNightMin + TotNightCall + 
                      TotNightCharge + TotIntlMin + TotIntlCall + TotIntlCharge + NumCustServCall)

# capture.output(forwardtest,file="test2b.doc")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is the logistic regression model based off of the results.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mylogit <- glm(Attrition ~ CHILDREN + YOJ + INCOME + TIME_AT_ADDRESS, data = trainData, family = "binomial")

summary(mylogit)

# capture.output(summary(mylogit),file="test2a.doc")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this section will evaluate the models fit and performance.

# influence Plot
influenceIndexPlot(mylogit, vars=c("Cook", "hat"), id.n=3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

## Put the coefficients and CI in a format onto a useful scale.

exp(mylogit$coefficients)
exp(confint(mylogit))

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#################################################################################
# Model 2. - Support Vector Machines
#################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a SVM from the tuned parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SVMModel <- svm(as.factor(Attrition) ~., data = trainData, gamma=0.1, cost=1)

print(SVMModel)
summary(SVMModel)



#################################################################################
# Model 3. - Random Forest Model
#################################################################################

trainData$CUST_ID <- NULL


RandomForestModel <- randomForest(y=trainData$Attrition, x=trainData[,-47], ntree=1000, mtry=5, importance=TRUE)

print(RandomForestModel)
importance(RandomForestModel)


plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=34, col="blue")

plot.new()
varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")

###################################################################
# Knowledge Discovery: Build a decision tree using C5.0 for Churn
###################################################################

# The decision variable class must be converted into a factor
# variable in order for the C50 to process correctly.

mydata$Churn <- as.factor(mydata$Churn)

# Run the c50 algorithm for a decision tree.

c50_tree_result<-C5.0(Churn~.,data=mydata)

# display the summary

summary(c50_tree_result)

C5imp(c50_tree_result,metric='usage')
C5imp(c50_tree_result,metric='splits')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the c50 algorithm and show the decision rules.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c50_rule_result<-C5.0(Churn~.,data=mydata, rules=TRUE)

# display the summary

summary(c50_rule_result)

# Rule 7, Rule 9, & Rule 13 

#################################################################################
# Evaluate the primary models performance
#################################################################################

# this is the function which creates the prediction onto the new dataset.

LogisticModel <- predict(mylogit, testData, type="response")
SVMResult <- predict(SVMModel, testData, type="response")
RFResult <- predict(RandomForestModel, testData, type="response")

# This will create the results as a new column on the dataset.

testData$YHat1 <- predict(mylogit, testData, type="response")
testData$YHat2 <- predict(SVMModel, testData, type="response")
testData$YHat3 <- predict(RandomForestModel, testData, type="response")

# These are threshold parameter setting controls.

Predict <- function(t) ifelse(LogisticModel > t , 1,0) #t is the threshold for which the confusion
Predict2 <- function(t) ifelse(SVMResult > t , 1,0) #t is the threshold for which the confusion
Predict3 <- function(t) ifelse(RFResult > t , 1,0) #t is the threshold for which the confusion


confusionMatrix(Predict(0.03), testData$Attrition) # Logistic Regression
confusionMatrix(Predict2(0.5), testData$Churn) # SVM
confusionMatrix(Predict3(0.5), testData$Churn) # Random Forest


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for unpruned model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pred1 <- prediction(testData$YHat1, testData$Churn)
pred2 <- prediction(testData$YHat2, testData$Churn)
pred3 <- prediction(testData$YHat3, testData$Churn)

perf <- performance(pred1, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")
perf3 <- performance(pred3, "tpr", "fpr")

plot.new()
plot(perf, col = "green", lwd = 2.5)
plot(perf2, add = TRUE, col = "blue", lwd = 2.5)
plot(perf3, add = TRUE, col = "orange", lwd = 2.5)
abline(0,1,col="Red", lwd=2.5, lty = 2)

title('ROC Curve')
legend(0.8,0.4,c("Logistic","SVM","RF"),        
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1.4,1.4,1.4),col=c("green","blue", "orange", "yellow")) # gives the legend lines the correct color and width

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# AUC calculation metrics

fit.auc1 <- performance(pred1, "auc")
fit.auc2 <- performance(pred2, "auc")
fit.auc3 <- performance(pred3, "auc")


fit.auc1 # Logistic regression - AUC 0.69
fit.auc2 # SVM - AUC 0.93
fit.auc3 # Random Forest - AUC 0.94

#################################################################################
# Save the model to a file
#################################################################################

save(RandomForestModel, file = "ChurnModel.rda")
