### Penalized Logistic Regression for Attrition
# Model begins from Line 114
# Preparations for Train/validation/test samples are shown in Line 67
# You can skip the data processing part before Line 114

###########################
### Data Set Creation
### Data Set consists of base customers plus attrited customers
### Define base customers: Same cust_id from July to Sept.
###########################

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/kane.de/Documents/RPackages/Projects")
#-------------------------------------------------------

### Import abc data
month1Data <- read.csv("Month1.csv")   #1,684,813
month3Data <- read.csv("Month3.csv")   #1,662,690
baseCust  <- month3Data[month3Data$CUST_ID %in% month1Data$CUST_ID,]   #1,631,983 base customers from July to Sept 
baseCust  <- baseCust[,c(-7,-9,-61,-62,-97)] #remove education, job,region, region postal,bank code string
baseCust  <- as.data.frame(append(baseCust,list(Attrition=0),after=1))
write.csv (baseCust, file="Base Customer from July to Sept.csv") 
lostCust1 <- read.csv("attritionMonth2.csv")
lostCust1 <- subset(lostCust1, lostCust1$Attrition==1)   #28506
lostCust2 <- read.csv("attritionMonth3.csv")
lostCust2 <- subset(lostCust2, lostCust2$Attrition==1)   #24324
total_lostCust <- rbind(lostCust1,lostCust2)   #52830
total_lostCust <- total_lostCust[,c(-1,-9,-11,-63,-64,-99)]   #remove x, education, job,region, region postal,bank code string
#Attrition data
baseCust_revised <- read.csv("Base Customer from July to Sept.csv")  #added Attrition=0 in the second column
nrow(baseCust_revised)   #1,048,575 data is reduced due to short of memory
data <- rbind(baseCust_revised,total_lostCust)   #1,101,405
data <- data[sample(nrow(data)),]











##############################################################################################
### Data Processing
# To facilitate analysis. Take a random sample (100k) of data
data_sample <- data[sample(1:nrow(data), 100000,replace=FALSE),]
write.csv (data_sample,file="100k data_sample for attrition.csv")
#Create dummy for categorical variables
data_sample1 <- as.data.frame(append(data_sample,list(SEX_DUM=ifelse(data_sample$SEX=="M",0,1)),after=3))  # Male=0; Female=1 
data_sample1 <- as.data.frame(append(data_sample1,list(MARRIED_DUM=ifelse(data_sample1$MARRIED=="W",3,
                                                                          ifelse(data_sample1$MARRIED=="D",2,
                                                                          ifelse(data_sample1$MARRIED=="S",1,0)))),after=5))
data_sample1 <- as.data.frame(append(data_sample1,list(OWN_RENT_DUM=ifelse(data_sample1$OWN_RENT=="O",1,0)),after=15)) #Own=1; Rent=0

# Remove (SEX, MARRIED and OWN_RENT)
data_sample1 <- data_sample1[,c(-3,-5,-15)]
# Create new a variable: Prod_owned
data_sample1 <- as.data.frame(append(data_sample1,list(Prod_owned=NA),after=93))
data_sample1$Prod_owned <- rowSums(data_sample1[,c(20:30)])
write.csv (data_sample1,file="100k data_sample for attrition_v2.csv")





### Create train, validation, test sets in a 70%,10%,20% split 
# Create training set
library(caret)
set.seed(999999)
data <-data_sample1
split1 <- createDataPartition(data$Attrition, p=0.7)[[1]]  
other  <- data[-split1,]
training <- data[split1,]   #70,000
table(training$Attrition)      #event to non-event is not balanced
event_training    <- training[training$Attrition==1,]   #3409
event_sample      <- event_training[sample(1:nrow(event_training),35000, replace=TRUE),] #replace=TRUE for up sample
nonevent_training <- training[training$Attrition==0,]   #66591
nonevent_sample   <- nonevent_training[sample(1:nrow(nonevent_training),35000, replace=FALSE),]
train <- rbind(nonevent_sample,event_sample)   #70,000
table(train$Attrition)   #Now we have a balanced sample

# Create validation & testing sets
validation <- other[sample(1:nrow(other),10000,replace=FALSE),]   #10,000
table(validation$Attrition)   
test     <- other[!other$CUST_ID %in% validation$CUST_ID,]   #20,000
table(test$Attrition)    

### Centering and Scaling
# Centering and Scaling Predictors in train set
for(i in seq(3,94, by=1)) {
  train[,i] <- scale(train[,i])
}

# Centering and Scaling Predictors in validation set
for(i in seq(3,94, by=1)) {
  validation[,i] <- scale(validation[,i])
}

# Centering and Scaling Predictors in test set
for(i in seq(3,94, by=1)) {
  test[,i] <- scale(test[,i])
}
write.csv (train, file="predicting attrite cust training set.csv")
write.csv (validation, file="predicting attrite cust validation set.csv")
write.csv (test, file="predicting attrite cust testing set.csv")

#### End of Data Processing



#########################################
######################################### 
#Penalized LR model 
library(glmnet)


data <- data[,-1] #remove cust id
train <- train[,-1] #remove cust id
validation <- validation[,-1] #remove cust id
test <- test[,-1] #remove cust id
predictors <-names(data)[names(data)!="Attrition"]  # character vectors are created for x in glmnet


################################################################
############ Model Training

penLR1 <- glmnet(x=as.matrix(train[,predictors]),
                 y=train$Attrition, family="binomial")


#Compute predictions for different levels of regularization
train_prediction <-predict(penLR1, newx=as.matrix(train[,predictors]),
                           s=c(0.01,0.03,0.05,0.06,0.07,0.09,0.1), #s defines regularization(penalty) levels 
                           type="class")         
#Feature Selection
selected_predictor <- predict(penLR1, newx=as.matrix(train[,predictors]),
                              s=c(0.01,0.03,0.05,0.06,0.07,0.09,0.1),    #predictors are eliminated as s increases
                              type="nonzero")                       # The penalized term "s" is the tunning parameter!!

# estimated coefficents at different s
coefficents <- predict(penLR1, newx=as.matrix(train[,predictors]),
                       s=c(0.05,0.06,0.7,0.1,0.12),type="coefficients")  # Remember to check out coefficients!!
# Confusion Matrix
prediction_train <- predict(penLR1, newx=as.matrix(train[,predictors]),
                            s=0.06,type="class")  # s=0.06 gave me the optimized AUC for this dataset
                                                  # AUC will be calculated shortly
require (caret)
confusionMatrix(prediction_train, train$Attrition) 
#If you get factor level difference error, try smaller 's'
#Accuracy= 0.6451(@0.06)
#Sensitivity : 0.6277(@0.06)  
#Specificity : 0.6626(@0.06)

#ROC
library(ROCR)
train_roc_pred <- prediction(as.numeric(prediction_train), train$Attrition)
train_roc <- performance(train_roc_pred,"tpr","fpr")
plot(train_roc,col="green")
abline(c(0,1))

# calculate area under curve
auc_train <- performance(train_roc_pred,"auc")   
auc_train <- unlist(slot(auc_train, "y.values"))
auc_train <- round(auc_train,digits=3) 
auc_val   #0.645(@0.06)

# Display AUC
legend(0.4,0.2,c("Area under curve =",auc_train),border="white",cex=1,box.col = "white")


######## End of Model training step
# What to do next?
# The feature selection helps narrow down variables, but Model selection still need to be performed.






##############################################################################################################
############ Validation Step
# In this step, use the s value that gave you the best result from the training step (s=0.06 for my case)
# Confusion Matrix
prediction_val <- predict(penLR1, newx=as.matrix(validation[,predictors]),
                          s=0.06,type="class")  
require (caret)
confusionMatrix(prediction_val, validation$Attrition) 
#Accuracy= 0.5704
#Sensitivity : 0.56015  
#Specificity : 0.78065

#ROC
library(ROCR)
val_roc_pred <- prediction(as.numeric(prediction_val), validation$Attrition)
val_roc <- performance(val_roc_pred,"tpr","fpr")
plot(val_roc,col="green")
abline(c(0,1))

# calculate area under curve
auc_val <- performance(val_roc_pred,"auc")   
auc_val <- unlist(slot(auc_val, "y.values"))
auc_val <- round(auc_val,digits=3) 
auc_val   #AUC=0.67

# Display AUC
legend(0.4,0.2,c("Area under curve =",auc_val),border="white",cex=1,box.col = "white")

# Probability Cutoff vs TP vs FP 
cutoffs <- data.frame(cut=val_roc@alpha.values[[1]], fpr=val_roc@x.values[[1]], 
                      tpr=val_roc@y.values[[1]])
head(cutoffs)

# Filter search by defining fpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))   #define fpr value






################################################################
############ Test Step
# Use the same s value from training and validation step
# Confusion Matrix
prediction_test <- predict(penLR1, newx=as.matrix(test[,predictors]),
                           s=0.06,type="class")  
require (caret)
confusionMatrix(prediction_test, test$Attrition) 
#Accuracy= 0.5718
#Sensitivity : 0.5623  
#Specificity : 0.7637

#ROC
library(ROCR)
test_roc_pred <- prediction(as.numeric(prediction_test), test$Attrition)
test_roc <- performance(test_roc_pred,"tpr","fpr")
plot(test_roc,col="green")
abline(c(0,1))

# calculate area under curve
auc_test <- performance(test_roc_pred,"auc")   
auc_test <- unlist(slot(auc_test, "y.values"))
auc_test <- round(auc_test,digits=3) 
auc_test  #AUC=0.663

# Display AUC
legend(0.4,0.2,c("Area under curve =",auc_test),border="white",cex=1,box.col = "white")

cutoffs <- data.frame(cut=test_roc@alpha.values[[0.1]], fpr=test_roc@x.values[[0.1]], 
                      tpr=test_roc@y.values[[0.1]])
head(cutoffs)
# Filter search by defining fpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))


#### THE END ####






