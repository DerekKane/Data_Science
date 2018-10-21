# Titanic Tutorial from Kaggle competition

# http://vimeo.com/69269984

#########################################################################

# Set the working directory for the analysis.

setwd("C:/Users/dkane/Documents/R Packages/Titanic Tutorial")

train = read.csv("train.csv")
attach(train)

head(train)

#########################################################################
# This section will extract the Mr., Mrs., etc... from the name column

library(reshape2)

name <- train$Name
names <- colsplit(Name, ", ", c("Last", "First"))
first <- names$First
titles <- colsplit(first, ". ", c("Title", "First"))
title <- titles$Title
fname <- titles$First
lname <- names$Last

# This combines the original table "train" with the title, last, & first names.

train <- data.frame(train, title, fname, lname)

#########################################################################

# We need to predict the age for different groups to impute the missing values.
# this code is looking at the fare, title, sibsp, and parch to find the most accurate imputation.

age.model=lm(Age~ Fare + as.factor(title) + SibSp + Parch)

# this is the routine which loops through the data and imputes the difference.

for(i in 1:nrow(train)){
  if(is.na(train[i, "Age"])){
    train[i, "Age"] = predict(age.model, newdata=train[i,])
  }
}

# write.csv(train, "train data with estimated age.csv")

#########################################################################

# Logistic Regression Model

Train = read.csv("train data with estimated age.csv")

attach(Train)

# model with some interactions

model = glm(Survived ~ Pclass + Fare + SibSp + Parch + 
              Sex + Age + Pclass:Sex + 
              Age:Sex + SibSp:Sex, family= binomial(link="logit"))

# + as.Factor(Title)
# modeling pclass as a factor has no bearing on the overall results.
# embarked reduced the model fit.

summary(model)

#########################################################################

# Run some predictions on the Train dataset from the model.

predict(model, newdata = Train)

# by default, the predict function gives the logit.
# to transform into a probability, do the following transformation:
# exp(predict(model, newdata = Train)) / 
#         (1 + exp(predict(model, newdata = Train)))

?predict.glm

# or specify the type = "response" function

predict(model, newdata=Train, type="response")

# compare the predictions with what you would expect from the data.

head(Train)

#########################################################################

# See how well you did in predicting the training dataset.

P = predict(model, newdata=Train, type="response")

p.survive = round(P)

# We will use the caret package to see how well the model is perfoming.

library("caret")

confusionMatrix(p.survive, Survived)

# we could also use a cross validation to determine the accuracy of the model
# your in sample prediction gives an overly optimistic estimate of your model
# accuracy because the model was estimated with that data.

#########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################################################################

# make predictions using the test set.

test.data= read.csv("test.csv")
attach(test.data)

#########################################################################
# This section will extract the Mr., Mrs., etc... from the name column

library(reshape2)

name <- test.data$Name
names <- colsplit(Name, ", ", c("Last", "First"))
first <- names$First
test.data1 <- colsplit(first, ". ", c("Title", "First"))
title2 <- test.data1$Title
fname <- test.data1$First
lname <- names$Last

# This combines the original table "train" with the title, last, & first names.

test.data <- data.frame(test.data, title2, fname, lname)

# replace the ms value with Miss for the Age imputation/conversion to work correctly 

test.data$title2[test.data$title2=="Ms"] <- "Miss"

#########################################################################


# We need to predict the age for different groups to impute the missing values.
# this code is looking at the fare, title, sibsp, and parch to find the most accurate imputation.

age.model1=lm(Age~ Fare + as.factor(title2) + SibSp + Parch)


for(i in 1:nrow(test.data)){
  if(is.na(test.data[i, "Age"])){
    test.data[i, "Age"] = predict(age.model1, newdata=test.data[i,])
  }
}

#########################################################################

# In the test dataset, the fare for the 153 observation is missing.
# estimate the fare as the mean of the third class.

test.data$Fare[153] = mean(with(test.data, subset(Fare, Pclass == 3)), na.rm= TRUE)

#########################################################################

# predictions for the test data set: if you want to specify a custom cutoff
# not recomended but can be useful at times.

predict(model, test.data)
p.survive = rep(NA, nrow(test.data))
for (i in 1:nrow(test.data )){
  P = predict(model, newdata= test.data[i, ], type = "response")
  # changing this will give you different numbers
  if(P<= 0.5){
    p.survive[i] = 0  
  }
  else{
    p.survive[i] = 1
  }
}

#########################################################################

# Here is the section for creating the final submission.
# we dont want to include the row id.

data = data.frame(PassengerId = 1:nrow(test.data), Survived = p.survive)
write.csv(data, "submission.csv", row.names = TRUE)


#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

# this is another technique that we can use in order to approach the 
# titanic dataset.

library(Amelia)

#... code for loading test and train data in a data frame
missmap(train, main = "Missingness Map Train")
missmap(test.data, main = "Missingness Map Test")


#########################################################################

library(caret)

# Here is Here is a snippet of code where i successively train a random forest and a 
# gradient boosting machine (GBM) using the same train function from caret.

forest.model1 <- train(survived ~ pclass + sex + title + sibsp +parch ,
                       data.train,
                       importance=TRUE)


