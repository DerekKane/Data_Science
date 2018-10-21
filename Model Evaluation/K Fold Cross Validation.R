##################################################################
# K Fold Cross Validation on Iris Data
##################################################################

# original example from Digg Data website (Takashi J. OZAKI, Ph. D.) 
# http://diggdata.in/post/58333540883/k-fold-cross-validation-in-r

# install.packages("randomForest")

library(plyr)
library(randomForest)

data <- iris

# in this cross validation example, we use the iris data set to 
# predict the Sepal Length from the other variables in the dataset 
# with the random forest model 

k = 5 #Folds

# sample from 1 to k, nrow times (the number of observations in the data)

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

# prediction and testset data frames that we add to with each iteration over
# the folds

prediction <- data.frame()
testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  
  # run a random forest model
  mymodel <- randomForest(trainingset$Sepal.Length ~ ., data = trainingset, ntree = 100)
  
  # remove response column 1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}

# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)

# As an example use Mean Absolute Error as Evalution 
summary(result$Difference)
