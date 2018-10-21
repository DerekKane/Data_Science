#load the iris data
data(iris)

# this data has 150 rows
nrow(iris)

# look at the first few
head(iris)

# splitdf function will return a list of training and testing sets

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2)) # The /2 is creating a 50%/50% training and testing split
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(iris, seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 75 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset


########### Optional: apply to iris data using randomForest ###########

#load the randomForest library. if you havent installed it, run the next line
install.packages("randomForest")
library(randomForest)

#fit the randomforest model
model <- randomForest(Sepal.Length~., 
                      data = training, 
                      importance=TRUE,
                      keep.forest=TRUE)
print(model)

#what are the important variables (via permutation)
varImpPlot(model, type=1)

#predict the outcome of the testing data
predicted <- predict(model, newdata=testing[ ,-1])

# what is the proportion variation explained in the outcome of the testing data?
# i.e., what is 1-(SSerror/SStotal)
actual <- testing$Sepal.Length
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)
