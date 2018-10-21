# Genetic Algorithm for SVM Tutorial

############################################################################
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Support Vector Machines")

############################################################################
# Load Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(GA)
library(caret)
library(genalg)
library(e1071)
library(kernlab)

mydata <- read.csv("breastcancer.csv")
mydata$Class <- as.factor(mydata$Class)

mydata$Sample.Code <- NULL


############################################################################
# Prediction function to be used for backtesting
############################################################################

pred1pd = function(t) {
  print(t)
  ##add section to select the best variable set from those available using GA
  # evaluation function - selects the best indicators based on miminsied training error
  mi.evaluate <- function(string=c()) {
    tmp <- mydata[(t-lookback):t,-1]
    x <- string
    tmp <- tmp[,x==1]
    tmp <- cbind(mydata[(t-lookback):t,1],tmp)
    colnames(tmp)[1] <- "targets"
    trainedmodel = ksvm(targets ~ ., data = tmp, type = ktype, kernel="rbfdot", kpar=list(sigma=0.1), C = C, prob.model = FALSE, cross = crossvalid)
    result <- error(trainedmodel)
    print(result)
  }
  
  ## monitor tge GA process
  monitor <- function(obj) {
    minEval = min(obj$evaluations);
    plot(obj, type="hist");
  }
  
  ## pass out the GA results; size is set to be the number of potential indicators
  gaResults <- rbga.bin(size=39, mutationChance=0.10, zeroToOneRatio=10, evalFunc=mi.evaluate, verbose=TRUE, monitorFunc=monitor, popSize=50, iters=3, elitism=10)
  
  ## now need to pull out the best chromosome and rebuild the data frame based on these results so that we can train the model
  
  bestChro <- gaResults$population[1,]
  newData <- mydata[,-1]
  newData <- newData[,bestChro==1]
  newData <- cbind(mydata[,1],newData)
  colnames(newData)[1] <- "targets"
  print(colnames(newData))
  
  # Train model using new data set
  model = trainSVM(newData[(t-lookback):t, ], ktype, C, crossvalid)
  # Prediction
  pred = as.numeric(as.vector(predict(model, newData[t+1, -1], type="response")))
  # Print for user inspection
  print(pred)
}

