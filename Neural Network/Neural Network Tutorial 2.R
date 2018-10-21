# Neural Network Credit Scoring Tutorial

###########################################################################

# We must always set the seed to ensure that the results can be duplicated.

set.seed(1234567890)


# Set the working directory

setwd("C:/Users/Derek/Documents/RPackages/Neural Network")

dataset <- read.csv("NNCredit.csv") 

head(dataset)

##########################################################################

# Select the first 800 observations for the training set

trainset<- dataset[1:800,]

# Select the following 1200 for testing

testset<- dataset[801:2000,]

##########################################################################

library("neuralnet") 

# The neuralnet package uses resilient backpropagation 
# with weight backtracking as its standard algorithm.

# The lifesign option refers to the verbosity. 
# The ouput is not linear and we will use a threshold value of 10%. 

# Build the neural network

creditnet <- neuralnet(default10yr ~ LTI + age, trainset, hidden = 4, lifesign = "minimal",linear.output = FALSE, threshold = 0.1)

## plot the NN

plot(creditnet, rep = "best")


##########################################################################
# once we have trained the NN we are ready to test the set.

## test the resulting output

temp_test <- subset(testset, select = c("LTI", "age")) 

creditnet.results <- compute(creditnet, temp_test)

# Lets see what the NN had produced

head(temp_test)

##########################################################################
# this will be to check the prediction against the actual observed

results <- data.frame(actual = testset$default10yr, prediction = creditnet.results$net.result)

results[100:115, ]

# Round to the nearest integer to make it easier to read.

results$prediction <- round(results$prediction)
results[100:115, ]