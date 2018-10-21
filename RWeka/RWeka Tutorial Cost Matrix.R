# R to WEKA Tutorial

# http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/

########################################################################

mydata<-iris

########################################################################

library(RWeka)
library(rpart)

mydata_j48 <- J48(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = mydata)
mydata_j48

########################################################################
# Display the summary of the dataset.

summary(mydata_j48)

# plot the decision tree

plot(mydata_j48)

########################################################################

# evaluate the model in WEKA

eval_j48 <- evaluate_Weka_classifier(mydata_j48, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
eval_j48


########################################################################

# This allows the use of the WEKA controls

WOW("J48")

########################################################################

# If you think classifying for example a versicolor wrongly is very harmful, you want to penalize such a classification in our example, you can do that easily - you just 
# have to choose a different classifier, 
# namely the "Cost-sensitive classifier" in Weka:

csc <- CostSensitiveClassifier(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = mydata, control = Weka_control(`cost-matrix` = matrix(c(0,10, 0, 0, 0, 0, 0, 10, 0), ncol = 3), W = "weka.classifiers.trees.J48", M = TRUE))

# But you have to tell the "cost-sensitive-classifier" that you want to use J48 as algorithm, and you have to tell 
# him the cost matrix you want to apply, name ly the matrix of the form

matrix(c(0, 10, 0, 0, 0, 0, 0, 10, 0), ncol = 3)

# where you penalize "versicolor" being 
# falsely classified as one of the others by factor 10

# And again we evaluate on 10-fold CV:

eval_csc <- evaluate_Weka_classifier(csc, numFolds = 10, complexity = FALSE,seed = 1, class = TRUE)

eval_csc


