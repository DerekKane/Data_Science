
# title: "What's Cooking?"
# author: "Andrew Man Hay Chiu"
# output: html_document

# {r, init, echo = FALSE, messages = FALSE}

library(jsonlite)
library(dplyr)
library(ggplot2)
library(tm) # For NLP; creating bag-of-words
library(caret)
library(rpart)
library(rpart.plot)

# install.packages("jsonlite")

## Introduction

# When I tried to learn the bag of words technique using Kaggle's "Bag of Words Meets Bag of Popcorn" knowledge competition, the only tutorial available was for Python. 
# I hope this helps with some Kagglers who prefer to use R/RStudio as their data analytics program.  

# Some of the script is based on this rpubs [file](https://rstudio-pubs-static.s3.amazonaws.com/92510_018db285fda546fcb89b53dd2847b5d4.html#video-5-building-models-r-script-reproduced-here)

## Data

setwd("C:/Users/dkane/Documents/R Scripts/Kaggle/Whats Cooking/") 


train <- fromJSON("train.json", flatten = TRUE)


# Let's have a look at the train data. It seems like there are lots of Italian recipes. No wonder why the benchmark was set as predicting all test recipes as "Italian"!
  

ggplot(data = train, aes(x = cuisine)) + 
  geom_histogram() +
  labs(title = "Cuisines", x = "Cuisine", y = "Number of Recipes")


### Create Corpus

# In R, using the **tm** package, a *Corpus* is created from the lists of ingredients within the training data. It is essentially a form of storing the list of words in a format for NLP.
# The output shows that there are 39744 "documents", or in this case recipes, in the corpus. 


ingredients <- Corpus(VectorSource(train$ingredients))
ingredients


#### Preprocessing
# The corpus basically has the words/ingredients as they were imported. 
# Now there are words that are very similar, but may not be identical within the corpus. For example, there are plurals of words which count as a separate word (e.g.: *thigh* and *thighs*). 

# These should be reduced by preprocessing the data. One preprocessing technique shown below is to "stem" the words. This usually gets words spelt slightly differently into the same "stem" word.

ingredients <- tm_map(ingredients, stemDocument)
ingredients


# Note: Other preprocessing procedures in the tm package include: tolower, removePunctuation, removeWords, stopwords*
  
### Create Document Term Matrix (Bag of Ingredients)
  
# After preprocessing, a *Document Term Matrix* is created. The Term-Document Matrix is a matrix of words (or in this case, ingredients) in all of the recipes, and whether the ingredient appears in each recipe.

ingredientsDTM <- DocumentTermMatrix(ingredients)
ingredientsDTM


### Feature selection
# The Term-Document Matrix contains a lot of columns (ingredients). Reducing the number of features, by removing ingredients that don't occur often, may help with the model (although sometimes unique ingredients may be key to predict certain cuisines).

sparse <- removeSparseTerms(ingredientsDTM, 0.99)

## This function takes a second parameters, the sparsity threshold.
## The sparsity threshold works as follows.
## If we say 0.98, this means to only keep terms that appear in 2% or more of the recipes.
## If we say 0.99, that means to only keep terms that appear in 1% or more of the recipes.

sparse

# By selecting only ingredients that appear in at least 1% of the recipes, the number of ingredients in the Document Term Matrix was reduced from 2771 to 249, i.e.: only 8.99% of the full set of ingredients. The DTM is then converted to a data.frame for modelling.

ingredientsDTM <- as.data.frame(as.matrix(sparse))

## Add the dependent variable to the data.frame
ingredientsDTM$cuisine <- as.factor(train$cuisine)


## Create Model
# To estimate the error of the model, the "train" dataset/bag of ingredients is separated into a training and validation set. This can be done using the *caret* package.


inTrain <- createDataPartition(y = ingredientsDTM$cuisine, p = 0.6, list = FALSE)
training <- ingredientsDTM[inTrain,]
testing <- ingredientsDTM[-inTrain,]


### CART

# A basic model to create from the bag of ingredients is the CART model. This creates a decision tree based on some of the more important ingredients.
# The tree below shows the decision tree created from the training data.

set.seed(9347)
cartModelFit <- rpart(cuisine ~ ., data = training, method = "class")

## Plot the tree
prp(cartModelFit)

# We can then evaluate the model's accuracy using a confusion matrix. The accuracy of this model seems very poor at only 42.87%. This was close the result on the actual test set when submitted to Kaggle.
# Having a look at the prediction matrix shows where some of the problems are. Predictions were almost exclusively Chinese, Indian, Italian, Mexican, Southern US and Thai. Therefore, it might be useful to explore the ingredients unique to the other cuisines.

# With the highest score currently at over twice the accuracy, this simple bag of ingredients example is not very good. However, I hope this script can help some people!
  

cartPredict <- predict(cartModelFit, newdata = testing, type = "class")
cartCM <- confusionMatrix(cartPredict, testing$cuisine)
cartCM
