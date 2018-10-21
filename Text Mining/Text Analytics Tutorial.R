# Text Analytics Tutorial - Gender Prediction
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Text Mining")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set Options - Important for text analytics

options(stringAsFactors = FALSE)


mydata <- read.csv("BlogGender.csv")

mydata2 <- mydata
mydata2$Text <- NULL

# mydata$Gender <- NULL

#########################################################################

# Step 1. - Initialize the packages.

library("tm")
library("plyr")
library("class")
library("NLP")
library(SnowballC)


########################################################################
# Create a corpus
########################################################################

Corpus.mydata <- Corpus(VectorSource(mydata$Text))

# multi step approach for building a corpus from a df

# my.docs <- VectorSource(mydata)
# Corpus.mydata <- Corpus(my.docs)
# inspect(Corpus.mydata)


########################################################################
# Corpus Cleanup
########################################################################

Corpus.mydata <- tm_map(Corpus.mydata, removeNumbers)
Corpus.mydata <- tm_map(Corpus.mydata, tolower)
Corpus.mydata <- tm_map(Corpus.mydata, removeWords, stopwords("english"))
Corpus.mydata <- tm_map(Corpus.mydata, removePunctuation)
Corpus.mydata <- tm_map(Corpus.mydata, stripWhitespace)

inspect(Corpus.mydata[1:3])

#########################################################################
# Step 3. - Generate the TDM.
#########################################################################

# Matrix with columns as the documents and rows as the terms.
# Corpus.TDM <- TermDocumentMatrix(Corpus.mydata)

# Matrix with columns as the terms and rows as the documents.
Corpus.TDM <- DocumentTermMatrix(Corpus.mydata)

# Remove Sparse Terms from Corpus.TDM
Corpus.TDM <- removeSparseTerms(Corpus.TDM, 0.95)

#########################################################################
# Step 3b. - Understand the corpus by frequency and association. 
#########################################################################


findFreqTerms(Corpus.TDM, 2000)

findFreqTerms(Corpus.TDM, lowfreq=10)

# which words are associated with "company"?

findAssocs(Corpus.TDM, 'company', 0.30)


# Here is how to create a list of term frequencies

count<- as.data.frame(inspect(Corpus.TDM))
count$word = rownames(count)
colnames(count) <- c("count","word" )
count<-count[order(count$count, decreasing=TRUE), ]


#temp <- inspect(Corpus.TDM)
#FreqMat <- data.frame(apply(temp, 1, sum))
#FreqMat <- data.frame(ST = row.names(FreqMat), Freq = FreqMat[, 1])
#FreqMat <- FreqMat[order(FreqMat$Freq, decreasing = T), ]
#row.names(FreqMat) <- NULL
#View(FreqMat)


#########################################################################
# Step 4. - Add Gender to dataframe
#########################################################################

# Create a result dataset for review

mydata.df <- as.data.frame(inspect(Corpus.TDM))

# mydata.df2 <- as.data.frame(inspect(Corpus.TDM.2))

# This will append the column to the end of the dataframe for analysis

mydata.df <- cbind(mydata.df, mydata2) 

#########################################################################
# Step 5. - Create a classification algorithm
#########################################################################

# Create the train and test dataset for modeling. The train
# sample will contain 70% of the dataset.

train.idx <- sample(nrow(mydata.df), ceiling(nrow(mydata.df) * 0.7)) 
test.idx <- (1:nrow(mydata.df)) [-train.idx]

#########################################################################
# kNN Analysis
#########################################################################

# This will include all of the rows and just the target gender variable.

tdm.gend <- mydata.df[, "Gender"]

# This will have all of the variables except the target gender variable.

tdm.gend.nl <-mydata.df[, !colnames(mydata.df) %in% "Gender"] 

# Here is the kNN model. The knn(tdm.gend.nl[train.idx]) code specifies
# to use the train data and the dataset without the targetcandidate variable.

knn.pred <- knn(tdm.gend.nl[train.idx, ], tdm.gend.nl[test.idx, ], tdm.gend[train.idx])

# the tdm.gend.nl[train.idx, ] runs the knn algorythm, the tdm.gend.nl[test.idx, ]
# applies the knn on the train set to the test dataset, and the 
# tdm.gend[train.idx] gives the names of the gender for the final results.

#########################################################################
# Determine the accuracy of the model
#########################################################################

conf.mat <- table("Predicitons" = knn.pred, Actual = tdm.gend[test.idx])
conf.mat

# Formula for accuracy

(accuracy <- sum(diag(conf.mat)) / length(test.idx) * 100)


#########################################################################
# SVM Analysis
#########################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Library for SVM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(e1071)

mydata.df$Gender <- as.factor(mydata.df$Gender)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Split the data into test and training sets.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(1234)
ind <- sample(2, nrow(mydata.df), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata.df[ind==1,]
testData <- mydata.df[ind==2,]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a SVM for tuning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tuned <- tune.svm(Gender~.,data = trainData, gamma = 10^(-6:-1), cost = 10^(-1:1))


summary(tuned)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a SVM from the tuned parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SVMModel <- svm(Gender~., 
                data = trainData, gamma=0.001, cost=10)

print(SVMModel)
summary(SVMModel)


# test with train data
testData$Predict <- predict(SVMModel, testData)

# Update class to correct issue.

testData$class[testData$class == "2"] <-"1"
testData$class[testData$class == "3"] <-"1"
testData$class[testData$class == "12"] <-"1"
testData$class[testData$class == "4"] <-"1"
testData$class[testData$class == "5"] <-"1"
testData$class[testData$class == "8"] <-"1"


# Check for accuracy:
table(testData$Predict, testData$class)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(mydata.df, file='C:/Users/Derek/Documents/mydata.csv', row.names=T)

