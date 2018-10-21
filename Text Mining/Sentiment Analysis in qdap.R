###################################################################
# Sentiment Analysis in Qdap
###################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/R Scripts/Text Mining")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set Options - Important for text analytics

options(stringAsFactors = FALSE)


mydata <- read.csv("Facebook2.csv")

########################################################################
# Remove Unneeded columns from table
########################################################################

mydata$ID <- NULL
mydata$Name <- NULL
mydata$Type <- NULL
mydata$Gender <- NULL
mydata$Birthday <- NULL
mydata$Relationship <- NULL

mydata <- na.omit(mydata)


# install.packages("qdap")


library(qdap)
library(tm)

#a good package, also takes into account  negative words and amplifiers
#see: http://www.inside-r.org/packages/cran/qdap/docs/polarity

# data<- read.csv("comments.csv")
# mycorpus<-Corpus(VectorSource(data$message))

mycorpus<-Corpus(VectorSource(mydata$Message))

#inspect the first few comments
inspect(mycorpus[1:4])

#Sentiment Analysis
#Text Segmentation

mycorpus<-tm_map(mycorpus,tolower)
mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,removeNumbers)
mycorpus<-tm_map(mycorpus,removeWords,stopwords(kind="English"))
stopwords(kind="English")
mycorpus<-tm_map(mycorpus,stripWhitespace)
# mycorpus<-tm_map(mycorpus,PlainTextDocument)


#convert corpus to data frame
mydf<-data.frame(text=unlist(sapply(mycorpus,'[',"content")),stringAsFactors=FALSE)
mydf$text<-as.character(mydf$text)


a <- unlist(apply(mydf,1,function(x) polarity(x[1])[[2]]$ave.polarity))
attributes(a) <- NULL
a  <- as.vector(a)
score <- data.frame("Sentence_Num" = 1:nrow(mydf),"Sentiment Score" = a)

str(score)
final<-cbind(mydata, score[,2])

write.csv(final,"final.csv")

# References:
# http://stackoverflow.com/questions/22774913/estimating-document-polarity-using-rs-qdap-package-without-sentsplit