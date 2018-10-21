# Word Cloud Tutorial
# http://georeferenced.wordpress.com/2013/01/15/rwordcloud/

setwd("C:/Users/dkane/Documents/R Packages/WordCloud")

#specifies the exact folder where my text file(s) is for analysis with tm.

library(tm)

a  <-Corpus(DirSource("/Users/dkane/Documents/R Packages/WordCloud"), readerControl = list(language="lat")) 
summary(a)  #check what went in

inspect(a)

# this will cleanup the document

a <- tm_map(a, stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english")
a <- tm_map(a, stemDocument)

library(wordcloud)

wordcloud(a, scale=c(5,0.5), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

# this is how we will get rid of words that are not useful.

a <- tm_map(a, removeWords, c("noble", "lord"))