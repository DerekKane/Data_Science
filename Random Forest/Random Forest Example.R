# Random Forest Tutorial

# http://stackoverflow.com/questions/10112678/what-does-the-parameter-classwt-in-randomforest-function-in-randomforest-pack
# http://r.789695.n4.nabble.com/R-help-with-RandomForest-classwt-option-td817149.html

#########################################################################

mydata<-iris

library(randomForest)

#########################################################################

set.seed(126)

# The classwt function is used udring model training to assigning 
# a relative cost weight for classification errors. Used to balance the errors. 

RandomForestModel <- randomForest(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=mydata, ntree=1, mtry=3, classwt=c(setosa=1,versicolor=3,virginica=1), importance=TRUE)
print(RandomForestModel)
importance(RandomForestModel)

# this will show the oob scores and the prediction scores

RandomForestModel$votes
RandomForestModel$test$votes


plot.new()
plot(RandomForestModel, log="y")
varImpPlot(RandomForestModel)


plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=9, col="blue")

varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")

#########################################################################

mydata$RFPredict <- predict(RandomForestModel, newdata = mydata, type = "response")

# This is a way to combine the datasets into a single dataframe.

Pred1 <- predict(RandomForestModel, newdata = mydata, type = "prob")

# Convert the matrix "Pred1" into a dataframe

Pred1DF<-data.frame(Pred1)
mydata2<- cbind(mydata, Pred1DF)

#########################################################################

library(xlsx)
write.xlsx(mydata2, "C:/Users/dkane/Documents/RFModelExport2.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RF model with different (original) classwt option.

RandomForestModel <- randomForest(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=mydata, ntree=1, mtry=3, classwt=c(setosa=1,versicolor=3,virginica=1), importance=TRUE)