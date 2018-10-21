mydata <- iris

head(iris)

library(randomForest)
library(party)
library(Design)
library(languageR)


frmla <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# Decision Tree in R

(ct = ctree(frmla, data = mydata))
plot(ct, main="Conditional Inference Tree")

#Table of prediction errors
table(predict(ct), mydata$Species)

# Estimated class probabilities
tr.pred = predict(ct, newdata=mydata, type="prob")

##################
## randomForest

# Because the process has the two sources of randomness that we discussed earlier, it is a good idea to set the random seed in R before you begin. This makes your results reproducible next time you load the code up, otherwise you can get different classifications for each run.

set.seed(415)

library(randomForest)

RForestIris <- randomForest(frmla, data=mydata, ntree=1000, mtry=3, cutoff=c(1,2))
RForestIris


data.controls <- cforest_unbiased(ntree=1000, mtry=3, cutoff) 

data.cforest <- cforest(frmla, data = mydata, controls=data.controls)

data.cforest.varimp <- varimp(data.cforest, conditional = TRUE)

#####################################################################
# Predict Function from Random Forest

mydata$y <- predict(data.cforest, newdata = mydata, type="response")
mydata

mydata$prob=predict(data.cforest, newdata = mydata, type="prob")
mydata

#####################################################################

# Simple Dotplot
dev.off()


dotchart(sort(data.cforest.varimp),main="Variable Importance Plot", 
         xlab="Mean Gini Score", cex=1.1, color="black") 

# Advanced Dotplot

dotchart(sort(data.cforest.varimp), xlab="Variable Importance in DATA\n(predictors to right of dashed vertical line are significant)", panel = function(x,y){ 
  chart.dotplot(x, y, col='darkblue', pch=16, cex=1.1)
  chart.abline(v=abs(min(data.cforest.varimp)), col='red', 
  lty='longdash', lwd=2) } )
chart.abline(v=0, col='blue')
             
# Advanced 2

