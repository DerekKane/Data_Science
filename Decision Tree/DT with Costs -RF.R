########################################################################
# Weighting model fit with ctree in party

# http://heuristically.wordpress.com/2010/03/15/weighting-model-fit-ctree-party/

########################################################################

# load the mlbench package which has the BreastCancer data set

require(mlbench)

# if you don't have any required package, use the install.packages() command
# load the data set

data(BreastCancer)

# remove the unique identifier, which is useless and would confuse the machine learning algorithms
BreastCancer$Id <- NULL

# partition the data set for 80% training and 20% evaluation (adapted from ?randomForest)
set.seed(2)
ind <- sample(2, nrow(BreastCancer), replace = TRUE, prob=c(0.8, 0.2))

# model using ctree without weights
require(party)
x.ct <- ctree(Class ~ ., data=BreastCancer[ind == 1,])
x.ct.pred <- predict(x.ct, newdata=BreastCancer[ind == 2,])
x.ct.prob <-  1- unlist(treeresponse(x.ct, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)]

# model using ctree with weights 1:10 (benign:malignant)
x.ctw <- ctree(Class ~ ., data=BreastCancer[ind == 1,], weights= ifelse(BreastCancer[ind == 1,]$Class=='benign', 1, 10))
x.ctw.pred <- predict(x.ctw, newdata=BreastCancer[ind == 2,])
x.ctw.prob <-  1- unlist(treeresponse(x.ctw, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)] 

# model using ctree with weights 10:1 (benign:malignant)
x.ctw2 <- ctree(Class ~ ., data=BreastCancer[ind == 1,], weights= ifelse(BreastCancer[ind == 1,]$Class=='benign', 10, 1))
x.ctw2.pred <- predict(x.ctw2, newdata=BreastCancer[ind == 2,])
x.ctw2.prob <-  1- unlist(treeresponse(x.ctw2, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)] 

# Output the plot to a PNG file for display on web.  To draw to the screen,
# comment this line out.
# png(filename="roc_curve_weights.png", width=700, height=700)

# plot performance
require(ROCR)

# create an ROCR prediction object from probabilities
x.ct.prob.rocr <- prediction(x.ct.prob, BreastCancer[ind == 2,'Class'])
# prepare an ROCR performance object for ROC curve (tpr=true positive rate, fpr=false positive rate)
x.ct.perf <- performance(x.ct.prob.rocr, "tpr","fpr")
# plot it
plot(x.ct.perf, col=2, main="ROC curves comparing classification performance of ctree weighted vs unweighted")

# Draw a legend.
legend(0.6, 0.6, c('unweighted', 'weighted 1:10 (benign:malignant)', 'weighted 10:1'), 2:4)

# weighted 1:10
x.ctw.prob.rocr <- prediction(x.ctw.prob, BreastCancer[ind == 2,'Class'])
x.ctw.perf <- performance(x.ctw.prob.rocr, "tpr","fpr")
# add=TRUE draws on the existing chart
plot(x.ctw.perf, col=3, add=TRUE)

# weighted 10:1
x.ctw2.prob.rocr <- prediction(x.ctw2.prob, BreastCancer[ind == 2,'Class'])
x.ctw2.perf <- performance(x.ctw2.prob.rocr, "tpr","fpr")
# add=TRUE draws on the existing chart
plot(x.ctw2.perf, col=4, add=TRUE)

# close and save PNG
dev.off()