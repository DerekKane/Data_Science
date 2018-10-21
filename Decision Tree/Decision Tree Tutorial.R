###################################################################
# Decision Tree Tutorial 
###################################################################

# Set working Directory 

setwd("C:/Users/Derek/Documents/RPackages/Decision Tree/") 

# Load Libraries

library(caret)
library(rpart)
library(rpart.plot)
library(C50)
library(rattle)
library(party)
library(partykit)
library(RWeka)
library(randomForest)



# Load the data

diabetes <- read.csv("Diabetes.csv")
churn <- read.csv("Churn.csv")

###################################################################
# Build a decision tree using RPART for diabetes
###################################################################

# Grow the Decision Tree 

fit <- rpart(Class~.,method="class", data=diabetes)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 

plot(fit, uniform=TRUE, 
     main="Classification Tree for Diabetes")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prune the Decision Tree
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# This code will prune based on the specified cp value.
# Ex. cp = 0.043 will produce a tree of size 3.
# pfit<- prune(fit, cp=0.043)


# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Diabetes")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fancy plot of the decision tree with Rattle
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fancyRpartPlot(pfit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a RPART decision tree using the party package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

party.pfit <- as.party(pfit)
plot(party.pfit)


###################################################################
# Build a decision tree using C5.0 for Churn
###################################################################

# The decision variable class must be converted into a factor
# variable in order for the C50 to process correctly.

churn$Churn <- as.factor(churn$Churn)

# Run the c50 algorithm for a decision tree.

c50_tree_result<-C5.0(Churn~.,data=churn)

# display the summary

summary(c50_tree_result)

C5imp(c50_tree_result,metric='usage')
C5imp(c50_tree_result,metric='splits')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the c50 algorithm and show the decision rules.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c50_rule_result<-C5.0(Churn~.,data=churn, rules=TRUE)

# display the summary

summary(c50_rule_result)

###################################################################
# Build a RIPPER decision tree using WEKA for Churn
###################################################################

# http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/

# Here is the algorithm for the RIPPER.
churn_RIPPER <- JRip(Churn~.,data=churn)

# This is the set of unpruned decision rules produced.

churn_RIPPER

# Here is the summary of the models performance.
summary(churn_RIPPER)

# Here is the pruning of the rules using the WEKA controls.

list_Weka_interfaces()
WOW("JRip")

# Lets change the minimum weights of instances within a split from 2 to 5.

churn_RIPPER_control <- JRip(Churn~., data = churn, control = Weka_control(N = 5))
churn_RIPPER_control


summary(churn_RIPPER_control)

###################################################################
# Build a Regression Decision Tree for MPG
###################################################################

data(cu.summary)
attach(cu.summary)

# grow tree 
fit <- rpart(Mileage~., method="anova", data=cu.summary)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results   

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Pruning the tree

pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fancy plot of the decision tree with Rattle
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fancyRpartPlot(fit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a RPART decision tree using the party package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

party.pfit <- as.party(fit)
plot(party.pfit)

###################################################################
# Build a Random Forest for Churn
###################################################################

# Drop the factor variable with over 32 levels.

churn$State <- NULL
churn$Phone.Number <- NULL

# Build the random forest model

RandomForestModel <- randomForest(Churn~., data=churn, ntree=500, mtry=5, importance=TRUE)
print(RandomForestModel)
importance(RandomForestModel)

plot.new()
plot(RandomForestModel, log="y")
varImpPlot(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=40, col="blue")

varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=100, col="blue")


###################################################################
# Create the predictions based off of the models.
###################################################################

churn$c50predict <- predict(c50_tree_result, churn)
churn$JRippredict <- predict(churn_RIPPER_control, churn)
churn$RFpredict <- predict(RandomForestModel, churn)



###################################################################
###################################################################
###################################################################
###################################################################

# CART for kytosis


data(kyphosis)
attach(kyphosis)

# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fancy plot of the decision tree with Rattle
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fancyRpartPlot(fit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a RPART decision tree using the party package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

party.pfit <- as.party(fit)
plot(party.pfit)

