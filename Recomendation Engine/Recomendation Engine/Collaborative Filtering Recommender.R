#########################################################################
# Recommender Lab Tutorial
#########################################################################

# http://bigdata-doctor.com/recommender-systems-101-practical-example-in-r/


# install.packages("recommenderlab")

library("recommenderlab")
library("dplyr")

# Set Working Directory for the data file.

setwd("C:/Users/dkane/Documents/R Scripts/Recomendation Engine/Last FM/") 

# Read input data and check contents

data("MovieLense")

# Coerce the sample data matrix to a dataframe

mydata <- as(MovieLense, "data.frame")

View(mydata)

# Loading to pre-computed affinity data	 

affinity.matrix <- as(mydata,"realRatingMatrix")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample Code

# We are going to create a model called UBCF or U(ser) B(ased) 
# C(ollaborative) F(iltering) trained with 5000 users.
# Alternatively, we could use a less memory intensive 
# approach without having to load the entire user data 
# base in memory called IBCF ??? I(ser) B(ased) C(ollaborative) 
# F(iltering), just changing the parameter in the code below:

# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
# Rec.model<-Recommender(affinity.matrix[1:5000], method = "UBCF")

# This model computes internally the cosine similarity between all users represented as vectors, which in R is as simple as:

# crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####################################################################
# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
####################################################################
# 400 users

Rec.model=Recommender(affinity.matrix[1:400],method="UBCF", 
                      param=list(normalize = "Z-score",method="Cosine",
                      nn=5, minRating=1))

####################################################################
# The model in action ??? top N items and item affinity
####################################################################

# Now we can play with our model??? for example, let???s try to obtain 
# the top recommendations for a particular user ???u15348???

# recommended  items for user 101
recommended.items.101 <- predict(Rec.model, affinity.matrix["101",])

# to display them
as(recommended.items.101, "list")

# to obtain the top 3
recommended.items.101.top3 <- bestN(recommended.items.101, n = 3)

# to display them
display <- as(recommended.items.101.top3, "list")


# Convert into data
Results <- as.data.frame(Results)

# Turn Row Names into a heading
Results <- cbind(Movies = rownames(Results), Results)

# Remove the original row names
rownames(Results) <- NULL

# Rename the 2nd column
colnames(mydata)[2] <- "Rating"




########################################################################
# Now, for the same user ???101???, let???s have a look at the affinity value 
# computed for all items we didn???t have any value in the original data:

# Predict list of product which can be recommended to given users	 	
# to predict affinity to all non-rated items

predicted.affinity.101 <- predict(Rec.model, affinity.matrix["101",], type="ratings")

# to see the user "101"'s predicted affinity for items we didn't have any value for
as(predicted.affinity.101, "list")

# .. and the real affinity for the items obtained from the affinity.matrix
Results <- as(affinity.matrix["101",], "list")

Results <- as.data.frame(Results)

# Turn Row Names into a heading
Results <- cbind(Movies = rownames(Results), Results)

# Remove the original row names
rownames(Results) <- NULL

# Rename the 2nd column
colnames(mydata)[2] <- "Rating"


########################################################################
# Validation
########################################################################

# create evaluation scheme splitting taking 90% of the date for training and leaving 10% for validation or test
e <- evaluationScheme(affinity.matrix[1:1000], method="split", train=0.9, given=15)

# creation of recommender model based on ubcf
Rec.ubcf <- Recommender(getData(e, "train"), "UBCF")

# creation of recommender model based on ibcf for comparison
Rec.ibcf <- Recommender(getData(e, "train"), "IBCF")

# making predictions on the test data set
p.ubcf <- predict(Rec.ubcf, getData(e, "known"), type="ratings")

# making predictions on the test data set
p.ibcf <- predict(Rec.ibcf, getData(e, "known"), type="ratings")

# obtaining the error metrics for both approaches and comparing them
error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(e, "unknown"))
error.ibcf<-calcPredictionAccuracy(p.ibcf, getData(e, "unknown"))
error <- rbind(error.ubcf,error.ibcf)
rownames(error) <- c("UBCF","IBCF")
error