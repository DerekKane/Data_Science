########################################################################
# Recommendation System - Tutorial v2.0
########################################################################

# Load the dataset for analysis

mydata <- read.csv("C:/Users/Derek/Documents/R Scripts/Recomendation Engine/Movies.csv")

# Load the libraries for analysis

# install.packages("lsa")

library(reshape)
library(reshape2)
library(lsa)

# Pivot the dataframe to create a crosstab. Use the reshape2 package.

newdata <- dcast(mydata, Users ~ Movie, value.var = "Rating")

# data.m <- melt(mydata, id = c("Users","Movie")) 

# Transform the new dataset into a matrix for further processing.
# Perform a cosine similarity calculation

x = newdata[,2:7]
x[is.na(x)] = 0

item_sim = cosine(as.matrix(x))

# Step2: Predicting the targeted item rating for the targeted User CHAN.

# Recommending items for chan: since three movies are not rated\
# nas a first step we have to predict rating value for each movie\
# in CHANs case we have to first predict values for Titanic, Inception,Matrix"

########################################################################
# Item based collaborative filtering
########################################################################


# This function will create the recomendation for user number we choose

ItemRecommendation = function(userno)
{
  #extract all the movies not rated by CHAN
  userRatings = newdata[userno,]
  non_rated_movies = list()
  rated_movies = list()
  for(i in 2:ncol(userRatings)){
    if(is.na(userRatings[,i]))
    {
      non_rated_movies = c(non_rated_movies,colnames(userRatings)[i])
    }
    else
    {
      rated_movies = c(rated_movies,colnames(userRatings)[i])
    }
  }
  non_rated_movies = unlist(non_rated_movies)
  rated_movies = unlist(rated_movies)
  #create weighted similarity for all the rated movies by CHAN
  non_rated_pred_score = list()
  for(j in 1:length(non_rated_movies)){
    temp_sum = 0
    df = item_sim[which(rownames(item_sim)==non_rated_movies[j]),]
    for(i in 1:length(rated_movies)){
      temp_sum = temp_sum+ df[which(names(df)==rated_movies[i])]
    }
    weight_mat = df*newdata[userno,2:7]
    non_rated_pred_score = c(non_rated_pred_score,rowSums(weight_mat,na.rm=T)/temp_sum)
  }
  pred_rat_mat = as.data.frame(non_rated_pred_score)
  names(pred_rat_mat) = non_rated_movies
  for(k in 1:ncol(pred_rat_mat)){
    newdata[userno,][which(names(newdata[userno,]) == names(pred_rat_mat)[k])] = pred_rat_mat[1,k]
  }
  return(newdata[userno,])
}

# Now lets call the function 

ItemRecommendation(1)

# We can see the recomendations of Matrix 3.17, Titanic 3.08, & Inception 2.94 for Chan



########################################################################
# User Based collaborative filtering
########################################################################

#calculate the euclidian distance 
#EUD = dist(newdata[,2:7])

#cosine similarity calculation
x  = newdata[,2:7]
x[is.na(x)] = 0
user_sim = cosine(as.matrix(t(x))) #user similarity

#create weighted matrix
weight_mat = user_sim[,7]*newdata[,2:7] # user_sim[,7] is for Tom

UserBasedRecommedation = function(userNo)
{
  #calculate column wise sum
  col_sums= list()
  rat_user = newdata[userNo,2:ncol(newdata)]
  x=1
  tot = list()
  z=1
  
  for(i in 1:ncol(rat_user)){
    if(is.na(rat_user[1,i]))
    {
      
      col_sums[x] = sum(weight_mat[,i],na.rm=TRUE)
      x=x+1
      
      temp = as.data.frame(weight_mat[,i])
      
      sum_temp=0
      
      for(j in 1:nrow(temp)){
        if(!is.na(temp[j,1])){
          sum_temp = sum_temp+user_sim[j,ncol(rat_user)]
        }
        
      }
      tot[z] = sum_temp
      z=z+1
    } 
    
    
  }
  z=NULL
  
  z=1
  for(i in 1:ncol(rat_user)){
    if(is.na(rat_user[1,i]))
    {
      rat_user[1,i] = col_sums[[z]]/tot[[z]]
      z=z+1
    }
    
  }
  
  return(rat_user)
}


#to get N recommendations:
UserBasedRecommedation(1) #first person recommendations


###########################################################################

# http://www.dataperspective.info/2014/05/basic-recommendation-engine-using-r.html
# http://www.dataperspective.info/2015/11/item-based-collaborative-filtering-in-r.html
