######################################################################
# Recommendation Engine - Collaborative Filtering
######################################################################

# http://dataanalytics.zone/2015/10/recommendation-engine-in-r/

# Libraries, clear memory, set working directory

library(dplyr)
rm(list=ls(all=TRUE))

# Set Working Directory for the data file.

setwd("C:/Users/dkane/Documents/R Scripts/Recomendation Engine/Last FM/") 

# Read input data and check contents

user_data <- read.csv("user_artists.dat", sep="\t")
user_data %>% glimpse
user_data[is.na(user_data)] <- 0

# Reshape data to be in 'wide' matrix input-format

input_data <- user_data %>%
  reshape(idvar = "userID", timevar = "artistID", direction = "wide")

# Remove user-id as we don't need it, it's in the first column

item_based_similarities <- input_data[,-1]
item_based_similarities[is.na(item_based_similarities)] <- 0

# Help-function to compute cosine-value of two vectors

cosine <- function(x,y) 
{
  result <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(result)
}

# Matrix that contains artist x artist similarity scores

item_matrix  <- matrix(NA, nrow=ncol(item_based_similarities),
                       ncol=ncol(item_based_similarities),
                       dimnames=list(colnames(item_based_similarities),
                                     colnames(item_based_similarities)))

# Determine similarities for all columns
# Double loop over all column to determine values

for(i in 1:ncol(item_based_similarities)) {
  for(j in 1:ncol(item_based_similarities)) {
    #Compute cosine similarities for this column
    item_matrix[i,j] <- cosine(as.matrix(item_based_similarities[i]),
                               as.matrix(item_based_similarities[j]))
  }
}
###
# Back to dataframe

item_matrix <- as.data.frame(item_matrix)

# Determine top 25 similar items

similar_items <- matrix(NA, nrow=ncol(item_matrix),
                        ncol=26,
                        dimnames=list(colnames(item_matrix)))

#Now store similar items

for(i in 1:ncol(item_based_similarities)) 
{
  similar_items[i,] <- (t
                        (head
                        (n=26,
                        rownames(item_matrix[order
                                             (item_matrix[,i],
                                             decreasing=TRUE
                                             ),][i]
                        )
                        )
                        )
  )
}
similar_items %>% head

#Add artists data and have a look
artists <- read.csv("artists.dat", sep="\t")
artists %>% glimpse

#Convert similar items to a data-frame for easier manipulation
df <- as.data.frame(similar_items)

#Replace all weight. from the data-frame by using gsub which replaces and sapply to do this on the entire data-frame
df2 <- as.data.frame(sapply(df, gsub, pattern="weight.", replacement=""))

#Make a copy of data-frame 2 to modify with the proper names
df3 <- df2

#Match the values from the data-frame with the artists id, and then select the second column (name) to store in df3
for (i in 1:ncol(df2))
{
  df3[,i] <- artists[match(df2[,i], artists$id), 2, drop=F]
}
df3 %>% head

#Create a function to determine scoring

scoring <- function(history, similarities)
{
  sum(history*similarities)/sum(similarities)
}

#Create a matrix to store the end-results
recommendations <- matrix(NA, 
                          nrow=nrow(input_data),
                          ncol=ncol(input_data)-1,
                          dimnames=list((input_data$userID),
                                        colnames(input_data[-1])))
#Loop over all users

for(i in 1:nrow(recommendations)) 
{
  #Loop over all items, in this case artists
  for(j in 1:ncol(recommendations)) 
  {
    #Get user id and product id
    user <- rownames(recommendations)[i]
    product <- colnames(recommendations)[j]
    
    #Do not recommend products you already have, if you want, leave this if-statement out
    if(as.integer(input_data2[input_data2$userID==user,product]) > 0)
    { 
      recommendations[i,j]<-""
    } 
    else 
    {
      #Get the top 25 neighbours sorted on similarity
      topN<-((head(n=26,(item_matrix[order(item_matrix[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      #Remove first item, it will be the same item itself
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      #Then retrieve user's purchase history for those items
      topN.purchases<- input_data[,c("userID",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      #Calculate scoring for that product-user combination
      recommendations[i,j]<-scoring(similarities=topN.similarities,history=topN.userPurchases)
      
    } 
  } 
} 

#Now change this to a more readable output

end_recommendations <- matrix(NA, nrow=nrow(recommendations),
                              ncol=100,
                              dimnames=list(rownames(recommendations)))

for(i in 1:nrow(recommendations)) 
{
  end_recommendations[i,] <- names(
    head(n=100,
         (recommendations[,order(recommendations[i,],
                                 decreasing=TRUE)]
         )[i,]
    ))
}

#Convert recommended items to a data-frame for easier manipulation
reco_df <- as.data.frame(end_recommendations)

#Replace all weight. from the data-frame by using gsub which replaces and sapply to do this on the entire data-frame
reco_df <- as.data.frame(sapply(reco_df, gsub, pattern="weight.", replacement=""))

#Make a copy of the data-frame to modify with the proper names
final <- reco_df

#Match the values from the data-frame with the artists id, and then select the second column (name) to store in final
for (i in 1:ncol(final))
{
  final[,i] <- artists[match(reco_df[,i], artists$id), 2, drop=F]
}
final %>% head