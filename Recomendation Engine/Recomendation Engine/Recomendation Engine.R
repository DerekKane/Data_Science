###################################################################
# Recomendation Engine Tutorial
###################################################################

# http://www.craftbeeranalytics.com/beer-data.html
# http://blog.yhathq.com/posts/recommender-system-in-r.html

# Set working Directory 

setwd("C:/Users/dkane/Documents/R Scripts/Recomendation Engine/") 


# Load the data

data <- read.csv("beerreview.csv")

summary(data)


# remove NA from dataset

data <- na.omit(data)

###################################################################
# Find Common Reviewers
###################################################################


# We'll need a function which takes two beers and returns their mutual 
# reviewers (or sameset). To do this, we'll use the intersect 
# function in R which finds common elements between two lists or vectors.

# I wrote two functions: common_reviewers_by_id to extract the sameset given
# two beer_ids, and common_reviewers_by_name to extract the samesets given two beer_names. 

# Note: beer_id = 5 and 17, or beer_name = Appalachian 666 and Alameda Black Bear XX Stout

# This is a function for common reviewers by beer id

common_reviewers_by_id <- function(beer1, beer2) {
  reviews1 <- subset(data, beer_id==beer1)
  reviews2 <- subset(data, beer_id==beer2)
  reviewers_sameset <- intersect(reviews1[,'review_profilename'],
                                 reviews2[,'review_profilename'])
  if (length(reviewers_sameset)==0) {
    NA
  } else {
    reviewers_sameset
  }
}

# This creates the lookup for distinct beers

beer_lookup <- data[,c("beer_id", "beer_name")]

# remove duplicate entries

beer_lookup <- beer_lookup[duplicated(beer_lookup)==FALSE,]

# This is a function for common reviewers by name.

common_reviewers_by_name <- function(name1, name2) {
  beer1 <- subset(beer_lookup, beer_name==name1)$beer_id
  beer2 <- subset(beer_lookup, beer_name==name2)$beer_id
  common_reviewers_by_id(beer1, beer2)
}

# Here is a mock example for review

common_reviewers_by_id(5, 17)
common_reviewers_by_name("Appalachian 666", "Alameda Black Bear XX Stout")


###################################################################

# Next we need a function to extract features for a given beer. Features, in this case, 
# are the numerical ratings provided by users as part of each beer's review.

features <- c("review_overall", "review_aroma", "review_palate", "review_taste")

get_review_metrics <- function(beer, userset) {
  beer.data <- subset(data, beer_id==beer & review_profilename %in% userset)
  o <- order(beer.data$review_profilename)
  beer.data <- beer.data[o,]
  dups <- duplicated(beer.data$review_profilename)==FALSE
  beer.data <- beer.data[dups,]
  #this can return more than 1 type of metric
  beer.data[,features]
}

# Lets take a look here.

head(reviews) # must be an error in the code somewhere?


###################################################################
# Quantifying Our Beliefs
###################################################################

# I don't need a statistical model to tell me that 
# someone who likes Fat Tire is probably going to like 
# Dale's Pale Ale more than Michelob Ultra. But what about picking between 
# Dale's Pale Ale and Sierra Nevada Pale Ale? Things get a little more complicated. 
# For this reason (and because we don't want to manually select between each beer pair), 
# we're going to write a distance function that will quantify similarity.

# For our similarity metric we're going to use a weighted average 
# of the correlation of each metric. In other words, for each two-beer-pair
# we calculate the correlation of review_overall, review_aroma, review_palate, 
# and review_taste separately. Then we take a weighted average each result to 
# consolidate them into one number.

# We're going to weight review_overall with 2 and the 
# remainder will have a weight of 1. This gives review_overall 40% 
# of the score (NOTE: this is totally arbitrary, you can use whatever 
# weighting function you want. A lot of times the simplest stuff works 
# the best in my experience).

calc_similarity <- function(b1, b2) {
  common_users <- common_reviewers_by_id(b1, b2)
  if (is.na(common_users)) {
    return (NA)
  }
  beer1.reviews <- get_reviews(b1, common_users)
  beer2.reviews <- get_reviews(b2, common_users)
  #this can be more complex; we're just taking a weighted average
  weights <- c(2, 1, 1, 1)
  corrs <- sapply(names(beer1.reviews), function(metric) {
    cor(beer1.reviews[metric], beer2.reviews[metric])
  })
  sum(corrs * weights, na.rm=TRUE)
}

# lets test a couple of scenarios for 2 beers

b1 <- beer_name_to_id("Dicks Imperial IPA")
b2 <- beer_name_to_id("Highland Gaelic Ale")

calc_similarity(b1, b2)

# [1] 0.7295

b2 <- beer_name_to_id("Narragansett Light")

calc_similarity(b1, b2)

# [1] 0.21




###################################################################
