###################################################################
# Recomendation Engine Tutorial # 2
###################################################################

# Set working Directory 

setwd("C:/Users/dkane/Documents/R Scripts/Recomendation Engine/") 

# Load the data

data <- read.csv("beerreview.csv")

summary(data)

# remove NA from dataset

data <- na.omit(data)


# Load Libraries

install.packages("tm")
install.packages("Snowball")

library(tm)