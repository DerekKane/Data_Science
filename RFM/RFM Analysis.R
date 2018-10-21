##########################################################################
# RFM Analysis
##########################################################################

# http://www.dataapple.net/?p=84

##########################################################################

# read CDNOW_SAMPLE.txt

df <- read.table(file.choose(),header=F)

# construct a data frame with the necessary columns of customer ID, transaction date, and money amount paid by a customer per transaction  
df <- as.data.frame(cbind(df[,1],df[,3],df[,5]))

# add appropriate column names for the above three column and  
names <- c("ID","Date","Amount")

names(df) <- names

#tranfer the the text column type to date type
  
df[,2] <- as.Date(as.character(df[,2]),"%Y%m%d")

head(df)


dim(df)

#remove the rows with the duplicated IDs to see how many customers in total

uid <- df[!duplicated(df[,"ID"]),]

dim(uid)

##########################################################################
# Calculate the Recency, Frequency, and Monetary Values
##########################################################################

# set the startDate and endDate, we will only analysis the records in this date range
# We must also run the functions in the RFM Functions.R file prior to this step.


startDate <- as.Date("19970101","%Y%m%d")
endDate <- as.Date("19980701","%Y%m%d")

df <- getDataFrame(df,startDate,endDate)

head(df) 


##########################################################################
# Calculate the Recency, Frequency, and Monetary Rank
##########################################################################

df1 <-getIndependentScore(df)

head(df1[-(2:3)])



##########################################################################
# Review the histograms to see if the splits are even.
#########################################################################

#Draw the histograms in the R, F, and M dimensions so that we can see the distribution of customers in each RFM cell.

drawHistograms(df1)


# Let's further find out how many customers have a total score larger than 500 or 400.

S500<-df1[df1$Total_Score>500,]

dim(S500)

S400<-df1[df1$Total_Score>400,]

dim(S400)

##########################################################################
# RFM Scoring with Breaks
#########################################################################

# Sometimes users want to determine the breaks for each dimension by themselves according to their own business requirement. 
# For example, a user can set 0 -30 days, 30 -90 days, 90 - 180 days, 
# 180 - 360 days, and more than 360 days as the 5 breaks for Recency 
# rather than just let the computer segment the customers into 
# 5 aliquots without considering the specific business requirement.

# Before we execute the function, we can take a look at the distributions of Recency, Frequency, and Monetary.

par(mfrow = c(1,3))

hist(df$Recency)
hist(df$Frequency)
hist(df$Monetary)


# set the Recency ranges as 0-120 days, 120-240 days, 240-450 days, 450-500days, and more than 500days.

r <-c(120,240,450,500)

# set the Frequency ranges as 0 - 2times, 2-5 times,5-8 times, 8-10 times, and more than 10 times.

f <-c(2,5,8,10)

# set the Monetary ranges as 0-10 dollars, 10-20 dollars, and so on.

m <-c(10,20,30,100)

# Than we can execute the function of "getScoreWithBreaks" and see the customers distributions in the RFM cells.

df2<-getScoreWithBreaks(df,r,f,m)

drawHistograms(df2)

# We can also calculate how many customers have a total score of more than 500 or 400.

S500<-df2[df2$Total_Score>500,]

dim(S500)

drawHistograms(df2)

S400<-df2[df2$Total_Score>400,]

dim(S400)

# There are 641 customers have a RFM score more than 400.

##########################################################################
# eStimate Response Rate
#########################################################################

# After we segment the customers into RFM cells, we can assign the response rate to each RFM cell according to historical responding data. If it is the first time to use RFM analysis 
# and there is no historical data, we can select some customers, 
# say 10% percent, randomly from each RFM cells. Send mails to
# the selected customers as a trail and count the response rate for each cell. 
# Below is an example of the response rate table.



