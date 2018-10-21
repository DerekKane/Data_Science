################################################################################
# Function
# 	getDataFrame(df,startDate,endDate,tIDColName="ID",tDateColName="Date",tAmountColName="Amount")
#
# Description
#	Process the input data frame of transcation records so that the data frame can be ready for RFM scoring.
#	A.Remove the duplicate records with the same customer ID
#	B.Find the most recent date for each ID and calculate the days to the endDate, to get the Recency data
#	C.Calculate the quantity of translations of a customer, to get the Frequency data
#	D.Sum the amount of money a customer spent and divide it by Frequency, to get the average amount per transaction, that is #the Monetary data.
#
# Arguments
#	df - A data frame of transcation records with customer ID, dates, and the amount of money of each transation
#	startDate - the start date of transcation, the records that happened after the start date will be kepted
#	endDate - the end date of transcation, the records that happed after the end date will be removed. It works with the start #date to set a time scope
#	tIDColName - the column name which contains customer IDs in the input data frame
#	tDateColName - the column name which contains transcation dates in the input data frame
#	tAmountColName - the column name which contains the amount of money of each transcation in the input data frame
#
# Return Value
#	Returns a new data frame with three new columns of "Recency","Frequency", and "Monetary". The number in "Recency" is the # quantity of days from the # #most recent transcation of a customer to the endDate; The number in the "Frequency" is the quantity # of transcations of a customer during the period from # #startDate to endDate; the number in the "Monetary" is the average amount # of money per transcation of a customer during that period.
#
#################################################################################
getDataFrame <- function(df,startDate,endDate,tIDColName="ID",tDateColName="Date",tAmountColName="Amount"){
  
  #order the dataframe by date descendingly
  df <- df[order(df[,tDateColName],decreasing = TRUE),]
  
  #remove the record before the start data and after the end Date
  df <- df[df[,tDateColName]>= startDate,]
  df <- df[df[,tDateColName]<= endDate,]
  
  #remove the rows with the duplicated IDs, and assign the df to a new df.
  newdf <- df[!duplicated(df[,tIDColName]),]
  
  # caculate the Recency(days) to the endDate, the smaller days value means more recent
  Recency<-as.numeric(difftime(endDate,newdf[,tDateColName],units="days"))
  
  # add the Days column to the newdf data frame
  newdf <-cbind(newdf,Recency)
  
  #order the dataframe by ID to fit the return order of table() and tapply()
  newdf <- newdf[order(newdf[,tIDColName]),]
  
  # caculate the frequency
  fre <- as.data.frame(table(df[,tIDColName]))
  Frequency <- fre[,2]
  newdf <- cbind(newdf,Frequency)
  
  #caculate the Money per deal
  m <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
  Monetary <- m[,1]/Frequency
  newdf <- cbind(newdf,Monetary)
  
  return(newdf)
  
} # end of function getDataFrame


########################################################################################### 
## Function
##        getPercentages <- function(df,colNames)
## Description
##        Caculating the probabilities of "Buy"/Repurchase grouped by R, F, M values respectively or in combination
## Arguments
##        df,a date frame with discreted variables of Recency, Frequency, and Monetary based on the data frame returned by the ## function of getDataFrame above
##        colNames,a vector of column names to be grouped by, such as c("Requency") or c("Requency","Frequency")
## Return Value
##        a data frame with the variables being used to grouped by and the percentages of customers who buy accordingly
###########################################################################################
require(plyr)
getPercentages <- function(df,colNames){
  
  Var<-c(colNames,"Buy")
  
  df<-df[,names(df) %in% Var,drop=F]
  
  
  a <- ddply(df,Var,summarize,Number=length(Buy))
  b <- ddply(a,
             .(),
             .fun=function(x){
               transform(x, Percentage=with(x,round(ave(Number,a[,names(a) %in% Var,drop=F],FUN=sum)/ave(Number,a[,names(a) %in% colNames,drop=F],FUN=sum),2)))
             })
  
  b<-b[b$Buy==1,-1]
  
  return(b)
  
}

########################################################################################### 
## Function
##        getCLV<-function(r,f,m,n,cost,periods,dr,pModel)
## Description
##        Caculating CLV
## Arguments
##        r, Recency value, e.g. r=0
##        f, Frequency value,e.g. f=1
##        m, the profit a customer can contribute
##        n, number of the customer who have the same Recency and Frequency value
##        cost, the cost accured in each purchasing period to every potential customers who would buy or not buy in the future ## period. e.g the postage to each customer for new product promotion.
##        periods, how many periods the customer will stay before he/she churn
##        dr, discount rate
##        pModel, the regression model which is used to predict the "buy" rate based on Recency,Frequency and/or Monetary 
## Return Value
##        the customers' value during the periods
###########################################################################################
getCLV<-function(r,f,m,n,cost,periods,dr,pModel){
  
  df<-data.frame(period=c(0),r=c(r),f=c(f),n=c(n),value=c(0))
  
  for(i in 1:periods){
    backstep<-df[df$period==i-1,]
    nrow<-nrow(backstep)
    for(j in 1:nrow){
      r<-backstep[j,]$r
      f<-backstep[j,]$f
      n<-backstep[j,]$n
      p<-predict(pModel,data.frame(Recency=r,Frequency=f),type='response')[1]
      buyers<-n*p
      df<-rbind(df,c(i,0,f+1,buyers,buyers*(m-cost) / (1+dr)^i))
      df<-rbind(df,c(i,r+1,f,n-buyers,(n-buyers)*(-cost)  / (1+dr)^i ))
    }
  }
  
  return(sum(df$value))
  
}

#####################################################################################
# read CDNOW_SAMPLE.txt
df <- read.table(file.choose(),header=F)

# construct a data frame with the necessary columns of customer ID, transaction date, and money amount paid by a customer # per transaction
df <- as.data.frame(cbind(df[,1],df[,2],df[,4]))

# add appropriate column names for the above three column and 
names <- c("ID","Date","Amount")
names(df) <- names

#tranfer the the text column type to date type
df[,2] <- as.Date(as.character(df[,2]),"%Y%m%d")

head(df)

# set the "history" transaction time scope
startDate_history <- as.Date("19970101","%Y%m%d")
endDate_history <- as.Date("19980228","%Y%m%d")

#  set the "forecast" transaction time scope which are a bi-month purchasing cycle time
startDate_forcast <- as.Date("19980301","%Y%m%d")
endDate_forcast <- as.Date("19980430","%Y%m%d")

#get the rolled up R,F,M data frames
history <- getDataFrame(df,startDate_history,endDate_history)
forcast <- getDataFrame(df,startDate_forcast,endDate_forcast)

# set the purchasing cycle time as 60 days, and discrete the Recency 
history$Recency<- history$Recency %/% 60 

#discrete the Monetary by $10 interval
breaks<-seq(0,round(max(history$Monetary)+9),by=10)
history$Monetary<-as.numeric(cut(history$Monetary,breaks,labels=FALSE))

#add a Buy/No Buy column to the RFM data frame
Buy<-rep(0,nrow(history))
history<-cbind(history,Buy)

# find out the those who repurchased in the forcast period 19980301 - 19980430
history[history$ID %in% forcast$ID, ]$Buy<-1

train<-history
head(train)

# get "Buy" percentages based on the variable Recency
colNames<-c("Recency")
p<-getPercentages(train,colNames)

# get the Buy ~ Recency model
r.glm=glm(Percentage~Recency,family=quasibinomial(link='logit'),data=p)
p_r<-p

# get "Buy" percentages based on the variable Frequency
colNames<-c("Frequency")
p<-getPercentages(train,colNames)

# get the Buy ~ Frequency model
f.glm=glm(Percentage~Frequency,family=quasibinomial(link='logit'),data=p)
p_f<-p

# get "Buy" percentages based on the variable Monetary
colNames<-c("Monetary")
p<-getPercentages(train,colNames)

# get the Buy ~ Monetary model
m.glm=glm(Percentage~Monetary,family=quasibinomial(link='logit'),data=p)
p_m<-p

#plot and draw fit curves of Percentage ~ r,f,m
par(mfrow=c(1,3),oma=c(0,0,2,0))

plot(p_r$Recency,p_r$Percentage*100,xlab="Recency",ylab="Probablity of Purchasing(%)")
lines(lowess(p_r$Recency,p_r$Percentage*100),col="blue",lty=2)

plot(p_f$Frequency,p_f$Percentage*100,xlab="Frequency",ylab="Probablity of Purchasing(%)")
lines(lowess(p_f$Frequency,p_f$Percentage*100),col="blue",lty=2)

plot(p_m$Monetary,p_m$Percentage*100,xlab="Monetary",ylab="Probablity of Purchasing(%)")
lines(lowess(p_m$Monetary,p_m$Percentage*100),col="blue",lty=2)

title("Percentages ~ Recency, Frequency, Monetary", y=10,outer=TRUE)

par(mfrow=c(1,1))

model<-glm(Buy~Recency+Frequency,family=quasibinomial(link='logit'),data=train)
pred<-predict(model,data.frame(Recency=c(0),Frequency=c(1)),type='response')

## caculating the CLV for a customer with R=0,F=1,average profit=100,discount rate=0.02 for 3 periods
v<-getCLV(0,1,100,1,0,3,0.02,model)
