# Time Series - Adventureworks Tutorial
###########################################################################

# http://scn.sap.com/docs/DOC-46927

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/Time Series")

###########################################################################

# The name of the dataset is now called AdWorks

mydata <- read.csv("AdventureWorks.csv")

###########################################################################

## Forecast multiple time-series in bulk, but individually with the auto.arima function from the forecast package,
## which finds the best Arima model for a time series.

## Prerequisites
## - The historic data is monthly with two numerical columns for year and month. 
##   Use a "Data Type Definition" component from SAP Predictive Analsis if need to change the date type of a column.
## - The data is already aggregated by year and month and the column that separates the time series 
##   (ie 'Product' in case your data includes multiple time series by product)
## - No missing data in the time series.

mymain <- function (mydata, strColMeasure, strColForecastBy, strColYear, strColMonth, iMonthsToForecast, iConfidenceLevel)
{
  library(forecast)
  
  ## Count how many time series need to be forecasted
  strTimeSeriesNames = unique(mydata[, strColForecastBy])
  iTimeSeriesCount <- length(strTimeSeriesNames)
  
  ## Allow for maximum of three charts
  iChartCount <- min(3, iTimeSeriesCount)
  par(mfrow=c(iChartCount,1))
  
  ## Bring historic data from input data into format to union later with forecasted data
  myactuals <- data.frame(Year=mydata[, strColYear], Month=mydata[, strColMonth], YearMonthString=paste(as.character(mydata[, strColYear]), sprintf("%02d", mydata[, strColMonth]), sep='-'), ForecastBy=mydata[, strColForecastBy], Measure=mydata[ ,strColMeasure] , Type='Actuals', PILower=0, PIUpper=0, Model='')
  
  ## Iterate through each historic time series and forecast individually   
  mysubset <- NULL
  myts <- NULL
  myets <- NULL
  myforecast <- NULL
  futuremonths <- NULL
  mymonths <- NULL
  myyears <- NULL
  mytempfutureset <- NULL
  myfuturesetcollection <- NULL
  for (i in 1:iTimeSeriesCount)
  {
    ## Create an individual and sorted dataset for the current time series
    mysubset <- mydata [mydata[, strColForecastBy]==strTimeSeriesNames[i],]
    mysubset<- mysubset[order(mysubset[, strColYear], mysubset[, strColMonth]),]  
    
    ## Create a time series for the current dataset
    myts <- ts(mysubset[, strColMeasure], start=c(mysubset [1, strColYear], mysubset [1, strColMonth]), end=c( mysubset[nrow(mysubset),strColYear],  mysubset[nrow(mysubset),strColMonth]), frequency=12) 
    
    ## Find the best Arima model
    myarima <- auto.arima(myts)
    
    ## Carry out the forecast
    myforecast <- forecast(myarima, level=iConfidenceLevel, h=iMonthsToForecast)
    
    ## Plot chart if within maxiumum of previously defned number of charts
    if (i <= iChartCount)
    {plot(myforecast, main=strTimeSeriesNames[i])}
    
    ## Deduct from forecasted time series the year and month values
    futuremonths <- 0:(iMonthsToForecast-1)
    mymonths <- (start(myforecast$mean)[1] *12 +start(myforecast$mean)[2] + futuremonths) 
    myyears <- as.integer((mymonths-1) / 12)
    mymonths <- as.integer(mymonths %% 12)
    mymonths[which(mymonths==0)] <- 12
    
    ## Bring forecast into format for union with actuals 
    mytempfutureset <- data.frame(Year=myyears, Month=mymonths, YearMonthString=paste(as.character(myyears), sprintf("%02d",mymonths), sep='-'), ForecastBy=strTimeSeriesNames[i], Measure=as.numeric(myforecast$mean),  Type='Forecast', PILower=as.numeric(myforecast$lower), PIUpper=as.numeric(myforecast$upper), Model=capture.output(myarima)[2])     
    
    ## Union current forecast with previously forecasted time series
    myfuturesetcollection <- rbind(mytempfutureset, myfuturesetcollection)
  }
  
  ## Union actuals and forecasts for all time series
  output <- rbind(myactuals, myfuturesetcollection)   
  
  ## Return the actuals and forecasts
  return(list(out=output))
}
