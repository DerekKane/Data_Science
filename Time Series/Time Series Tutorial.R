# Time Series Tutorial
# https://media.readthedocs.org/pdf/a-little-book-of-r-for-time-series/latest/a-little-book-of-r-for-time-series.pdf

#-----------------------------------------------------------------

# An example is a data set of the number of births per month in New York city, from January
# 1946 to December 1959 (originally collected by Newton).

# read in the birth data

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

# This data has to be firt turned  into time series using the TS function
# Because the data is in months, the frequency=12
# the start date is Month 1 in 1946 therefore, start=c(1946,1)

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

# review the dataset

birthstimeseries

####################################################################

# lets plot the time series

plot.ts(birthstimeseries)

# We can see from this time series that there seems to be seasonal variation in the number of births per month:
# there is a peak every summer, and a trough every winter. Again, it seems that this time series could probably be
# described using an additive model, as the seasonal fluctuations are roughly constant in size over time and do not
# seem to depend on the level of the time series, and the random fluctuations also seem to be roughly constant in
# size over time.

####################################################################

# this step will decompose this seasonal data 

library("TTR")

birthstimeseriescomponents <- decompose(birthstimeseries)

# Display the estimated value of the seasonal component.

birthstimeseriescomponents$seasonal

#-----------------------------------------------------------------
# The largest seasonal factor is for July (about 1.46), and the lowest is for February (about -2.08), indicating that there
# seems to be a peak in births in July and a trough in births in February each year.
#-----------------------------------------------------------------

# plot the decomposition of the seasonal components.

plot(birthstimeseriescomponents)

####################################################################
# Seasonally adjusting the data

# If you have a seasonal time series that can be described using an additive model, you can seasonally adjust the
# time series by estimating the seasonal component, and subtracting the estimated seasonal component from the
# original time series.

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

# plot the seasonaly adjusted dataset

plot(birthstimeseriesseasonallyadjusted)

####################################################################
####################################################################

# Read in the skirts data set

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)

skirtsseries <- ts(skirts,start=c(1866))

# Plot the series
plot.ts(skirtsseries)

# This will apply Holt-Winters Smoothing fpor Forecasts

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)

# Display the Forecasts parameter results

skirtsseriesforecasts

# The estimated value of alpha is 0.84, and of beta is 1.00. These are both high, telling us that both the estimate
# of the current value of the level, and of the slope b of the trend component, are based mostly upon very recent
# observations in the time series. This makes good intuitive sense, since the level and the slope of the time series
# both change quite a lot over time.

# this will calculate the Sum of Squared Errors  
skirtsseriesforecasts$SSE

# We can plot the original time series as a black line, with the forecasted values as a red line on top of that, by
# typing

plot(skirtsseriesforecasts)

# If you wish, you can specify the initial values of the level and the slope b of the trend component by using the
# "l.start" and "b.start" arguments for the HoltWinters() function. It is common to set the initial value of the level
# to the first value in the time series (608 for the skirts data), and the initial value of the slope to the second value
# minus the first value (9 for the skirts data). For example, to fit a predictive model to the skirt hem data using Holt's
# exponential smoothing, with initial values of 608 for the level and 9 for the slope b of the trend component, we
# type:

skirtsseriesforecasts2 <- HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
plot(skirtsseriesforecasts2)

####################################################################

# This section will be to apply a forecast based on the exponential smoothing. 

#-----------------------------------------------------------------

# Before we begin, we must create a function called plotForecastErrors
# credit to 

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
  
#-----------------------------------------------------------------

# our time series data for skirt hems was for 1866 to 1911, so we can make predictions for 1912 to 1930 (19 more data points), and plot
# them, by typing:

library("forecast")

skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19)
plot.forecast(skirtsseriesforecasts2)

# The forecasts are shown as a blue line, with the 80% prediction intervals as an blue shaded area, and the 95%
# prediction intervals as a gray shaded area.

# we can check whether the predictive model could be improved upon by
# checking whether the in-sample forecast errors show non-zero autocorrelations at lags 1-20. For example, for the
# skirt hem data, we can make a correlogram, and carry out the Ljung-Box test, by typing:

acf(skirtsseriesforecasts2$residuals, lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# Here the correlogram shows that the sample autocorrelation for the in-sample forecast errors at lag 5 exceeds
# the significance bounds. However, we would expect one in 20 of the autocorrelations for the first twenty lags to
# exceed the 95% significance bounds by chance alone. Indeed, when we carry out the Ljung-Box test, the p-value
# is 0.47, indicating that there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags
# 1-20.

# As for simple exponential smoothing, we should also check that the forecast errors have constant variance over
# time, and are normally distributed with mean zero. We can do this by making a time plot of forecast errors, and a
# histogram of the distribution of forecast errors with an overlaid normal curve:

# make a time plot

plot.ts(skirtsseriesforecasts2$residuals) 

# make a histogram

plotForecastErrors(skirtsseriesforecasts2$residuals) 