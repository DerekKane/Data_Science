# demonstration of R access to and display of financial data from FRED
# requires a connection to the Internet

# begin by installing the package quantmod 

library(quantmod)

# here we demonstrate the wonders of FRED 
# ecomonic research data from the Federal Reserve Bank of St. Louis 
# see documentation of tags at http://research.stlouisfed.org/fred2/
# if you choose a particular series and click on it a graph will be displayed
# in parentheses in the title of the graph will be the symbol for the financial series
# some time series are quarterly, some monthly... others weekly

# here we show how to download the Consumer Price Index
# for All Urban Consumers: All Items, Not Seasonally Adjusted, Monthly
getSymbols("CPIAUCNS",src="FRED",return.class = "xts")
print(str(CPIAUCNS)) # show the structure of this xtx time series object
# plot the series
chartSeries(CPIAUCNS,theme="white")

# M1 money stock time series not seasonally adjusted
getSymbols("M1NS",src="FRED",return.class = "xts")
print(str(M1NS)) # show the structure of this xtx time series object
# plot the series
chartSeries(M1NS,theme="white")

# Real Gross National Product in 2005 dollars
getSymbols("GNPC96",src="FRED",return.class = "xts")
print(str(GNPC96)) # show the structure of this xtx time series object
# plot the series
chartSeries(GNPC96,theme="white")

# National Civilian Unemployment Rate, not seasonally adjusted (monthly, percentage)
getSymbols("UNRATENSA",src="FRED",return.class = "xts")
print(str(UNRATENSA)) # show the structure of this xtx time series object
# plot the series
chartSeries(UNRATENSA,theme="white")

# California Civilian Unemployment Rate, not seasonally adjusted (monthly, percentage)
getSymbols("CAURN",src="FRED",return.class = "xts")
print(str(CAURN)) # show the structure of this xtx time series object
# plot the series
chartSeries(CAURN,theme="white")

# Loans and Leases in Bank Credit for All Commercial Banks, not seasonally adjusted
getSymbols("LOANSNSA",src="FRED",return.class = "xts")
print(str(LOANSNSA)) # show the structure of this xtx time series object
# plot the series
chartSeries(LOANSNSA,theme="white")

# Savings Deposits at All Depository Institutions, not seasonally adjusted (weekly)
getSymbols("WSAVNS",src="FRED",return.class = "xts")
print(str(WSAVNS)) # show the structure of this xtx time series object
# plot the series
chartSeries(WSAVNS,theme="white")

# Return on Average Assets for all U.S. Banks, not seasonally adjusted (quarterly percentage)
getSymbols("USROA",src="FRED",return.class = "xts")
print(str(USROA)) # show the structure of this xtx time series object
# plot the series
chartSeries(USROA,theme="white")

# Delinquency Rate On Credit Card Loans, All Commercial Banks, not seasonally adjusted (quarterly,percentage)
getSymbols("DRCCLACBN",src="FRED",return.class = "xts")
print(str(DRCCLACBN)) # show the structure of this xtx time series object
# plot the series
chartSeries(DRCCLACBN,theme="white")

# Number of Commercial Banks in the United States, not seasonally adjusted (quarterly)
getSymbols("USNUM",src="FRED",return.class = "xts")
print(str(USNUM)) # show the structure of this xtx time series object
# plot the series
chartSeries(USNUM,theme="white")

# University of Michigan: Consumer Sentiment, not seasonally adjusted (monthly, 1966 = 100)
getSymbols("UMCSENT",src="FRED",return.class = "xts")
print(str(UMCSENT)) # show the structure of this xtx time series object
# plot the series
chartSeries(UMCSENT,theme="white")

# New Homes Sold in the US, not seasonally adjusted (monthly, thousands)
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
print(str(HSN1FNSA)) # show the structure of this xtx time series object
# plot the series
chartSeries(HSN1FNSA,theme="white")

