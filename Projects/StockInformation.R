# This is to extract Financial data related to stock prices
# http://www.quantmod.com/examples/intro/

install.packages("quantmod")

# Open the library 

library(quantmod)

# This is how to get some data from the web. We are going to get stock information from Yahoo 
# as YHOO from Google Finance and Google from Yahoo Finance.

getSymbols("YHOO",src="google") # source is from google finance 

# getSymbols("GOOG",src="yahoo") # from yahoo finance 
# getSymbols("DEXJPUS",src="FRED") # FX rates from FRED 
# getSymbols("XPT/USD",src="Oanda") # Platinum from Oanda 

# This will produce a bar chart for the Yahoo Stock in R

barChart(YHOO)

# Add multi-coloring and change background to white 

candleChart(YHOO,multi.col=TRUE,theme="white")

# This section will show how to get the platnium in USD

getSymbols("XPT/USD",src="oanda")
chartSeries(XPTUSD,name="Platinum (.oz) in $USD") 

# Platinum, now weekly with custom color candles using the quantmod function to.weekly 

chartSeries(to.weekly(XPTUSD),up.col='white',dn.col='blue') 

# Create a quantmod object for use in later model fitting. Note there is no need to load the data before hand. 

setSymbolLookup(SPY='yahoo',+ VXN=list(name='^VIX',src='yahoo'))

mm <- specifyModel(Next(OpCl(SPY)) ~ OpCl(SPY) + Cl(VIX)) 
modelData(mm) 
