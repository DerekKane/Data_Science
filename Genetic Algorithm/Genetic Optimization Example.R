################################################################
# Genetic Algorithm - Stock Prices
################################################################

# http://stackoverflow.com/questions/17476203/genetic-algorithm-optimization

################################################################
# Load the libraries

library(genalg)
library(RCurl)
library(quantmod)

################################################################
# Objective Function
################################################################

div.ratio <- function(weight, vol, cov.mat){
  weight <- weight / sum(weight)
  dr <- (t(weight) %*% vol) / (sqrt(t(weight) %*% cov.mat %*% (weight)))  
  return(-dr)
}

################################################################

rm(list=ls())
require(RCurl)
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz',     binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con = gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)
load.packages('quantmod')


data <- new.env()

tickers<-spl("VTI,VGK,VWO,GLD,VNQ,TIP,TLT,AGG,LQD")
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na', dates='1990::2013')

prices<-data$prices[,-10]  
ret<-na.omit(prices/mlag(prices) - 1)
vol<-apply(ret,2,sd)
cov.mat<-cov(ret)

out <- optim(par     = rep(1 / length(vol), length(vol)),  # initial guess
             fn      = div.ratio,
             vol     = vol,
             cov.mat = cov.mat,
             method  = "L-BFGS-B",
             lower   = 0,
             upper   = 1)

opt.weights <- out$par / sum(out$par) #optimal weights

################################################################
################################################################