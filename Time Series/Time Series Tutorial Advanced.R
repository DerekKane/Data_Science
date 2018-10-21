# Time Series - Tutorial
###########################################################################

# http://www.inside-r.org/howto/time-series-analysis-and-order-prediction-r

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/Time Series")

###########################################################################

# Load the orderts dataset. The orders column has been normalized to show a value 
# between 0 and 1 in respect to the year.

load("C:/Users/Derek/Documents/RPackages/Time Series/orderts.RData")

###########################################################################

# Inspect the dataset

head(orderts, n=10)

###########################################################################

# Graph the existing orders.

library("ggplot2")
library("grid")
qplot(week, orders, data = orderts, colour = as.factor(year), geom = "line")


###########################################################################

# By Quarter Histogram

qplot(week, data=orderts, colour=as.factor(year), binwidth=0.5) + facet_wrap(~ quarter)

###########################################################################

# There was one more business week in the first quarter of 2009 than in the first quarter 
# of 2010 and 2011; consequently, there is a consistent one week difference between the last weeks of the quarters. However, due to the sudden surge of orders, 
# from the business point of view this is exactly the week that counts the most.

# The model building approaches we will use later can usually cope with such a one-week 
# offset discrepancy; for quick and dirty data exploration here let's get simply rid of the 
# 13-th week of 2009 Q1. Also, to support visualization we introduce a "week of current quarter" variable.

orderts2 <- cbind(orderts[-13,], weekinq=c(1:117))
prev <- orderts2[1,]
runvar <- 1
for(i in 2:nrow(orderts2)){
  current <- orderts2[i,]
  orderts2[i,"weekinq"] <- ifelse(prev$quarter == current$quarter, runvar+1, 1)
  runvar <- ifelse(prev$quarter == current$quarter, runvar+1, 1)
  prev <- current
}
rm(prev, current, runvar, i)


###########################################################################

# Let us compare the orders now on a quarter by quarter basis:

qplot(weekinq, orders, data = orderts2, colour = as.factor(year), geom = "line") +
  facet_wrap(~quarter)

# These plots tell us the following:
#   At the very end of the quarter, there is always a significant spike in the orders; for Q3 and Q4, even the exact amounts are very close.
#   During the quarters, there is also significant similarity between the orders.
#   On the whole, the time series seems to be stationary and highly periodic; therefore, it should be worthwile to analyse its characteristics in the frequency domain.

###########################################################################

# The following code performs the common Fast Fourier Transformation

# Visual inspection hinted at a strong periodicity in the time series at the quarter, 
# half year and year intervals. In order to prove this suspicion, we now perform a 
# brief power spectrum analysis of the first two years of the data.

f <- data.frame(coef = fft(orderts2[1:104, "orders"]), freqindex = c(1:104))
qplot(freqindex, Mod(coef), data = f[2:53,], geom = "line")

# This results in the following "power spectrum" plot that uses the complex modulus to measure "magnitude":

# It is apparent that a number of frequencies have significantly higher amplitude than the others, 
# hinting at a strong underlying periodic nature in the data. Let's identify these peaks:

f[Mod(f$coef) > 3 & f$freqindex < 53, "freqindex"] - 1

# What we see is that a component with the frequency of 8 / 104 (2 * 4 quarters in the two years) and its harmonics 
# (note the exactly 8 difference between the peaks) seem to dominate the signal. 
# However, these frequencies alone are insufficient to capture the time series 
# to the precision we would like to. To demonstrate this, the following code 
# segment eliminates the frequencies with "small magnitude" in a copy of the 
# Fourier transform. The remaining ones are transformed back to the time domain 
#(an inverse FFT call) in order to be compared to the original time series.

###########################################################################

peaks <- Mod(f$coef) > 3
ffilt <- f
ffilt[!peaks, "coef"] <- 0
ffilt <- data.frame(index=ffilt$freqindex, value=Re(fft(ffilt$coef, inverse=TRUE))/104, type=rep("filtered", times=104))
ffilt <- rbind(ffilt, data.frame(index=seq(1:104), value=orderts2[1:104,"orders"], type=rep("original", times=104)))

# insert GG Plot graphic for ffilt


###########################################################################
# Forecasting Approach

# The signal we deal with shows strong regularity, but is at the same time highly 
# complex (and decidedly nonlinear). Therefore, for forecasting purposes we 
# split it into simpler periodic time series and train neural networks for the 
# finite time window forecasting of each simplified component. The "splitting" 
# is based on a non-overlapping partitioning of the frequency domain.


###########################################################################

# We split the frequency domain of the time series into intervals so that each interval 
# contains either the fundamental frequency of the strong periodic signal or one 
# harmonic of it. This is effectively a band-pass filtering based decomposition:

midindex <- ceiling((length(f$coef)-1)/ 2) + 1
peakind <- f[abs(f$coef) > 3 & f$freqindex > 1 & f$freqindex < midindex,]
lindex <- length(f$coef)

lowerind <- 1

subsignals <- lapply(c(peakind$freqindex, midindex+1), function(x){
  upperind <- x
  fsub <- f
  notnullind <- ((fsub$freqindex >= lowerind
                  & fsub$freqindex < upperind)
                 |
                   (fsub$freqindex >  (lindex - upperind + 2)
                    & fsub$freqindex <= (lindex - lowerind + 2)))
  fsub[!notnullind,"coef"] <- 0
  lowerind <<- upperind
  Re(fft(fsub$coef, inverse=TRUE)/length(fsub$coef))
})

# The code produces the time series signals belonging to the harmonics-defined 
# frequency bands. For the above two year time series and 
# harmonic set, this produces the following decomposition in the time domain:

grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

psig <- function(x, y, z){
  h <- data.frame(index = c(1:length(subsignals[[x]])),
                  orders = subsignals[[x]])
  lab <- paste("Subseries ", as.character(x), sep="")
  print(qplot(index, orders, data = h, geom = "line", main=lab), vp = vplayout(y,z))
  TRUE
}

psig(1,1,1); psig(2,1,2); psig(3,2,1); psig(4,2,2); psig(5,3,1); psig(6,3,2); psig(7,4,1)


###########################################################################
# Neural network training

# We use the "Multilayer Perceptron" (MLP) feedforward artificial neural network model to map a finite time window of order observations onto predictions about the orders that 
# can be expected in the future. We preferred nonlinear modeling against linear 
# predictors, since we assumed the latter one could not catch the information 
# underlying our data. Some other notable advantages of MLP models are the following.
#       Universal function approximator: theoretically any function can be learned by an MLP.
#       Only a small number of arguments (in our case, the number of neurons in the so-called "hidden layer") has to be chosen.
#       Powerful support in R (via the nnet package).

library("nnet")

# number of hidden neurons - trial and error

nn.sizes <- c(4,2,3,3,3,2,2,2)

###########################################################################

numofsubs <- length(subsignals)
twindow <- 4

offsettedsubdfs <- lapply(1:numofsubs, function(x){
  singleoffsets <- lapply(0:(twindow-1), function(y){
    subsignals[[x]][(twindow-y):(length(subsignals[[x]])-y-1)]
  })
  a <- Reduce(cbind, singleoffsets)
  names <- lapply(1:twindow, function(y){paste("TS", as.character(x), "_", as.character(y), sep = "")})
  b <- as.data.frame(a)
  colnames(b) <- names
  b
})

# For later use we calculate the number of samples in our dataset.

sample.number <- length(offsettedsubdfs[[1]][,1])

# After this, we use the input to actually train the neural networks.

#the neural networks

nns <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  nn <- nnet(offsettedsubdfs[[i]][1:(sample.number),], #the training samples
             
             subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], #the output
             
             #corresponding to the training samples
             
             size=nn.sizes[i], #number of neurons
             
             maxit = 1000, #number of maximum iteration
             
             linout = TRUE) #the neuron in the output layer should be linear
  
  #the result of the trained networks should be plotted
  
  plot(subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], type="l")
  
  lines(nn$fitted.values,type="l",col="red")
  
  nn
  
})

# The red line is the response of the network, 
# and the black is the original time series. All training data fits well, 
# and the response of the neural networks at the training samples is accurate. 
# The MSE (mean square error) for all training data is smaller than 10^(-5).

# Now we have a set of neural network predictors that each have learned 
# a "part" of the original signal - an quite 
# well to that. And as the Fourier-transform is a linear operation, the sum 
# of the individual predictions will give the full time series prediction back.

###########################################################################
# Forecasting

# At this point we have a composite predictor that is able to predict the order amount for 
# the first week of 2011. Actually, it is able to predict any week of the year
# - provided that the observations for the previous weeks are available. 

# We have mentioned earlier that we have used only the first 8 quarters as training data. 
# Now we will use the data for the last, 
# ninth quarter to check how well we can predict with a one quarter horizon:

number.of.predict <- 14

#long term prediction

long.predictions <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  prediction <- vector(length=number.of.predict, mode="numeric")
  
  #initial input
  
  input <- offsettedsubdfs[[i]][sample.number,]
  
  for (j in 1 : number.of.predict)
    
  {
    
    prediction[j] <- predict(nns[[i]], input)
    
    input <- c(prediction[j],input[1:(length(input)-1)])
    
  }
  
  #we want to plot the prediction
  
  plot(c(nns[[i]]$fitted.values,prediction), type="l",col="red")
  
  lines(subsignals[[i]][(twindow+1):length(subsignals[[i]])])
  
  prediction
  
})


###########################################################################

df <- data.frame(matrix(unlist(long.predictions), nrow=7, byrow=T))


###########################################################################