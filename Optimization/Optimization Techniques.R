#############################################################################
# Optimization Techniques Tutorial
#############################################################################

# Set a working directory
setwd("C:/Users/Derek/Documents/R Scripts/Optimization")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linearly Constrained Optimization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is the function to be maximized subjected to a single constraint that X > 0.

f = function (x) {
  term = x*(2^(x-1))*exp(-2^x)
  return(-term) # since the function will be minimized by default we reverse the value
}

# Step 2. Choose the initial solution

init = 1

# Step 3. run the optimization

constrOptim(init, f, grad=NULL, ui=1, ci=0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simple Linear Constraint Tutorial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# https://icyrock.com/blog/2013/12/linear-programming-in-r-using-lpsolve/

# Problem

# Suppose a farmer has 75 acres on which to plant two crops: wheat and barley. 
# To produce these crops, it costs the farmer (for seed, fertilizer, etc.) $120 per acre 
# for the wheat and $210 per acre for the barley. The farmer has $15000 available for expenses. 
# But after the harvest, the farmer must store the crops while awaiting favourable market conditions. 
# The farmer has storage space for 4000 bushels. Each acre yields an average of 110 bushels 
# of wheat or 30 bushels of barley. If the net profit per bushel of wheat (after all expenses have been subtracted) 
# is $1.30 and for barley is $2.00, how should the farmer plant the 75 acres to maximize profit?

# Mathematical Formulation

# maximize
#     P = (110)(1.30)x + (30)(2.00)y = 143x + 60y
# subject to:
# 120x + 210y <= 15000
# 110x + 30y <= 4000
# x + y <= 75
# x >= 0
# y >= 0

# install.packages("lpSolveAPI")

# Load lpsolve library
library("lpSolveAPI")

# Represent our problem

lprec <- make.lp(0, 2)  # the 2 represents 2 variables, x and y | the 0 represents a value greater than 0 for x/y
lp.control(lprec, sense="max") # This is the establishing the LP as a maximization problem
set.objfn(lprec, c(143, 60)) # We are maximizing 163X plus 60y in a vector format

# Establish Constraints
add.constraint(lprec, c(120, 210), "<=", 15000)
add.constraint(lprec, c(110, 30), "<=", 4000)
add.constraint(lprec, c(1, 1), "<=", 75)

# Display the lpsolve matrix

lprec

# Solve the optimization problem

solve(lprec)

# Get maximum profit
get.objective(lprec)

# Get the solution

get.variables(lprec)


# Thus, to achieve the maximum profit ($6315.625), the farmer should plant 21.875 acres of wheat and 53.125 acres of barley.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intermediate Linear Constraint Tutorial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# https://www.r-bloggers.com/linear-programming-in-r-an-lpsolveapi-example/

# The example case:

# A trading company is looking for a way to maximize profit per transportation 
# of their goods. The company has a train available with 3 wagons. When stocking
# the wagons they can choose between 4 types of cargo, each with its own specifications. 
# How much of each cargo type should be loaded on which wagon in order to maximize profit?

# The following constraints have to be taken in consideration;

# Weight capacity per train wagon
# Volume capacity per train wagon
# Limited availability per cargo type

library(lpSolveAPI)

#used for result visualization
library(ggplot2)
library(reshape)
library(gridExtra)

#define the datasets

train<-data.frame(wagon=c('w1','w2','w3'), weightcapacity=c(10,8,12), spacecapacity=c(5000,4000,8000))

cargo<-data.frame(type=c('c1','c2','c3','c4'), available=c(18,10,5,20), volume=c(400,300,200,500),profit=c(2000,2500,5000,3500))

# Next, we start with building the actual lp model, our goal is to end up with the following model:

#create an LP model with 10 constraints and 12 decision variables (W*C or 3*4=12)

lpmodel<-make.lp(2*NROW(train)+NROW(cargo),12)

#I used this to keep count within the loops, I admit that this could be done a lot neater
column<-0
row<-0

#build the model column per column
for(wg in train$wagon){
  row<-row+1
  for(type in seq(1,NROW(cargo$type))){
    column<-column+1
    
    #this takes the arguments 'column','values' & 'indices' (as in where these values should be placed in the column)
    set.column(lpmodel,column,c(1, cargo[type,'volume'],1), indices=c(row,NROW(train)+row, NROW(train)*2+type))
  }}

#set rhs weight constraints
set.constr.value(lpmodel, rhs=train$weightcapacity, constraints=seq(1,NROW(train)))

#set rhs volume constraints
set.constr.value(lpmodel, rhs=train$spacecapacity, constraints=seq(NROW(train)+1,NROW(train)*2))


#set rhs volume constraints
set.constr.value(lpmodel, rhs=cargo$available, constraints=seq(NROW(train)*2+1,NROW(train)*2+NROW(cargo)))

#set objective coefficients
set.objfn(lpmodel, rep(cargo$profit,NROW(train)))

#set objective direction
lp.control(lpmodel,sense='max')

#I in order to be able to visually check the model, I find it useful to write the model to a text file
write.lp(lpmodel,'model.lp',type='lp')

# (Review the lp file)


# Now to see if the model will solve:

#solve the model, if this return 0 an optimal solution is found
solve(lpmodel)

#this return the proposed solution
get.objective(lpmodel)

# The value of the solution is returned by get.objective(lpmodel), this shows a maximum total profit of 107500$

# Get the solution

get.variables(lpmodel)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Price Optimization Tutorial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://decisionfirst.files.wordpress.com/2015/10/price-optimization-with-sap-predictive-analytics-2.pdf


# For this particular use case, the pricing department for our manufacturing company would like to know
# how to best price each of our products for the next year. For this simple case, we'll use 9 products, and
# include the product name (or SKU identifier), the current annual volume sold, current price of the
# product, and current profit margin.

# Some of the products are currently unprofitable (as evidenced by the negative profit margin), so
# management would like to re-set prices for next year to ensure that:

#   1.) No product has a price increase or decrease of over 10%
#   2.) Any product that is currently unprofitable has a minimum price that is break-even for profit
#   3.) Overall, given the same volumes as this year, total revenue will increase by 2-4%


# Load the dataset for analysis

mydata <- read.csv("C:/Users/Derek/Documents/R Scripts/Optimization/tiny_example_data.csv")

# Remove Max and Min Variables

mydata$MaxPrice <- NULL
mydata$MinPrice <- NULL


# Linear programming is a very popular optimization algorithm, and
# will ensure that all of our required business rules are met. In order to use this custom component, you
# must install the lpSolveAPI R package in your R installation.
# In an effort to avoid diving into too much technical detail for the code that supports this very specific
# use case, I will simply outline the functionality contained in the custom R component:
  

# Step 1 

# Calculate the minimum and maximum acceptable prices for each product based on the business
# rules (max is 10% increase, the minimum is either a 10% price decrease or the current price -
#       (current price * profit margin), whichever is greater). For example, Shirts, which have a 2% 
# profit margin, can sustain a maximum price of $4.26 (10% higher than the current price of $3.87)
# or a minimum price of $3.81 (the break-even profit price).

mydata$MaxPrice <- mydata$CurrPrice*1.10

mydata$MinPrice <- if (mydata$CurrPrice*0.90 > mydata$CurrPrice-(mydata$CurrPrice*mydata$CurrProfit)){
  mydata$CurrPrice*0.90
} else {
  mydata$CurrPrice-(mydata$CurrPrice*mydata$CurrProfit)
}

# Step 2
# Create constraints for each product in the form of: $3.81 <= Price(Shirt) <= $4.26

# Load lpsolve library
# library("lpSolveAPI")

# Declare variables for simulation

maxPriceLvl <- 0.04
minPriceLvl <- 0.02
Obj <- "max"

library(lpSolveAPI);

PriceOpt<-function(tab, SKUs, CurrVol, CurrPrice, CurrProfit, minPriceLvl, maxPriceLvl, Obj){
  
  
  
  tab <- data.frame(Part=mydata$PRODUCT, 
                  Curr.Vol=mydata$CurrVol,
                  Curr.Price=mydata$CurrPrice, 
                  Curr.Profit=mydata$CurrProfit, 
                  Max.price=mydata$MaxPrice
  );
  
  
  
  numProds=length(tab$Curr.Price);
  
  #calculate min price
  minp=tab$Curr.Price;
  for(i in 1:numProds){
    if(minp[i]*0.9<(1-tab$Curr.Profit[i])*minp[i]){minp[i]<-(1-tab$Curr.Profit[i])*minp[i]}
    else{minp[i]<-minp[i]*0.9}
    
  };
  
  
  tab$Min.price<-minp;
  
  
  #Create constraing labels
  ProdVec=seq(from = 1,  to = numProds, by = 1);
  L_const_names = paste(ProdVec, "Lower Constraint");
  U_const_names = paste(ProdVec, "Upper Constraint");
  L_const_names = c(L_const_names, "Overall Lower");
  U_const_names = c(U_const_names, "Overall Upper");
  NumConstraints=2*numProds+2;
  #build empty RowNames Vector
  RowNames<-vector(mode="character", length=NumConstraints);
  
  #assign constraing Names
  for (i in 1:NumConstraints) {
    if (i%%2 == 0){
      RowNames[i]=U_const_names[i/2]
    } else{
      RowNames[i]=L_const_names[floor(i/2)+1]
    }
    
  }
  
  
  
  lprecH <- make.lp(0, dim(tab)[1]);
  set.objfn(lprecH, tab$Curr.Vol);
  
  #add max and min price by part constraints
  for (i in 1:dim(tab)[1] ) {
    vec<-seq(1, dim(tab)[1] )*0
    vec[i]=1
    add.constraint(lprecH, vec, ">=", tab$Min.price[i])
    add.constraint(lprecH, vec, "<=", tab$Max.price[i])
  };
  
  
  overallmax=sum(tab$Curr.Price*tab$Curr.Vol)*(1+as.numeric(maxPriceLvl));
  overallmin=sum(tab$Curr.Price*tab$Curr.Vol)*(1+as.numeric(minPriceLvl));
  
  
  #overall price change constraint = between 2 and 4%
  add.constraint(lprecH, tab$Curr.Vol, "<=", overallmax);
  add.constraint(lprecH, tab$Curr.Vol, ">=", overallmin);
  
  
  
  lp.control(lprecH,sense=Obj);
  
  
  ColNames<-paste("Product", ProdVec);
  
  dimnames(lprecH) <- list(RowNames, ColNames);
  
  #print(lprecH)
  
  
  solve(lprecH);
  get.objective(lprecH);
  get.variables(lprecH);
  # write.lp(lprecH,'C:\\modelH.lp',type='lp');
  
  
  
  par(mfrow=c(3,2));
  #plot old and new total revenue
  #par(mfrow=c(1,1));
  rev<-c(sum(tab$Curr.Price*tab$Curr.Vol), get.objective(lprecH));
  bp<-barplot(rev, main="Overall Revenue Change",  names=c("Current", "Proposed"), horiz=TRUE,
              xlab=paste("overall revenue change: ", sprintf("%.2f%%", (rev[2]/rev[1]-1)*100)));
  text(bp, 0, sprintf("$%.2f",rev), horiz=TRUE, cex=1, pos=3);
  
  
  currrevbypart<-tab$Curr.Price*tab$Curr.Vol
  proprevbypart<-get.variables(lprecH)*tab$Curr.Vol
  barplot(as.matrix(cbind(currrevbypart, proprevbypart)), col=rainbow(numProds), horiz=TRUE, main="Overall Revenue Change by SKU",
          names=c("Current", "Proposed"), xlab=paste("overall revenue change: ", sprintf("%.2f%%", (rev[2]/rev[1]-1)*100)))
  legend("center", fill=rainbow(numProds), horiz=TRUE, title="SKU", legend=tab$Part)
  
  
  #plot old price by SKU
  #partname<-matrix(seq(from = 1,  to = numProds, by = 1), nrow=10, ncol=1);
  partname<-matrix(tab$Part);
  pricebypartcurr<- tab$Curr.Price;
  pricebypartprop<-get.variables(lprecH);
  pricebypart<-c(pricebypartcurr, pricebypartprop);
  #par(mfrow=c(2,1));
  bp2curr<-barplot(pricebypartcurr, main="Price by SKU--Current", names=c(partname), xlab="SKU Number", col=rainbow(numProds));
  text(bp2curr, 0, sprintf("$%.2f", pricebypartcurr), cex=1, pos=3);
  
  
  #plot price change by SKU
  
  pricechgbypart<-c(pricebypartprop-pricebypartcurr);
  bp3<-barplot(pricechgbypart, main="Price Change by SKU ($)", names=c(partname), xlab="SKU Number", col=rainbow(numProds));
  text(bp3, 0, sprintf("$%.2f", pricechgbypart), cex=1, pos=3);
  
  
  #plot new price by SKU
  
  bp2prop<-barplot(pricebypartprop,  names=c(partname), main="Price by SKU--Proposed", xlab="SKU Number", col=rainbow(numProds));
  text(bp2prop, 0, sprintf("$%.2f", pricebypartprop), cex=1, pos=3);
  
  
  #plot pct price change by SKU
  pricechgbypart2<-c((pricebypartprop/pricebypartcurr-1)*100);
  bp4<-barplot(pricechgbypart2, main="Price Change by SKU (%)", names=c(partname), xlab="SKU Number", col=rainbow(numProds));
  text(bp4, 0, sprintf("%.0f%%", pricechgbypart2), cex=1, pos=3);
  
  print(rev);
  
  out<-cbind(tab, get.variables(lprecH));
  return(list(output=out, model=lprecH));
}














# Get the solution

get.variables(lprec)


# Thus, to achieve the maximum profit ($6315.625), the farmer should plant 21.875 acres of wheat and 53.125 acres of barley.





constraint1 = (mydata$MinPrice <= mydata$CurrPrice <= mydata$MaxPrice)  



mydata <-data.frame(PRODUCT=c('Shirt','Pants','Jacket', 'Socks', 'Hat', 'Dress', 'Necklace', 'Earring','Wallet',
                              'Hairclip', 'Bracelet', 'Shoes', 'Shorts', 'Tie', 'Scarf'), 
                    CurrVol=c(490.726, 213.60007, 611.30530, 412.46491, 703.01489, 514.52871, 772.99246, 320.01358,
                              751.25310, 17.49786, 288.27874, 133.30393, 462.84954, 687.17171, 126.30996), 
                    CurrPrice=c(3.87, 7.33, 2.36, 4.84, 1.21, 3.57, 0.34, 6.00, 0.61, 9.78, 6.40, 8.33, 4.21, 1.41, 8.42),
                    CurrProfit=c(0.016226465, -0.027278599, -0.029319706, 0.242550658, 0.001857236, 0.122904002, 0.193949035,
                                 -0.072064944, -0.013124629, -0.026675148, -0.024014913, 0.367460394, 0.257394103, 0.097980519, 0.278531436))


# https://decisionfirst.files.wordpress.com/2015/10/price-optimization-with-sap-predictive-analytics-2.pdf
