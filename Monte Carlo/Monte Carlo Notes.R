############################################################################
# Monte Carlo Simulation - Revenue from ASP and Qty predictions
############################################################################


# We must first declare our starting parameters for the simulation.


Lower.ASP <- 0.315 # Lower Bound for ASP
Upper.ASP <- 0.365 # Upper Bound for ASP

Lower.Qty <- 350000 # Lower Bound for the Qty
Upper.Qty <- 450000 # Upper Bound for the Qty


############################################################################
# Calculate the Totals
############################################################################

Total.Lower <- Lower.ASP * Lower.Qty
Total.Upper <- Upper.ASP * Upper.Qty

average <- mean(Total.Lower:Total.Upper)

# Total.Lower <- 62300
# Total.Upper <- 85200

############################################################################
# Determine the Standard Deviation of the Population
############################################################################

# Create the vector of values:

foo <- c(Total.Lower,Total.Upper, average)

# Here is the standard deviation of the population

popSD <- function(x) {
  return(sqrt(sum((x - mean(x))^2) / length(x)))
}

std.p <- popSD(foo)


############################################################################
# Determine the Minimum Number of Iterations for an error of 2%  or less
############################################################################

randvar <- average / 50 # the divide by 50 produces the 2% error rate

min.iterations <- ceiling(((3 *std.p)/randvar)^2) # ceiling is the excel roundup function 


############################################################################
# Monte Carlo Simulation
############################################################################

# Execute with 2% error rate - Minimal Iterations Runs

# runs <- min.iterations


# ASP.sample <- runif(runs, min=Lower.ASP, max=Upper.ASP)
# Qty.sample <- runif(runs, min=Lower.Qty, max=Upper.Qty)

# Revenue <- ASP.sample*Qty.sample 

# MC.Estimation <- mean(Revenue)
# MC.Upper95 <- MC.Estimation + popSD(Revenue)
# MC.Lower95 <- MC.Estimation - popSD(Revenue)

# Execute Monte Carlo Simulation with 100,000 iterations

runs <- 100000

ASP.sample <- runif(runs, min=Lower.ASP, max=Upper.ASP)
Qty.sample <- runif(runs, min=Lower.Qty, max=Upper.Qty)

Revenue <- ASP.sample*Qty.sample 

MC.Estimation <- mean(Revenue)
MC.Upper95 <- MC.Estimation + popSD(Revenue)
MC.Lower95 <- MC.Estimation - popSD(Revenue)