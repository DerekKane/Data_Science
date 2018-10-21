############################################################################
# Monte Carlo Simulation - Revenue from ASP and Qty predictions
############################################################################


# Remove all objects from Global Environment.
rm(list=ls())

# Supress warning messages.
options(warn=-1)

# Set a working directory
setwd("C:/Users/Derek/Documents/R Scripts/Monte Carlo")

# Load the csv file.

mydata <- read.csv("C:/Users/Derek/Documents/R Scripts/Monte Carlo/MonteCarlo.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a temp table to hold the results.

MC.Results <- data.frame(Dimension1=character(),
                         Dimension2=character(),
                         MC.Estimation=numeric(),
                         MC.Lower=numeric(),
                         MC.Upper=numeric(),
                         stringsAsFactors=FALSE) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the for loop for the simulation.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

runs <- 100000

for (i in 1:nrow(mydata)) {
  
  # Add the dimensions to the table.
  
  Dimension1 <-  mydata$Dimension1[i] 
  Dimension2 <-  mydata$Dimension2[i]
  
  # Declare variables for the simulation
  Lower.ASP <- mydata$ASP.Lower[i]
  Upper.ASP <- mydata$ASP.Upper[i]
  Lower.Qty <- mydata$Qty.Lower[i]
  Upper.Qty <- mydata$Qty.Upper[i]
  
  Total.Lower <- Lower.ASP * Lower.Qty
  Total.Upper <- Upper.ASP * Upper.Qty
  average <- mean(Total.Lower:Total.Upper)
  
  foo <- c(Total.Lower,Total.Upper, average)
  
  popSD <- function(x) {
    return(sqrt(sum((x - mean(x))^2) / length(x)))
  }
  
  std.p <- popSD(foo)
  
  # Run the simulation.
  
  ASP.sample <- runif(runs, min=Lower.ASP, max=Upper.ASP)
  Qty.sample <- runif(runs, min=Lower.Qty, max=Upper.Qty)
  
  Revenue <- ASP.sample*Qty.sample 
  
  MC.Estimation <- mean(Revenue)
  MC.Upper <- MC.Estimation + popSD(Revenue)
  MC.Lower <- MC.Estimation - popSD(Revenue)
  
  # Append the results back to the table
  
  MC.Results2 <- data.frame(Dimension1, Dimension2, MC.Estimation, MC.Upper, MC.Lower)
  
  MC.Results <- rbind(MC.Results, MC.Results2)
  
}

############################################################################
# Export the Results as a .csv
############################################################################

write.csv(MC.Results, file='C:/Users/Derek/Documents/RPackages/Monte Carlo/MCResults.csv', row.names=F)

