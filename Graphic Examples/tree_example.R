# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/kane.de/Documents/RPackages/Graphic Examples")
#-------------------------------------------------------


# Execute this script in R to load: source("tree_example.txt")
# This tutorial will show how to load a pre built dataset in R and run an assessment
# First, we need to find the data set in R

# This function identifies the prebuilt functions: data()
# After reviewing, lets choose the "trees" dataset.

data(trees)

# We need to attach the tree data set to the directory to work from the column names

attach(trees)

# The grpahic window in R only will show a single graphic. We can split this into a 2x2 window
# with the following function: par(mfrow=c(2,2))
# Also, substitute mfrow with mfcol to rearrange the order of the graphics.

par(mfrow=c(2,2))

# We can now run 4 graphics at once.

hist(Height)
boxplot(Height)
hist(Volume)
boxplot(Volume)

# This will reset the graphic window to the standard size: par(mfrow=c(1,1))
# Here is a scatter plot of two variables: plot(Height,Volume)
# Here is a scatter plot of all the variables: pairs(trees)
