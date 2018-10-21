# Here is the url link for the tutorial
# http://r-dir.com/blog/2013/07/data-visualization-with-ggplot.html

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/GG Plot Examples")
#-------------------------------------------------------

# Open up Library ggplot2 and scales (for commas)

library("ggplot2")
library("scales")

# Import the example dataset from ggplot-data.csv

data <- read.csv("ggplot-data.csv", header=TRUE, nrows=200)

# Show the first 10 records of the dataset

head(data, n=10)

# Here is the first example of a plot in ggplot2.
# This code declares the variable gg to hold the ggplot commands
# Also, this code is indicating that the X axis is related to the Keyword Column

gg <- ggplot(data, aes(x=Keyword))

# We still need to tell the chart what to put on the Y axis 
# and also the type of chart to use. The geom_bar refers to a standard bar chart.
# The weight refers to the column of data on the table that will be in the results.
# This case it is Traffic.

gg <- gg + geom_bar(aes(weight=Traffic))

# If we want to view the chart, we can use the gg command below.
gg

# Notice that the chart displayed with black columns 
# does not break out the different countries. This is fixed by adding the fill statement
# into the geom_bar command

gg <- gg + geom_bar(aes(weight=Traffic, fill=Country))
gg

# Now lets flip this graph on the axis.
gg <- gg + coord_flip()
gg

# In order to get the chart to display in a particular order (as an inverted waterfall
# with the most traffic at the top and the least traffic at the bottom) we must manipulate
# the data with the reorder() command.

data$kw <- reorder(data$Keyword, data$Traffic)

# We will call ggplot2 again and put the kw on the X Axis

gg <- ggplot(data, aes(x=kw))
gg <- gg + geom_bar(aes(weight=Traffic, fill=Country))
gg <- gg + coord_flip()
gg

# Now lets make it look pretty with some good labels

gg <- gg + labs(list(x="Keyword", y="Monthly Search Volume", title="Search Volume by Keyword"))
gg

# This will fix the axis and add commas to using the scales library.

gg <- gg + scale_y_continuous(labels=comma)
gg

