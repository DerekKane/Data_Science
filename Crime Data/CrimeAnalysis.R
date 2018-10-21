# Here is the url link for the tutorial
# http://r-dir.com/blog/2013/07/data-visualization-with-ggplot.html

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/kane.de/Documents/RPackages/Crime Data")

library(maps)
library(ggplot2)
library(plyr)

#############################################################################

# This is a tutorial to build useful maps from crime data
# the data comes from http://www.crimemapping.com/default.aspx


# This is the code in case we want to check if there is a file first.
# and then use this file instead of having to go to the web.
# This avoids having to open the google api for each data point.

fn.NMcrime2 <- "C:/Users/kane.de/Documents/RPackages/Crime Data"
if (file.exists(fn.NMcrime2)) {
# if this file exists, then weve already done the geocode(),
# just read the file
 
  NMcrime2 <- read.csv(fn.NMcrime2, stringsAsFactors = FALSE)
} else {
# otherwise, read the original file and do the geocode() and write the file
NMcrime <- read.csv("http://statacumen.com/teach/SC1/SC1_16_crimemapping_Theft2013Q1.csv"
                     , header = FALSE, skip = 11, stringsAsFactors = FALSE
                     , col.names = c("Type", "Description", "Case", "Location", "Agency", "Date"))
NMcrime$CityState <- "Albuquerque NM"
NMcrime$Address <- paste(NMcrime$Location, NMcrime$CityState)

# geocode the lat/lon, though geocode returns lon/lat (for x,y order)
# Note, I include "warning=FALSE, message=FALSE" in the knitr options
# to supress all the Google Maps API messages in the output.

ll.NMcrime <- geocode(NMcrime$Address)
NMcrime2 <- cbind(NMcrime, ll.NMcrime)

# Since it takes a while to geocode many addresses,
# save this output to a file that can be read in conveniently as you
# develop the code below.
write.csv(NMcrime2, file = "NMcrime2.csv")
}

#############################################################################

# Remove an outlier (large lon)
NMcrime2 <- NMcrime2[-which(NMcrime2$lon == max(NMcrime2$lon)),]
NMcrime2$Description <- factor(NMcrime2$Description)

# day of week
day.temp <- weekdays(as.Date(NMcrime2$Date, format = "%m/%d/%Y %H:%M"))
NMcrime2$day <- factor(day.temp, levels = rev(unique(day.temp)), ordered = TRUE)

# time of day
time.temp <- as.POSIXct(NMcrime2$Date, format = "%m/%d/%Y %H:%M")

# convert time to 6-hour blocks
NMcrime2$time <- cut(as.POSIXlt(time.temp)$hour, c(0,6,12,18,24))

# Lets build a map

map <- get_map( location = "Lomas/Girard Albuquerque NM"
                , zoom = 14
                , maptype = "roadmap"
                , color = "bw" # make black & white so color is data
)

# This will integrate the data set as points on the map.

p <- ggmap(map)
p <- p + geom_point(data = NMcrime2
                    , aes(x = lon, y = lat, colour = Description)
                    , alpha = 0.5, size = 2
                    , position = "jitter")
print(p)

# This will create a 2D density map.

p <- ggmap(map)

p <- p + scale_x_continuous(expand = c(0.05, 0)) # expand axes 5%

p <- p + scale_y_continuous(expand = c(0.05, 0)) # before creating the overlay

# Overlay the datapoints on the map.

overlay <- stat_density2d(data = NMcrime2
                          , aes(x = lon, y = lat, fill = ..level.. , alpha = ..level..)
                          , size = 1, bins = 10, geom = "polygon")

p <- p + overlay
p <- p + scale_fill_gradient("Density")
p <- p + scale_alpha(range = c(0.1, 0.3), guide = FALSE)
p <- p + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 16))
p <- p + geom_point(data = NMcrime2
                    , aes(x = lon, y = lat, colour = Description)
                    , alpha = 0.5, size = 2
                    , position = "jitter")
p <- p + labs(title = "Burglary and theft 2013 Q1")
print(p)

# By day of the week

p1 <- p + facet_wrap( ~ day, nrow = 2)
p1 <- p1 + labs(title = "Burglary and theft 2013 Q1, by weekday")
print(p1)

# By time of the day

p2 <- p + facet_wrap( ~ time, nrow = 2)
p2 <- p2 + labs(title = "Burglary and theft 2013 Q1, by time of day")
print(p2)