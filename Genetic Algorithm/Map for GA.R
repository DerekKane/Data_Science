library(ggmap)


# This solution will show the map of the ideal route produced by the GA.

x <- TSPdata[, 2]
y <- TSPdata[, 3]
plot(x,y, type= "n", asp=1, xlab="Latitude", ylab="Longitude", col="green")))
tour <- GA@solution[1,]
tour <- c(tour, tour[1])
n <- length(tour)
abline(arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length=0.15, angle=25, col = "steelblue", lwd=2))
text(x,y, labels(TSPdata$name), cex=0.8)


TSPdata <- subset(mydata, Cluster == "3")
TSPdata$Address <- NULL
TSPdata$Cluster <- NULL

# This will hardcode the order results from the GA

routeorder <- c(1,29,28,26,15,23,24,27,5,6,7,12,11,10,9,8,4,3,2)
# routeorder <- c(2,1,29,28,26,25,23,24,27,5,6,7,12,11,10,9,8,4,3)


TSPdata <- cbind(TSPdata, routeorder)
TSPdata <- TSPdata[order(TSPdata$routeorder),]


# plot locations on the map

dat.pts <- data.frame(x = TSPdata$Longitude, y = TSPdata$Latitude)


# get map layer

map <- get_googlemap(
  "Naperville, IL" # google search string
  , zoom = 13 # larger is closer
  , maptype = "roadmap" # map type
  , markers = dat.pts # markers for map
  , path = dat.pts # path, in order of points
  , scale = 2
)

# plot map
p <- ggmap(map
           , extent = "device" # remove white border around map
           , darken = 0.2 # darken map layer to help points stand out
)

p <- p + geom_text(data = TSPdata, aes(x = Longitude, y = Latitude, label = CustomerID)
                   , hjust = -0.2, colour = "white", size = 6)

print(p)







# Some basic map loading

map <- get_map(location = c(lon = min(TSPdata[,3]), lat = max(TSPdata[,2])),
               color = "color",
               source = "google",
               maptype = "roadmap",
               zoom = 11)

ggmap(map,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")
