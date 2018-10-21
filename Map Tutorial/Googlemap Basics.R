library(ggmap)
library(mapproj)

get_googlemap(urlonly = TRUE)
ggmap(get_googlemap())


# markers and paths are easy to access
d <- function(x=-95.36, y=29.76, n,r,a){
  round(data.frame(
    lon = jitter(rep(x,n), amount = a),
    lat = jitter(rep(y,n), amount = a)
  ), digits = r)
}

df <- d(n=50,r=3,a=.3)

######################################################################
# No Marker Lines 
######################################################################

map <- get_googlemap(markers = df, scale = 2)
ggmap(map)
ggmap(map, fullpage = TRUE) +
  geom_point(aes(x = lon, y = lat), data = df, size = 3, colour = "black")

######################################################################
# With Marker Lines 
######################################################################

map <- get_googlemap(markers = df, path = df,, scale = 2)
ggmap(map)
ggmap(map, fullpage = TRUE) +
  geom_point(aes(x = lon, y = lat), data = df, size = 3, colour = "black") +
  geom_path(aes(x = lon, y = lat), data = df)



######################################################################
# Clean up Google Map aspects
######################################################################

gc <- geocode("waco, texas")
center <- as.numeric(gc)
ggmap(get_googlemap(center = center, color = "bw", scale = 2), extent = "device")

# the scale argument can be seen in the following
# (make your graphics device as large as possible)
ggmap(get_googlemap(center, scale = 1), extent = "panel") # pixelated
ggmap(get_googlemap(center, scale = 2), extent = "panel") # fine

# archiving; note that you must meet google's terms for this condition
map <- get_googlemap(archiving = TRUE)
map <- get_googlemap()
ggmap(map)


# style
map <- get_googlemap(style = c(feature = "all", element = "labels", visibility = "off"))
ggmap(map)

######################################################################
# Crime Prediction in Houston
######################################################################

# Study of crimes in Houston

library(scales) # for muted graphics

str(crime)

data<-crime

# Extract location of crimes in houston
violent_crimes <- subset(crime, ((offense != "auto theft")
                                 & (offense != "theft")
                                 & (offense != "burglary")))
# rank violent crimes
violent_crimes$offense <- factor(violent_crimes$offense
                                 , levels = c("robbery", "aggravated assault"
                                              , "rape", "murder"))
# restrict to downtown
violent_crimes <- subset(violent_crimes, ((-95.39681 <= lon)
                                          & (lon <= -95.34188)
                                          & (29.73631 <= lat)
                                          & (lat <= 29.784)))
map <- get_map( location = "Houston TX"
                , zoom = 14
                , maptype = "roadmap"
                , color = "bw" # make black & white so color is data
)

p <- ggmap(map)
p <- p + geom_point(data = violent_crimes
                    , aes(x = lon, y = lat, size = offense, colour = offense))

# legend positioning, removing grid and axis labeling
p <- p + theme( legend.position = c(0.0, 0.7) # put the legend inside the plot area
                , legend.justification = c(0, 0)
                , legend.background = element_rect(colour = F, fill = "white")
                , legend.key = element_rect(fill = F, colour = F)
                , panel.grid.major = element_blank()
                , panel.grid.minor = element_blank()
                , axis.text = element_blank()
                , axis.title = element_blank()
                , axis.ticks = element_blank()
)
print(p)


#############################################################################
# 2D density plot
#############################################################################

p <- ggmap(map)
overlay <- stat_density2d(data = violent_crimes
                          , aes(x = lon, y = lat, fill = ..level.. , alpha = ..level..)
                          , size = 2, bins = 4, geom = "polygon")
p <- p + overlay
p <- p + scale_fill_gradient2("Violent Crime Density", low = "white", mid = "yellow", high = "red")
p <- p + scale_alpha(range = c(0.4, 0.75), guide = FALSE)
p <- p + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

#p <- p + inset(grob = ggplotGrob(ggplot() + overlay + theme_inset())
# , xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062)
print(p)

# now lets do it by day of week

p <- p + facet_wrap( ~ day, nrow = 2)
print(p)



#############################################################################
# Choropleth maps tutorial
#############################################################################

# A choropleth map is a thematic map in which areas are shaded or patterned
# in proportion to the measurement of the statistical variable being displayed
# on the map, such as population density or per-capita income. The choropleth
# map provides an easy way to visualize how a measurement varies across a
# geographic area or it shows the level of variability within a region.

library(maps)
library(ggplot2)
library(plyr)

# make fake choropleth data

newmexico <- map("county", regions = "new mexico", plot = FALSE, fill = TRUE)
newmexico <- fortify(newmexico)
newmexico <- ddply(newmexico, "subregion", function(df){
  mutate(df, fake = rnorm(1))
})

# make standard ggplot map (without geom_map)

p <- ggplot(newmexico, aes(x = long, y = lat, group = group, fill = fake))
p <- p + geom_polygon(colour = "white", size = 0.3)
print(p)

# Now, a fancier map using ggmap...
library(ggmap)
p <- qmap('New Mexico', zoom = 7, maptype = 'satellite', legend = 'topleft')

p <- p + geom_polygon(data = newmexico
                      , aes(x = long, y = lat, group = group, fill = fake)
                      , color = 'white'
                      , alpha = .75, size = .2)

# Add some city names, by looking up their location

cities <- c("Albuquerque NM", "Las Cruces NM", "Rio Rancho NM", "Santa Fe NM",
            "Roswell NM", "Farmington NM", "South Valley NM", "Clovis NM",
            "Hobbs NM", "Alamogordo NM", "Carlsbad NM", "Gallup NM", "Los Alamos NM")
cities_locs <- geocode(cities)


cities_locs$city <- cities
p <- p + geom_text(data = cities_locs, aes(label = city)
                   , color = 'yellow'
                   , size = 3)
print(p)
