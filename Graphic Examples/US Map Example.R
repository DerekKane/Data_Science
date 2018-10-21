# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/Graphic Examples")
#-------------------------------------------------------

## Load Packages
## -------------

## download Biobase so we don't have to manually open codebook
source("http://bioconductor.org/biocLite.R")
biocLite("Biobase", suppressUpdates = TRUE)
library(qdap)
## Load initial required packages
lapply(qcv(ggplot2, maps, ggthemes, Biobase), require, character.only = T)



## The Supreme Court Codebook and opened without clicking
## ------------------------------------------------------

## download the pdf code book and open it
url_dl(SCDB_2012_01_codebook.pdf, url = "http://scdb.wustl.edu/_brickFiles/2012_01/")
openPDF(file.path(getwd(), "SCDB_2012_01_codebook.pdf"))


## The Supreme Court Data; learn to download and open a zip file
## -------------------------------------------------------------

temp <- tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2012_01/SCDB_2012_01_caseCentered_Citation.csv.zip", 
              temp)
dat <- read.csv(unz(temp, "SCDB_2012_01_caseCentered_Citation.csv"))
unlink(temp)
htruncdf(dat, 6, 6)



## Source a Codebook for State Keys Used By Supreme Court Data
## -----------------------------------------------------------

source("http://copy.com/zEtAXJC8tG7yv7Zz")
head(state.key)



## Clean Supreme Court Data
## ------------------------

dat$state <- lookup(dat$caseOriginState, state.key)
dat2 <- dat[!is.na(dat$state), ]
dat_state <- data.frame(with(dat2, prop.table(table(state))))
head(dat_state)



## Minimal Chloropleth Example
## ---------------------------

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

states_map <- map_data("state")

ggplot(crimes, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) 



## Map the Data
## ------------

states_map <- map_data("state")
head(states_map)



## Plot the Data
## -------------

ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = Freq), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  ggtitle("Chloropleth Supreme Court")



## Generate labels
## ---------------

cnames <- aggregate(cbind(long, lat) ~ region, data = states_map, FUN = function(x) mean(range(x)))
cnames$angle <- 0
head(cnames)


## Plot With Labels 1
## ------------------

ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = Freq), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) + 
  ggtitle("Chloropleth Supreme Court (With Labels 1)")



## manually move state locations and change angle
## ----------------------------------------------

cnames[11, c(2:3)] <- c(-114.5, 43.5)  # alter idaho's coordinates
cnames[17, 3] <- 30.75  # alter louisiana's coordinates
cnames[21, c(2:3)] <- c(-84.5, 43)  # alter michigan's coordinates
cnames[23, 4] <- 90  # alter mississippi's angle
cnames[9, c(2, 4)] <- c(-81.5, 90)  # alter florida's angle and coordinates



## Plot With Labels 2
## ------------------

ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = Freq), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) + 
  ggtitle("Chloropleth Supreme Court (With Labels 2)")

#===================================================================================

## Further Exploring the Data
## Download and read in a zip file just like the Supreme Court Data.
## -----------------------------------------------------------------

temp <- tempfile()
download.file("http://www2.census.gov/census_2000/datasets/demographic_profile/0_All_State/2khxx.zip", 
              temp)
demo <- read.csv(unz(temp, "2khxx.csv"))
unlink(temp)


## Clean Census Data and Merge With dat_state From Above
## -----------------------------------------------------

## browseURL("http://www2.census.gov/census_2000/datasets/demographic_profile/Alabama/2kh01.pdf")

vars <- data.frame(codes = qcv(X281421906, X138053563, X143368343, X35.3, United.States), 
                   var = qcv(pop, male, female, med_age, state))

colnames(demo)[colnames(demo) %in% vars[, 1]] <- lookup(colnames(demo)[colnames(demo) %in% vars[, 1]], vars)

demo$state <- tolower(demo$state)
demo <- demo[, colnames(demo) %in% vars[, 2]]
demo <- demo[demo$state %in% tolower(state.name), ]

## Merge it
dat_state <- merge(demo, dat_state, by = "state")



## Clean Census Data and Reshape it using the melt function from Reshape2.
## -----------------------------------------------------------------------

library(reshape2)
dat_state <- transform(dat_state, per.male = male/c(male + female))
colnames(dat_state)[6] <- "case_origin"
dat_state2 <- melt(data.frame(dat_state[, 1, drop = FALSE], apply(dat_state[, 
                                                                            -c(1, 3:4)], 2, scale)))
head(dat_state2)


## Faceted Plot Attempt 1
## ----------------------

ggplot(dat_state2, aes(map_id = state)) +
  geom_map(aes(fill = value), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) +
  facet_grid(variable~.)


## A Hunger for Better Display
## ---------------------------

plot1 <- ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = case_origin), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="orange", name="Percent") +
  guides(fill = guide_colorbar(barwidth = .5, barheight = 10)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) +
  ggtitle("Origin of Supreme Court Case (percent)") 

plot2 <- ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = pop), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="red", name="People") +
  guides(fill = guide_colorbar(barwidth = .5, barheight = 10)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) +
  ggtitle("State Populations")

plot3 <- ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = med_age), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="darkgreen", name="Age") +
  guides(fill = guide_colorbar(barwidth = .5, barheight = 10)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) +
  ggtitle("Median Age")

plot4 <- ggplot(dat_state, aes(map_id = state)) +
  geom_map(aes(fill = per.male), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue", name="Percent Male") +
  guides(fill = guide_colorbar(barwidth = .5, barheight = 10)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) +
  ggtitle("Gender Distribution")

library(gridExtra)
grid.arrange(plot1, plot3, plot2, plot4, ncol = 2)



## Using grid.draw and Aligning Plot Edges
## ---------------------------------------

library(gtable)

p1 <- ggplotGrob(plot1)
p2 <- ggplotGrob(plot2)
p3 <- ggplotGrob(plot3)
p4 <- ggplotGrob(plot4)

library(gtable)
grid.draw(cbind(rbind(p1, p2, size="last"), rbind(p3, p4, size="last"), size = "first"))



## Hack to Align Legends
## ---------------------

plot1b <- plot1 + theme(panel.border = element_blank())
plot2b <- plot2 + theme(panel.border = element_blank())
plot3b <- plot3 + theme(panel.border = element_blank())
plot4b <- plot4 + theme(panel.border = element_blank())

p1b <- ggplotGrob(plot1b)
p2b <- ggplotGrob(plot2b)
p3b <- ggplotGrob(plot3b)
p4b <- ggplotGrob(plot4b)

gt <- cbind(rbind(p1b, p2b, size="last"), rbind(p3b, p4b, size="last"), size = "first")

for (i in which(gt$layout$name == "guide-box")) {
  gt$grobs[[i]] <- gt$grobs[[i]]$grobs[[1]]
}

grid.draw(gt)
