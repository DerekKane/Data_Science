## STAT 545A

## A 'solution' to assignment 1 (emulate Gapminder
## in two hours or less!) using base R graphics.

## See bryan-a01-15-latticeStepByStep.R for a step-by-step
## development of a solution.  This file reflects how I would do this
## in real life, assuming this is some important figure that I made
## over and over again, for talks and/or a report or publication.  The
## nitty gritty is done (hidden?) elsewhere and this script just takes
## advantage of all that hard work.

## 'home' directory for this analysis
whereAmI <- "/Users/jenny/teaching/STAT545A/examples/gapminder/"

## data import from local file
gDat <- read.delim(jPaste(whereAmI,"data/gapminderDataFiveYear.txt"))

## reach out and touch the data
str(gDat)
## 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 33 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
summary(gDat)
head(gDat)
peek(gDat)

## map continent and country into colors

## use the color scheme created in
## bryan-a01-30-makeGapminderColorScheme.R
continentColors <-
  read.delim(jPaste(whereAmI, "data/gapminderContinentColors.txt"),
             as.is = 3)                 # protect color
continentColors
##   continent nCountries   color
## 1    Africa         52 #7F3B08
## 2  Americas         25 #A50026
## 3      Asia         33 #40004B
## 4    Europe         30 #276419
## 5   Oceania          2 #313695

countryColors <-
  read.delim(jPaste(whereAmI, "data/gapminderCountryColors.txt"),
             as.is = 3)                 # protect color
str(countryColors)
## 'data.frame':	142 obs. of  3 variables:
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 95 39 43 28 118 121 127 6..
##  $ color    : chr  "#7F3B08" "#833D07" "#873F07" "#8B4107" ...
peek(countryColors)

## insert color as a variable in gDat
gDat <- merge(gDat, countryColors)
peek(gDat)

## Sort by year (increasing) and population (decreasing)
## Why? So larger countries will be plotted "under" smaller ones.
gDatOrdered <- with(gDat, gDat[order(year, -1 * pop),])

## ready to start plotting
source(jPaste(whereAmI,
              "code/bryan-a01-16-latticePlotGapminderOneYear.R"))

## test the function
plotGapminderOneYear(1952, gDatOrdered, countryColors,
                     continentColors)
plotGapminderOneYear(2002, gDatOrdered, countryColors, continentColors)


## loop over year
writeToFile <- TRUE               # write a figure file for each year?

for(jYear in sort(unique(gDatOrdered$year))) {
  print(plotGapminderOneYear(jYear, gDatOrdered,
                             countryColors, continentColors))
  if(writeToFile) {
    dev.print(pdf,
              file = jPaste(whereAmI,"figs/animationLattice/bryan-a01-lattice-",
                            jYear, ".pdf"),
              width = 9, height = 7)
  }
  Sys.sleep(0.5)                        # gives 'live' figures an
  # animated feel
}


## END: make the figures


## BEGIN: stitch figures together into an animation

setwd(jPaste(whereAmI, "figs/animationLattice/"))
system("convert -delay 100 -loop 0 *.pdf gapminder.gif")
## NOTE: convert is part of ImageMagick
## I view the resulting gif animation with a browser or Xee
## most browsers work and it can also be pasted into Keynote, which
## suggests it might work in PowerPoint too?

## END: stitch figures together into an animation


