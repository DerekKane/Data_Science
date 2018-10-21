## STAT 545A Assignment 1

## NOTE: not updated with most recent (log) axis labelling and grid
## goodness, subsequent to additions in lattice and latticeExtra; see
## the step-by-step and solution files for that

## Showing examples of what lattice is really good at (versus the very
## narrow task of emulating Gapminder) ... for example, multi-panel
## conditioning

library(hexbin)                         # hexbinplot()

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

## insert color as a variable in gDat
gDat <- merge(gDat, countryColors)

## Sort by year (increasing) and population (decreasing)
## Why? So larger countries will be plotted "under" smaller ones.
gDatOrdered <- with(gDat, gDat[order(year, -1 * pop),])

## we are ready to start plotting

## this is not a great function, but is better than having this
## repetitive command littering up the script
jPrintToPdf <- function(j, jDesc = "") {
  figFileName <- jPaste("bryan-a01-",
                        formatC(j, width = 2, flag = "0"),
                        "-", jDesc, ".pdf")
  cat("writing", figFileName, "\n")
  dev.print(pdf,
            jPaste(whereAmI, "figs/basicLatticeDemo/", figFileName),
            width = 9, height = 7)
}

## skip to the "solution" developed in the step-by-step script
jXlim <- c(200, 58000)
jYlim <- c(21, 88)
jXlab <- "Income per person (GDP/capita, inflation-adjusted $)"
jYlab <- "Life expectancy at birth (years)"
jLightGray <- trellis.par.get("reference.line")$col
jDarkGray <- 'grey20'
jPch <- 21
jFontsize <- 200
jCexDivisor <- 1500
library(grid)                           # grid.text()
library(latticeExtra)                   # useOuterStrips()

## map pop into circle radius
jPopRadFun <- function(jPop) {          # make area scale with pop
  sqrt(jPop/pi)
}

continentKey <-
  with(continentColors,
       list(x = 0.95, y = 0.05, corner = c(1, 0),
            text = list(as.character(continent)),
            points = list(pch = jPch, col = jDarkGray, fill = color)))

## start my fig counter
(i <- 0)

## "conditioning" on year for a modest number of years
(i <- i + 1)

jYears <- c(1962, 1977, 1992, 2007)
yDat <- subset(gDatOrdered, year %in% jYears)

jCexDivisor <- 1800
xyplot(lifeExp ~ gdpPercap | factor(year), yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10ticks,
       #       grid = list(h = -1, v = -1, col.line = jLightGray),
       panel = function(x, y, subscripts, ...) {
         panel.grid(h = -1, v = -1, col.line = jLightGray)
         panel.points(x, y, col = jDarkGray, pch = jPch,
                      fill = yDat$color[subscripts],
                      cex = jPopRadFun(yDat$pop[subscripts])/jCexDivisor)
       },
       key = continentKey)

jPrintToPdf(i, "condOnYear")

## "conditioning" on continent for one year
(i <- i + 1)

(jYear <- max(gDatOrdered$year))
yDat <- subset(gDatOrdered, year == jYear & continent != 'Oceania')

jCexDivisor <- 1800
xyplot(lifeExp ~ gdpPercap | continent, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10ticks,
       panel = function(x, y, subscripts, ...) {
         panel.grid(h = -1, v = -1, col.line = jLightGray)
         panel.points(x, y, col = jDarkGray, pch = jPch,
                      fill = yDat$color[subscripts],
                      cex = jPopRadFun(yDat$pop[subscripts])/jCexDivisor)
       },
       key = continentKey)

jPrintToPdf(i, "condOnContinentSubsetOnYear")

## "conditioning" on year and continent
(i <- i + 1)

jYears <- c(1962, 1977, 1992, 2007)
yDat <- subset(gDatOrdered, year %in% jYears & continent != 'Oceania')

jCexDivisor <- 2500
jPlot <-
  xyplot(lifeExp ~ gdpPercap | factor(year) * continent, yDat,
         xlab = jXlab, ylab = jYlab,
         scales = list(x = list(log = 10)),
         xlim = jXlim, ylim = jYlim,
         xscale.components = xscale.components.log10ticks,
         panel = function(x, y, subscripts, ...) {
           panel.grid(h = -1, v = -1, col.line = jLightGray)
           panel.points(x, y, col = jDarkGray, pch = jPch,
                        fill = yDat$color[subscripts],
                        cex = jPopRadFun(yDat$pop[subscripts])/jCexDivisor)
         })
useOuterStrips(jPlot)

jPrintToPdf(i, "condOnContinentAndYear")


## line plots, alpha transparency
(i <- i + 1)

countryColorsOrdered <-
  countryColors$color[match(levels(gDatOrdered$country),
                            countryColors$country)]
jGapminderPars <-
  list(superpose.line = list(alpha = 0.1, lwd = 3,
                             col = countryColorsOrdered))

xyplot(lifeExp ~ year | continent, gDatOrdered,
       subset = continent != "Oceania",
       ylim = jYlim, type = c("l","g"),
       groups = country,
       par.settings = jGapminderPars)

jPrintToPdf(i, "lifeExpVsTimeLineplot")


## demo of hexbinplot
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10ticks)
jPrintToPdf(i, "lifeExpVsGdpPlainScatter")


(i <- i + 1)
jXbins <- 55
hexbinplot(lifeExp ~ gdpPercap, gDatOrdered,
           scales = list(x = list(log = 10)),
           xlim = jXlim, ylim = jYlim,
           xscale.components = xscale.components.log10ticks,
           xbins = jXbins)
jPrintToPdf(i, "lifeExpVsGdpHexbin")

(i <- i + 1)
hexbinplot(lifeExp ~ gdpPercap | continent, gDatOrdered,
           subset = continent != "Oceania",
           scales = list(x = list(log = 10)),
           xlim = jXlim, ylim = jYlim,
           xscale.components = xscale.components.log10ticks,
           xbins = jXbins)
jPrintToPdf(i, "lifeExpVsGdpHexbinByContinent")


