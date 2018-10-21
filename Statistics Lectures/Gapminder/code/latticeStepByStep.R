## STAT 545A

## Step-by-step development that develops a 'solution' to the
## Gapminder challenge (emulate Gapminder in two hours or less!) using
## lattice

library(latticeExtra)                   # scale.components for custom
# lattice axis scaleds
# useOuterStrips()
library(help = "latticeExtra")
library(grid)                           # grid.text()

## JB has her own lattice theme
## sometimes she wants to get results under the usual default theme
## here's how to switch on the fly
## trellis.device(new = FALSE, theme = NULL)
## and back again
## trellis.device(new = FALSE, theme = jTheme)

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
  figFileName <- jPaste("bryan-a01-stepByStepLattice-",
                        formatC(j, width = 2, flag = "0"),
                        "-", jDesc, ".pdf")
  cat("writing", figFileName, "\n")
  dev.print(pdf,
            jPaste(whereAmI, "figs/stepByStepLattice/", figFileName),
            width = 9, height = 7)
}

## demo with just one year
(jYear <- max(gDatOrdered$year))

## start my fig counter
(i <- 0)

## the most basic plot
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear)
jPrintToPdf(i, "mostBasic")

## take control of axis labels
(i <- i + 1)
jXlab <- "Income per person (GDP/capita, inflation-adjusted $)"
jYlab <- "Life expectancy at birth (years)"
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab)
jPrintToPdf(i, "axisLabels")

## take control of the axis limits
(i <- i + 1)
jXlim <- c(200, 58000)
jYlim <- c(21, 88)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       xlim = jXlim, ylim = jYlim)
jPrintToPdf(i, "axisLimits")

## log transform the x = gdpPercap axis 'by hand'
(i <- i + 1)
xyplot(lifeExp ~ log10(gdpPercap), gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       xlim = log10(jXlim), ylim = jYlim)
jPrintToPdf(i, "logByHand")

## log transform the x = gdpPercap axis using the 'scales' argument
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       xlim = jXlim, ylim = jYlim,
       scales = list(x = list(log = 10)))
jPrintToPdf(i, "logViaScales")

## use xscale.components.logpower from latticeExtra to get better log axis
## tick mark labels (proper superscripts)
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.logpower)
jPrintToPdf(i, "logpowerticks")

## using equispaced.log argument to emulate traditional graphics
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       xlim = jXlim, ylim = jYlim,
       scales = list(x = list(log = 10), equispaced.log = FALSE))
jPrintToPdf(i, "equispacedLogFalse")

## use xscale.components.log10ticks from latticeExtra to get better log axis
## tick mark labels (ticks at powers of 10, evokes log graph paper)
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10ticks)
jPrintToPdf(i, "log10ticks")

## use xscale.components.log10.3 from latticeExtra to get better log axis
## tick mark labels (ticks at pretty places from original scale)
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10.3)
jPrintToPdf(i, "log10.3")

## use advanced tricks from Sarkar (2008) section 8.4.1 to label axis
## ticks on the original scale -- TOTALLY CUSTOM ... OVERKILL?
logTicks <- function (lim, loc = c(1, 5)) {
  ii <- floor(log10(range(lim))) + c(-1, 2)
  main <- 10^(ii[1]:ii[2])
  r <- as.numeric(outer(loc, main, "*"))
  r[lim[1] <= r & r <= lim[2]]
}

xscale.components.log10 <- function(lim, ...) {
  ans <- xscale.components.default(lim = lim, ...)
  tick.at <- logTicks(10^lim, loc = 1:9)
  tick.at.major <- logTicks(10^lim, loc = c(1, 5))
  major <- tick.at %in% tick.at.major
  ans$bottom$ticks$at <- log(tick.at, 10)
  ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
  ans$bottom$labels$at <- log(tick.at, 10)
  ans$bottom$labels$labels <- as.character(tick.at)
  ans$bottom$labels$labels[!major] <- ""
  ans$bottom$labels$check.overlap <- FALSE
  ans
}

(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10)
jPrintToPdf(i, "customXscaleComponents")
## gets a result fairly similar to simply setting 'equispaced.log =
## FALSE', which is a heck of a lot easier ... so YES this is probably
## overkill

## get gridlines in background
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       grid = TRUE)
## RELATED NICE TO DO: USE SIMILAR LOGIC, CUSTOM FUNCTIONS TO TAKE
## BETTER CONTROL OF GRID LINE LOCATIONS, I.E. MAKE MORE LIKE THE
## GAPMINDER TARGET
jPrintToPdf(i, "addGridlines")

## shade the plot symbols according to country
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       grid = TRUE,
       group = country)
jPrintToPdf(i, "groupCountryDefaultColors")

## shade the plot symbols according to country BUT now use our color
## scheme (and change the plotting symbol to something fillable and
## make the symbol outline transparent)
## enact the color using the group argument
jDarkGray <- 'grey20'
jPch <- 21
jGapminderPars <-
  list(superpose.symbol = list(pch = jPch, col = jDarkGray, cex = 2,
                               fill = countryColors$color[match(levels(gDatOrdered$country),
                                                                countryColors$country)]))
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       grid = TRUE,
       group = country,
       par.settings = jGapminderPars)
jPrintToPdf(i, "groupCountryMyCountryColors")

## add a legend using auto.key
## FAILS since the automatic key is country-specific, but we need
## something more coarse, e.g. continent-specific
## WARNING: there may be a noticeable pause to render the figure
(i <- i + 1)
xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       grid = TRUE,
       group = country,
       par.settings = jGapminderPars,
       auto.key = TRUE)
jPrintToPdf(i, "autoKey")

## add a legend for *continent* using key
(i <- i + 1)
continentKey <-
  with(continentColors,
       list(x = 0.95, y = 0.05, corner = c(1, 0),
            text = list(as.character(continent)),
            points = list(pch = jPch, col = jDarkGray, fill = color)))

xyplot(lifeExp ~ gdpPercap, gDatOrdered, subset = year == jYear,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       grid = TRUE,
       group = country,
       par.settings = jGapminderPars,
       key = continentKey)
jPrintToPdf(i, "customKey")

## now we get really fancy and will redefine the panel funtion in
## order to achieve fine control over size of the circles

## below is shown in excruiting step-by-step detail to demonstrate a
## workflow for gradually developing a custom panel function

## size the symbols according to pop
## change the method of coloring the symbols:
## instead of using group, now accomplishing via the cex argument of
## panel.points
## add year in background as watermark
jCexDivisor <- 1500                     # arbitrary scaling constant
jFontsize <- 200                        # TO DO: can this be sized
# relative to panel?
## map pop into circle radius
jPopRadFun <- function(jPop) {          # make area scale with pop
  sqrt(jPop/pi)
}

yDat <- subset(gDatOrdered, year == jYear)

## confirm that panel.xyplot is indeed the default panel function
(i <- i + 1)                            # 16
xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = panel.xyplot)
jPrintToPdf(i, "panel.xyplot")
## one might then go off to read documentation for panel.xyplot

## again, figure should not change ...
(i <- i + 1)                            # 17
xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = function(...) {
         panel.xyplot(...)
       })
jPrintToPdf(i, "panel.xyplot.naked")

## again, figure should not change ...
(i <- i + 1)                            # 18
xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
       })
jPrintToPdf(i, "panel.xyplot.xyArgs")

## NOW we boldly seize control and actually attempt to plot the points
## 'by hand' using panel.points
(i <- i + 1)                            # 19
xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = function(x, y, ...) {
         panel.points(x, y)
       })
## YES figure looks same; we have not broken anything!
jPrintToPdf(i, "justPoints")

## NOW ready to exert control over size of the circles
(i <- i + 1)                            # 20
xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = function(x, y, ...) {
         panel.points(x, y, pch = jPch,
                      cex = jPopRadFun(yDat$pop)/jCexDivisor)
       })
jPrintToPdf(i, "sizeIsRight")

## now I add my carefully chosen fill colors
(i <- i + 1)                            # 21
xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = function(x, y, ...) {
         panel.points(x, y, pch = jPch,
                      cex = jPopRadFun(yDat$pop)/jCexDivisor,
                      col = jDarkGray, fill = yDat$color)
       })
jPrintToPdf(i, "fillColor")

## finally, add reference grid and year in background
jLightestGray <- "gray90"
jLightGray <- "gray80"
#jLightGray <- trellis.par.get("reference.line")$col
(i <- i + 1)                            # 22

##trellis.device(new = FALSE, theme = NULL)
## and back again
##trellis.device(new = FALSE, theme = jTheme)

xyplot(lifeExp ~ gdpPercap, yDat,
       xlab = jXlab, ylab = jYlab,
       scales = list(x = list(log = 10)),
       xlim = jXlim, ylim = jYlim,
       xscale.components = xscale.components.log10,
       key = continentKey,
       panel = function(x, y, ...) {
         grid.text(jYear,
                   gp = gpar(fontsize = jFontsize, col = jLightestGray))
         panel.grid(h = -1, v = 0, col.line = jLightGray)
         panel.abline(v = log10(logTicks(c(10, 50000), loc = 1:9)),
                      col.line = jLightGray)
         panel.points(x, y, pch = jPch,
                      cex = jPopRadFun(yDat$pop)/jCexDivisor,
                      col = jDarkGray, fill = yDat$color)
       })

jPrintToPdf(i, "everything")

## xyplot(lifeExp ~ gdpPercap, gDatOrdered,
##        subset = year == jYear,
##        scales = list(x = list(log = 10)))
## dev.print(pdf,
##   "/Users/jenny/teaching/2011/STAT545A/classMeet/cm08/betterPlot.pdf",
##   width = 6, height = 6)

