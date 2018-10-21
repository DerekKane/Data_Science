## Step-by-step development that develops a 'solution' to Gapminder
## assignment (emulate Gapminder in two hours or less!) using base R
## graphics.

## 'home' directory for this analysis
whereAmI <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/"

## data import from local file
gDat <- read.delim(paste0(whereAmI,"data/gapminderDataFiveYear.txt"))

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
# peek(gDat)



## map continent and country into colors

## use the color scheme created in
## bryan-a01-30-makeGapminderColorScheme.R
continentColors <-
  read.delim(paste0(whereAmI, "data/gapminderContinentColors.txt"),
             as.is = 3)                 # protect color
continentColors
##   continent nCountries   color
## 1    Africa         52 #7F3B08
## 2  Americas         25 #A50026
## 3      Asia         33 #40004B
## 4    Europe         30 #276419
## 5   Oceania          2 #313695

countryColors <-
  read.delim(paste0(whereAmI, "data/gapminderCountryColors.txt"),
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
gDat <- with(gDat, gDat[order(year, -1 * pop),])

## we are ready to start plotting

(jYear <- max(gDat$year))
(i <- 0)

## this is not a great function, but is better than having this
## repetitive command littering up the script
jPrintToPdf <- function(j) {
  figFileName <- paste0("bryan-a01-stepByStepBase-", jYear, "-",
                        formatC(j, width = 2, flag = "0"), ".pdf")
  cat("writing", figFileName, "\n")
  dev.print(pdf,
            paste0("C:/Users/Derek/Documents/RPackages/Statistics Lectures/Gapminder", "/Charts/", figFileName),
            width = 9, height = 7)
}



## the most basic plot
(i <- i + 1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear)
jPrintToPdf(i)

## take control of whitespace around plot
(i <- i + 1)
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear)
par(op)
jPrintToPdf(i)

## take control of axis labels, orientation of tick labels
(i <- i + 1)
jXlab <- "Income per person (GDP/capita, inflation-adjusted $)"
jYlab <- "Life expectancy at birth (years)"
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab)
par(op)
jPrintToPdf(i)

## log transform the x = gdpPercap axis using the 'log' argument
(i <- i + 1)
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab,
     log = 'x')
par(op)
jPrintToPdf(i)

## log transform the x = gdpPercap axis 'by hand'
(i <- i + 1)
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ log(gdpPercap), gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab)
par(op)
jPrintToPdf(i)

## map pop into circle radius
jPopRadFun <- function(jPop) {          # make area scale with pop
  sqrt(jPop/pi)
}
(i <- i + 1)
plot(jPopRadFun(pop) ~ pop, gDat)       # looks promising
jPrintToPdf(i)

## try to use the symbols command alone
op <- par(mar = c(5, 4, 1, 1) + 0.1)
with(subset(gDat, year == jYear),
     symbols(x = gdpPercap, y = lifeExp,
             circles = jPopRadFun(pop),
             log = 'x'))
par(op)
## Error in plot.window(...) : Logarithmic axis must have positive
## limits
## learn that it's often easier to use 'plot()' to set things up and
## then add all the bells and whistles

## use 'plot()' to set things up and then add other elements
(i <- i + 1)
jDarkGray <- 'grey20'
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab,
     log = 'x', type = "n")
with(subset(gDat, year == jYear),
     symbols(x = gdpPercap, y = lifeExp,
             circles = jPopRadFun(pop), add = TRUE,
             inches = 0.7,
             fg = jDarkGray, bg = color))
par(op)
jPrintToPdf(i)

## suppress the automatic axes (tick marks)
## in anticipation of taking direct control
(i <- i + 1)
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab,
     log = 'x', type = "n",
     xaxt = "n", yaxt = "n")
with(subset(gDat, year == jYear),
     symbols(x = gdpPercap, y = lifeExp,
             circles = jPopRadFun(pop), add = TRUE,
             inches = 0.7,
             fg = jDarkGray, bg = color))
par(op)
jPrintToPdf(i)

## get some intuition for good axis limits
sapply(gDat[c('gdpPercap','lifeExp')], range)
##        gdpPercap lifeExp
## [1,]    241.1659  23.599
## [2,] 113523.1329  82.603

sapply(gDat[c('gdpPercap','lifeExp')], quantile,
       probs = c(0.9, 0.95, 0.98))
##     gdpPercap lifeExp
## 90%  19449.14 75.0970
## 95%  26608.33 77.4370
## 98%  33682.22 79.3694

(i <- i + 1)
jXlim <- c(200, 50000)
jYlim <- c(21, 84)
gdpTicks <- c(200, 400, 1000, 2000, 4000, 10000, 20000, 40000)
lifeExpTicks <- seq(from = 20, to = 85, by = 5)
jLightGray <- 'grey80'
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab,
     log = 'x', type = "n",
     xaxt = "n", yaxt = "n",
     xlim = jXlim, ylim = jYlim)
axis(side = 1, at = gdpTicks, labels = gdpTicks)
axis(side = 2, at = lifeExpTicks, labels = lifeExpTicks, las = 1)
abline(v = gdpTicks, col = jLightGray)
abline(h = lifeExpTicks, col = jLightGray)
with(subset(gDat, year == jYear),
     symbols(x = gdpPercap, y = lifeExp,
             circles = jPopRadFun(pop), add = TRUE,
             inches = 0.7,
             fg = jDarkGray, bg = color))
par(op)
jPrintToPdf(i)

## place YEAR as a watermark in background,
## include a legend
(i <- i + 1)
yearCex <- 15
op <- par(mar = c(5, 4, 1, 1) + 0.1)
plot(lifeExp ~ gdpPercap, gDat, subset = year == jYear,
     las = 1, xlab = jXlab, ylab = jYlab,
     log = 'x', type = "n",
     xaxt = "n", yaxt = "n",
     xlim = jXlim, ylim = jYlim)
text(x = sqrt(prod(jXlim)), y = mean(jYlim),
     jYear, adj = c(0.5, 0.5), cex = yearCex, col = jLightGray)
axis(side = 1, at = gdpTicks, labels = gdpTicks)
axis(side = 2, at = lifeExpTicks, labels = lifeExpTicks, las = 1)
abline(v = gdpTicks, col = jLightGray)
abline(h = lifeExpTicks, col = jLightGray)
with(subset(gDat, year == jYear),
     symbols(x = gdpPercap, y = lifeExp,
             circles = jPopRadFun(pop), add = TRUE,
             inches = 0.7,
             fg = jDarkGray, bg = color))
with(continentColors,
     legend(x = 'bottomright', bty = 'n',
            legend = continent,
            fill = color))
par(op)
jPrintToPdf(i)


