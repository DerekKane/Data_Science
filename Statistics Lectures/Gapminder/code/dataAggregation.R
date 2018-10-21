## code snippets seen in lecture re: data aggregation

## WARNING: This file does not present a coherent analysis.  It's a
## series of random snippets to support a lecture.  In some cases I
## deliberately write BAD code, to make a pedagogical point.  You have
## been warned.

## grab the Gapminder data to use in examples
whereAmI <- "/Users/jenny/teaching/STAT545A/examples/gapminder/"

## data import from local file
gDat <- read.delim(paste0(whereAmI, "data/gapminderDataFiveYear.txt"))

## reach out and touch the data
str(gDat)
## 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3  ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...

## creating a toy matrix, so I can demo apply
(jCountries <- sort(c('Canada', 'United States', 'Mexico')))
tinyDat <- subset(gDat, country %in% jCountries)
str(tinyDat)                   # 'data.frame': 36 obs. of 6 variables:
(nY <- length(unique(tinyDat$year)))    # 12 years

jLifeExp <- matrix(tinyDat$lifeExp, nrow = nY)
colnames(jLifeExp) <- jCountries
rownames(jLifeExp) <- tinyDat$year[1:nY]
jLifeExp

apply(jLifeExp, 1, mean)

apply(jLifeExp, 2, median)

jCountries[apply(jLifeExp, 1, which.max)]

## moving on to data.frames

## summary() is a good and bad starting example
sapply(gDat, summary)
summary(gDat)

## testing whether the variables are <sthg>
sapply(gDat, is.numeric)
gDatNum <- subset(gDat, select = sapply(gDat, is.numeric))
str(gDatNum)

## demo of sapply vs. lapply
sapply(gDatNum, median)
lapply(gDatNum, median)

sapply(gDatNum, range)
lapply(gDatNum, range)

## introducing tapply
with(gDat,
     tapply(lifeExp, continent, max))

## how many countries for each continent?
with(gDat,
     tapply(country, continent, function(x) {
       length(unique(x))
     }))

## tapply result often needs clean-up
(rangeLifeExp <- with(gDat,
                      tapply(lifeExp, continent, range)))
str(rangeLifeExp)

## rbind does what we want, but does not scale well
rbind(rangeLifeExp[[1]], rangeLifeExp[[2]],
      rangeLifeExp[[3]], rangeLifeExp[[4]], rangeLifeExp[[5]])

## do.call scales (and gets nice row names)
do.call(rbind, rangeLifeExp)

## let's do this completely properly
rangeLifeExp <- data.frame(do.call(rbind, rangeLifeExp))
names(rangeLifeExp) <- c("min", "max")
rangeLifeExp                            # lovely!


## fitting a linear regression model to each country
## and saving the estimated intercept and slope
(yearMin <- min(gDat$year))             # 1952
coefEst <- by(gDat, gDat$country, function(cty) {
  coef(lm(lifeExp ~ I(year - yearMin), cty))
})
head(coefEst)                           # ugly and unwieldy
str(coefEst, max.level = 0)

## clean up
coefEst <- data.frame(do.call(rbind,coefEst))
peek(coefEst)                           # not bad but room to improve
coefEst <-
  data.frame(country = factor(rownames(coefEst),
                              levels = levels(gDat$country)),
             coefEst)
names(coefEst) <- c('country','intercept','slope')
rownames(coefEst) <- NULL
peek(coefEst)                           # looking good

## bring in continent

## method 1: using match
coefEstVersion1 <- coefEst
coefEstVersion1$continent <-
  gDat$continent[match(coefEst$country, gDat$country)]
peek(coefEstVersion1)
str(coefEstVersion1)

## method 2: using merge

## create a table with variables for country and continent
justCountryContinent <- subset(gDat, select = c(country, continent))
dups <- duplicated(justCountryContinent)
justCountryContinent <- subset(justCountryContinent, !dups)
str(justCountryContinent)
## 'data.frame':	142 obs. of  2 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 4 1 1 2 5 4 3 3 4 ...

coefEstVersion2 <- merge(coefEst, justCountryContinent)
peek(coefEstVersion2)
str(coefEstVersion2)

## do the methods really do the same thing?
str(coefEstVersion1)
str(coefEstVersion2)
identical(coefEstVersion1$continent, coefEstVersion2$continent)
identical(coefEstVersion1, coefEstVersion2)
## yes they do!

## accept one
coefEst <- coefEstVersion1
summary(coefEst$continent)

## looking at the slopes for countries grouped by continent
densityplot(~ slope | continent, coefEst,
            type = c('p','g'),
            subset = continent != 'Oceania',
            xlab = paste("Slope from lm(lifeExp ~ year -",
                         yearMin, "), within country"))
dev.print(pdf,
          paste0(whereAmI, "figs/gapminderSlopesDensityplot.pdf"),
          width = 6, height = 6)

## picking out examples: best and worst for each continent
bestWorst <- by(coefEst, coefEst$continent, function(z) {
  z[c(which.min(z$slope), which.max(z$slope)),]
})
str(bestWorst)                          # an ugly list of data.frames!
bestWorst <- do.call(rbind, bestWorst)  # stack 'em up by row
bestWorst$status <- c('worst','best')
rownames(bestWorst) <- NULL
## drop Oceania ... only 2 countries
bestWorst <- subset(bestWorst, continent != "Oceania")
bestWorst                               # lovely!

## revisiting "raw" data for these interesting examples
zDat <- droplevels(subset(gDat, subset = country %in%
                            bestWorst$country))

## taking charge of the order of levels for country
levels(zDat$country)
zDat$country <-
  factor(zDat$country,
         levels = with(bestWorst,
                       as.character(country)[c(which(status == 'worst'),
                                               which(status == 'best'))]))
levels(zDat$country)

xyplot(lifeExp ~ year | country, zDat,
       layout = c(4,2), type = c('p','g','r'))
dev.print(pdf,
          paste0(whereAmI, "figs/gapminderBestWorst.pdf"),
          width = 6, height = 6)

## added in response to student question: "I did manage to get an
## overview of the country with the highest GDP for each year, but for
## example couldn't figure out how to get the country name instead of
## the number, using only one line of code"

maxGdpByYear <- do.call(rbind,
                        by(gDat, gDat$year, function(z) {
                          z[which.max(z$gdpPercap), c("year", "country", "gdpPercap")]
                        }))

