whereAmI <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/"

library(lattice)

## data files downloaded from here in 2009:
## http://www.gapminder.org/gapminder-world/documentation/

## total population

## prior to this, opened gapdata003.xls in Excel, changed format of
## Population column to 'general' to suppress embedded commas, saved
## as tab-delimited text file totalPop.txt

popDat <- read.delim(paste0(whereAmI, "data/totalPop.txt"))
str(popDat)                             # 22903 obs. of  10 variables:
head(popDat)
peek(popDat)

## get rid of columns I will not use; rename variables I keep
popDat <- subset(popDat, select = c(Area,Year,Population))
names(popDat) <- tolower(names(popDat))
names(popDat)[which(names(popDat) == 'area')] <- 'country'
names(popDat)[which(names(popDat) == 'population')] <- 'pop'
str(popDat)                             # 22903 obs. of  3 variables:

## focus on the years where most of the data is

summary(popDat$year)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
##   1491    1935    1967    1953    1988    2030    2448

yTab <- with(popDat, data.frame(table(year)))
yTab$year <- as.numeric(as.character(yTab$year))
xyplot(Freq ~ year, yTab, type = c("h","g"))
xyplot(Freq ~ year, yTab, type = c("h","g"),
       xlim = c(1800, 2010))       # big increase in frequency at 1950
xyplot(Freq ~ year, yTab, type = c("h","g"),
       xlim = c(2000, 2010))          # huge drop in frequency at 2009

## keep data from 1950 to 2008
yearMin <- 1950
yearMax <- 2008
popDat <- subset(popDat, subset = year >= yearMin & year <= yearMax)
str(popDat)                             # 14105 obs. of  3 variables:

## save for later
write.table(popDat,
            paste0(whereAmI, "data/totalPopClean.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)



## life expectancy at birth

## prior to this opened
## life-expectancy-reference-spreadsheet-20090204-xls-format.xls in
## Excel, saved as tab-delimited text file lifeExpect.txt

leDat <- read.delim(paste0(whereAmI, "data/lifeExpect.txt"))
str(leDat)                             # 52416 obs. of  9 variables:
head(leDat)

## get rid of columns I will not use; rename variables I keep
names(leDat)
leDat <- subset(leDat, select = pmatch(c('Continent','Country',
                                         'Year','Life.expectancy'), names(leDat)))
str(leDat)                             # 52416 obs. of  4 variables:
names(leDat) <- c('continent','country','year','lifeExp')
head(leDat)
# peek(leDat)

## get rid of NAs, early years, recent years
summary(leDat$year)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##   1800    1852    1904    1904    1955    2007

leTab <- with(leDat, data.frame(table(year)))
leTab$year <- as.numeric(as.character(leTab$year))
xyplot(Freq ~ year, leTab, type = c("h","g"))
## we seem to have some amount of data for all years
## very different from population data ....
leTab$notNA <-
  with(leDat,
       tapply(lifeExp, year, function(x) sum(!is.na(x))))
xyplot(notNA ~ year, leTab, type = c("h","g"))
## focus on 1950 -->
xyplot(notNA ~ year, leTab, type = c("h","g"),
       xlim = c(1950, 2010))       # spikes every five years
xyplot(notNA ~ year, leTab, type = c("h","g"),
       xlim = c(2000, 2010))          # 2007 is latest

## keep data from 1950 to 2007
yearMin <- 1950
yearMax <- 2007
leDat <- subset(leDat,
                subset = year >= yearMin & year <= yearMax & !is.na(lifeExp))
str(leDat)                              # 3786 obs. of  4 variables:

## save for later
write.table(leDat,
            paste0(whereAmI, "data/lifeExpectClean.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)


## GDP per capita

## opened gapdata001-1.xlsx in Excel, ruthlessly deleted tons of
## columns at front and up to 1950, saved as tab-delimited text file
## gdpPercap.txt

gdpDat <- read.delim(paste0(whereAmI, "data/gdpPercap.txt"))
str(gdpDat)                             # 259 obs. of  59 variables:
head(gdpDat)
peek(gdpDat)
## Sadly, this file is transposed relative to the first two.  Each row
## is a country and the columns give the GDP data for different
## years.  What a mess.

## reshaping the gdp data
(nYears <- length(gdpDat) - 1)
(theYears <- as.numeric(substr(names(gdpDat)[-1],2,5)))
newDat <- data.frame(country = rep(gdpDat$Area, each = nYears),
                     year = theYears,
                     gdpPercap = as.vector(t(as.matrix(gdpDat[-1]))))
head(newDat)
peek(newDat)
str(newDat)                             # 15022 obs. of  3 variables:
gdpDat <- subset(newDat, subset = !is.na(gdpPercap))
head(gdpDat)
peek(gdpDat)
str(gdpDat)                             # 10911 obs. of  3 variables:

## save for later
write.table(gdpDat,
            paste0(whereAmI, "data/gdpClean.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)