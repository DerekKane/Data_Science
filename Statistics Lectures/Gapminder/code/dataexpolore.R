whereAmI <- "/Users/jenny/teaching/STAT545A/examples/gapminder/"

## Link to dataset
## currently broken ... undecided where/how to serve these examples
## projects up
gdURL <- "http://www.stat.ubc.ca/~jenny/notRw/teaching/STAT545A/examples/gapminder/data/gapminderData.txt"

## data import from URL
gDat <- read.delim(file = gdURL)

## data import from local file
gDat <- read.delim(file = paste0(whereAmI,"data/gapminderData.txt"))

## reach out and touch the data
str(gDat)
## 'data.frame':	3312 obs. of  6 variables:
##  $ country  : Factor w/ 187 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 1288181..
##  $ continent: Factor w/ 7 levels "","Africa","Americas",..: 4 4 4 4 4 4 4 4 4 ..
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
peek(gDat)                              # jb personal function


## BEGIN: detailed exploration of data

## do we have NAs?
sapply(gDat, function(x) sum(is.na(x)))
##   country      year       pop continent   lifeExp gdpPercap
##        0         0         0         0         0         0
## no NAs ... good!

## year
summary(gDat$year)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    1950    1967    1982    1980    1996    2007

## confirming we have 1950, 1951, ..., 2007
unique(gDat$year)
plot(unique(gDat$year))                 # see ... they are not
# in chronological order!
sort(unique(gDat$year))                 # better safe than sorry!
plot(sort(unique(gDat$year)))

identical(sort(unique(gDat$year)), 1950:2007) # TRUE
length(1950:2007)                         # 58 poss vals for year

table(gDat$year)

barchart(table(gDat$year))
## most countries have data every five years, e.g. 1952, 1957, 1962,
## and so on
dev.print(pdf,
          paste0(whereAmI, "figs/barchartYear.pdf"),
          width = 5, height = 8)

dotplot(table(gDat$year),
        origin = 0,
        type = c("p", "h"))
dev.print(pdf,
          paste0(whereAmI, "figs/dotplotYear.pdf"),
          width = 5, height = 8)

dotplot(table(gDat$year))
dev.print(pdf,
          paste0(whereAmI, "figs/dotplotYear-dotsOnly.pdf"),
          width = 5, height = 8)

## country
str(gDat$country)                       # 187 countries

table(gDat$country)

barchart(table(gDat$country))           # Ugly on screen!
dev.print(pdf,                          # print huge!
          paste0(whereAmI, "figs/barchartCountry.pdf"),
          width = 5, height = 25)


dotplot(table(gDat$country),
        type = c("p", "h"),
        origin = 0)
dev.print(pdf,
          paste0(whereAmI, "figs/dotplotCountry.pdf"),
          width = 5, height = 25)


as.data.frame(table(table(gDat$country)))

## getting more informative variable names
as.data.frame(table(nObs = table(gDat$country)),
              responseName = "nCountries")


dotplot(table(table(gDat$country)),
        type = c("p","h"),
        origin = 0)
dev.print(pdf,
          paste0(whereAmI, "figs/dotplotCountryFreq.pdf"),
          width = 5, height = 8)

## Most countries have data for 12 years, i.e. the years highlighted
## above.  Some have data for 58 years, which I assume is the maximum.
## Otherwise, there's a little bit of everything between 1 and 58.

## continent
str(gDat$continent)                     # 7 values for continent,
# though 1 is the empty value
table(gDat$continent)
##           Africa Americas     Asia   Europe      FSU  Oceania
##     301      613      343      557     1302      122       74

## 301 rows have no continent data
## Is continent data uniform for all rows pertaining to one country?
foo <- tapply(gDat$continent, gDat$country, table)
foo <- as.matrix(do.call("rbind",foo))
table(apply(foo, 1, function(x) sum(x != 0)))
## yes, all 187 countries have exactly 1 associated value of continent

noContinent <- subset(gDat, continent == "")
nlevels(noContinent$country)            # all 187 levels remain :(

noContinent <- droplevels(subset(gDat, continent == ""))
nlevels(noContinent$country)            # 26 levels
levels(noContinent$country)

## fixing the continent data is a separate task
## to be completed in with a new script


## population

summary(gDat$pop)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
## 5.941e+04 2.679e+06 7.557e+06 3.161e+07 1.959e+07 1.319e+09

## I don't like reading scientific notation so much.

summary(gDat$pop, digits = 10)
##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
##     59412    2678572    7557218   31614891   19585222 1318683096

gDat[which.min(gDat$pop),]              # we have little countries,
# like Aruba w/ 60K people
gDat[which.max(gDat$pop),]              # ... and big countries,
# like China w/ 1.3B people

densityplot(gDat$pop)                   # ugly

## life expectancy
summary(gDat$lifeExp)                   # 23 to 83 years
densityplot(gDat$lifeExp)               # looks sensible
# note bimodality
# modes ~ 42 and 72

summary(gDat$gdpPercap)                 # $240 to $114K
# $113K???? really?
# maybe I want to live there!
gDat[which.max(gDat$gdpPercap),]        # OIL!  Kuwait.
# not sure I believe this, but OK
gDat[which.min(gDat$gdpPercap),]        # Congo, Dem. Rep.

densityplot(gDat$gdpPercap)             # looks sensible
# loooong right tail


