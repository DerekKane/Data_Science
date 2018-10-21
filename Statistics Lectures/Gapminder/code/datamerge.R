whereAmI <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/"
library(car)                            # recode()

## bring in prepped datasets

popDat <- read.delim(paste0(whereAmI, "data/totalPopClean.txt"))
str(popDat)
## 'data.frame':	14105 obs. of  3 variables:
##  $ country: Factor w/ 253 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year   : int  1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 ...
##  $ pop    : int  8150368 8284473 8425333 8573217 8728408 8891209 9061938 92409..
head(popDat)
peek(popDat)

leDat <- read.delim(paste0(whereAmI, "data/lifeExpectClean.txt"))
str(leDat)
## 'data.frame':	3786 obs. of  4 variables:
##  $ continent: Factor w/ 7 levels "","Africa","Americas",..: 4 4 4 4 4 4 4 4 4 ..
##  $ country  : Factor w/ 198 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
head(leDat)
peek(leDat)

gdpDat <- read.delim(paste0(whereAmI, "data/gdpClean.txt"))
str(gdpDat)
## 'data.frame':	10911 obs. of  3 variables:
##  $ country  : Factor w/ 229 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 ...
##  $ gdpPercap: num  757 767 779 813 815 ...
head(gdpDat)
peek(gdpDat)





## studying the overlap between countries in the different datasets
unionCountries <- sort(union(levels(popDat$country),
                             union(levels(leDat$country), levels(gdpDat$country))))
length(unionCountries)                  # 271 countries
unionCountries
## I see some problems such as:
## * one country with different variants
## e.g. Serbia vs Serbia and Montenegro vs Serbia excluding Kosovo
## * funky character stuff
## e.g. "Saint Barth\216lemy" or "\201land"

cTab <- data.frame(country = I(unionCountries))
## FYI: the I() protects country names from being converted to a
## factor

cTab$pop <- with(cTab, country %in% levels(popDat$country))
cTab$le <- with(cTab, country %in% levels(leDat$country))
cTab$gdp <- with(cTab, country %in% levels(gdpDat$country))
cTab$total <- with(cTab, pop + le + gdp)
table(cTab$total)
##  1   2   3
## 40  53 178

## Can I just ignore countries that appear in 1 or 2 datasets?
with(cTab, cTab[total < 3,])
## No, I cannot.

## if this were a "real" analyais, this would have to be done very
## carefully
## for now, I'll just fix some that jump out at me

mySubs <- c('"Bahamas, The"="Bahamas"',
            '"Central African Rep."="Central African Republic"',
            '"Cook Is"="Cook Islands"',
            '"Czech Rep."="Czech Republic"',
            '"Dominican Rep."="Dominican Republic"',
            '"Egypt, Arab Rep."="Egypt"',
            '"Gambia, The"="Gambia"',
            '"Iran, Islamic Rep."="Iran"',
            '"Russian Federation"="Russia"',
            '"Syrian Arab Republic"="Syria"',
            '"Venezuela, RB"="Venezuela"')
mySubs <- paste(mySubs, collapse = ";")
popDat$country <- with(popDat, recode(country, mySubs))
leDat$country <- with(leDat, recode(country, mySubs))
gdpDat$country <- with(gdpDat, recode(country, mySubs))

## re-doing the country survey
unionCountries <- sort(union(levels(popDat$country),
                             union(levels(leDat$country), levels(gdpDat$country))))
length(unionCountries)                  # 260 countries (was 271)
cTab <- data.frame(country = I(unionCountries))
cTab$pop <- with(cTab, country %in% levels(popDat$country))
cTab$le <- with(cTab, country %in% levels(leDat$country))
cTab$gdp <- with(cTab, country %in% levels(gdpDat$country))
cTab$total <- with(cTab, pop + le + gdp)
table(cTab$total)
## BEFORE recodes    AFTER recodes
##  1   2   3         1   2   3
## 40  53 178        28  44 188

## Now, can I just ignore countries that appear in 1 or 2 datasets?
with(cTab, cTab[total < 3,])
## What is going on with the USSR?  Otherwise, yes can ignore
## countries that appear in 1 or 2 datasets.

with(subset(popDat, country %in% c("Russia","USSR")),
     xyplot(pop ~ year, type = "l",
            groups = country[drop = TRUE], auto.key = TRUE))
## pop data present for USSR *and* Russia

## NOTE: syntax above limits the legend to the countries actually in
## the figure (vs. all possible values of country)
## to see what I mean, uncomment and run this
#xyplot(pop ~ year, popDat, type = "l",
#       subset = country %in% c("Russia","USSR"),
#       groups = country[drop = TRUE], auto.key = TRUE)

with(subset(leDat, country %in% c("Russia","USSR")),
     xyplot(lifeExp ~ year, type = "l",
            groups = country[drop = TRUE], auto.key = TRUE))
with(subset(gdpDat, country %in% c("Russia","USSR")),
     xyplot(gdpPercap ~ year, type = "l",
            groups = country[drop = TRUE], auto.key = TRUE))
## lifeExp and gdpPercap only have data for Russia

## I don't know what to do about fact that population data includes
## entries for USSR and Russia, where presumably the USSR data
## includes Russia (?).  I will just let the USSR entries go .....

(keepers <-
  sort(intersect(levels(popDat$country),
                 intersect(levels(leDat$country), levels(gdpDat$country)))))
length(keepers)                         # 188 countries

## merge all three datasets!  then enforce countries to keep
gDat <- merge(popDat, leDat)
gDat <- merge(gDat, gdpDat)
gDat <- subset(gDat, country %in% keepers)
## gDat <- refactor(gDat)
gDat <- droplevels(gDat)                # new built-in function R 2.12
# Feb 2011!
gDat <- gDat[with(gDat, order(country, year)),]
peek(gDat)
str(gDat)
## 'data.frame':	3312 obs. of  6 variables:
##  $ country  : Factor w/ 187 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 1288181..
##  $ continent: Factor w/ 7 levels "","Africa","Americas",..: 4 4 4 4 4 4 4 4 4 ..
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...

## tiny plot example -- make sure nothing obviously, hideously wrong
xyplot(lifeExp ~ gdpPercap, gDat, subset = year == 1985)
## let's hope this works!

write.table(gDat,
            paste0(whereAmI, "data/gapminderData.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)


