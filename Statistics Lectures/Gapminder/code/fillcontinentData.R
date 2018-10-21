whereAmI <- "/Users/jenny/teaching/STAT545A/examples/gapminder/"

gDat <- read.delim(paste0(whereAmI, "data/gapminderData.txt"))
str(gDat)
## 'data.frame':	3312 obs. of  6 variables:
##  $ country  : Factor w/ 187 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 1288181..
##  $ continent: Factor w/ 7 levels "","Africa","Americas",..: 4 4 4 4 4 4 4 4 4 ..
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...

str(gDat$continent)                     # 7 values for continent,
summary(gDat$continent)                 # though 1 is the empty value
##           Africa Americas     Asia   Europe      FSU  Oceania
##     301      613      343      557     1302      122       74

## 301 rows have no continent data.  That is a problem.

## I've never heard of the continent of FSU.
unique(gDat$country[gDat$continent == "FSU"])
## [1] Belarus    Kazakhstan Latvia     Lithuania  Russia     Ukraine
## FSU = Former Soviet Union (?)

## Is continent data uniform for all rows pertaining to one country?

## chop data.frame up by country, then tabulate continent
foo <- tapply(gDat$continent, gDat$country, table)

## glue the above together row-wise
foo <- do.call("rbind", foo)

## for each row, count non-zero entries, then tabulate
table(apply(foo, 1, function(x) sum(x != 0)))
##   1
## 187
## --> yes, all 187 countries have exactly 1 associated value of continent

## which countries do not have continent data?
blankContinent <- foo[ , 1] > 0
table(blankContinent)                   # confirming 26 countries
# affected

## create a working data.frame in which to populate the missing
## continent data
(needContinent <-
  data.frame(country = I(rownames(foo)[blankContinent]),
             continent = I((""))))

## pondered this and used Google to determine that ....
needContinent$continent[needContinent$country %in%
                          c("Hong Kong, China", "Maldives")] <- "Asia"
needContinent$continent[needContinent$country %in%
                          c("Armenia", "Georgia", "Uzbekistan")] <- "FSU"
needContinent$continent[needContinent$country %in%
                          c("Reunion", "Sao Tome and Principe")] <- "Africa"
needContinent$continent[needContinent$country %in%
                          c("Aruba", "Bahamas", "Barbados", "Belize", "Canada",
                            "French Guiana", "Grenada", "Guadeloupe", "Haiti",
                            "Martinique", "Netherlands Antilles")] <- "Americas"
needContinent$continent[needContinent$country %in%
                          c("Australia", "French Polynesia", "Micronesia, Fed. Sts.",
                            "New Caledonia", "Papua New Guinea", "Samoa", "Tonga",
                            "Vanuatu")] <- "Oceania"
table(needContinent$continent)
##  Africa Americas     Asia      FSU  Oceania
##       2       11        2        3        8
## All countries HAVE a continent now. YAY.

## remake the continent factor in main data.frame
gDat$continent <-
  factor(with(gDat,
              ifelse(continent == "",
                     needContinent$continent[match(country,
                                                   needContinent$country)],
                     as.character(continent))))
summary(gDat$continent)
##   Africa Americas     Asia   Europe      FSU  Oceania
##      637      470      577     1302      139      187

write.table(gDat,
            paste0(whereAmI, "data/gapminderDataWithContinent.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)

