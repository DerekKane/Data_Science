whereAmI <- "/Users/jenny/teaching/STAT545A/examples/gapminder/"

gDat <- read.delim(paste0(whereAmI, "data/gapminderDataWithContinent.txt"))
str(gDat)
## 'data.frame':	3312 obs. of  6 variables:
##  $ country  : Factor w/ 187 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460  ...
##  $ continent: Factor w/ 6 levels "Africa","Americas",..: 3 3 3 3 3 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...

## during data exploration, I learned that most countries have data
## every five years, e.g. 1952, 1957, 1962, and so on. figs &
## animation will look better if I just make that official
gDat <- subset(gDat, subset = year %% 5 == 2)
str(gDat)               # 'data.frame':	2012 obs. of  6 variables:

(yearMin <- min(gDat$year))
(yearMax <- max(gDat$year))
(allYears <- sort(unique(gDat$year)))
(nYears <- length(allYears))

## does every country have data for every jYear? ... I doubt it
## determining how many countries have data for 0, 1, 2, ... years
cDat <- data.frame(table(gDat$country))
names(cDat) <- c('country','nObs')
cDat <- transform(cDat, country = as.character(country))
str(cDat)                               # 187 obs. of  2 variables:
peek(cDat)

## adding info on country size ... because I won't hesitate to filter
## out tiny countries with lots of missing data
maxPop <- with(gDat, tapply(pop, country, max))
rankMaxPop <- rank(maxPop)
all(cDat$country == names(rankMaxPop))  # TRUE!
cDat$rankMaxPop <- rankMaxPop

xyplot(nObs ~ rankMaxPop, cDat)
## there is one really big country with data for only 11 years
## what is it?
subset(cDat, nObs == 11)
##       country nObs rankMaxPop
## 31 Cape Verde   11         20
## 35      China   11        187
## 99 Luxembourg   11         23

## It's China!  I cannot lose China. Will fix this below.  But first,
## will drop all other countries with incomplete data

(keepMe <- with(cDat, nObs == nYears | country == 'China'))
cDat[keepMe,]                           # keepers
cDat[!keepMe,]                          # will be dropped
table(keepMe)
## FALSE  TRUE
##    45   142
(countriesToKeep <- cDat$country[keepMe])

gDat <- subset(gDat, subset = country %in% countriesToKeep)
gDat <- droplevels(gDat)
str(gDat)
## 'data.frame':	1703 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 ..
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...



## BEGIN: Filling in missing data for China

## which year is the problem?
gDat[gDat$country == 'China',]          # 1952 is missing

xyplot(pop + lifeExp + gdpPercap ~ year, data = gDat,
       subset = country == 'China', type = c('p','smooth'),
       allow.multiple = TRUE, outer = TRUE,
       scales = list(y = list(relation = 'free')),
       xlim = c(1950, 2010))

## extremely low, low tech imputation for 1952
chinaGdpFit <- lm(gdpPercap ~ year, gDat,
                  subset = country == 'China' & year <= 1982)
summary(chinaGdpFit)
(chinaGdp1952 <- predict(chinaGdpFit, data.frame(year = 1952)))

chinaPopFit <- lm(pop ~ year, gDat, subset = country == 'China')
summary(chinaPopFit)
(chinaPop1952 <- predict(chinaPopFit, data.frame(year = 1952)))

chinaLifeExp1952 <- 44                  # total fiction, but no simple
# linear fit seems appropriate

gDat <- rbind(gDat,
              data.frame(country = 'China', year = 1952,
                         pop = chinaPop1952, continent = 'Asia',
                         lifeExp = chinaLifeExp1952,
                         gdpPercap = chinaGdp1952))
tail(gDat)
str(gDat)                               # 1704 obs. of  6 variables
xyplot(pop + lifeExp + gdpPercap ~ year, gDat,
       subset = country == 'China', type = c('p','smooth'),
       allow.multiple = TRUE, outer = TRUE,
       scales = list(y = list(relation = 'free')),
       xlim = c(1950, 2010))
## China looks OK now

## END: Filling in missing data for China

## re-sort by country then year
gDat <- with(gDat, gDat[order(country, year),])

write.table(gDat,
            paste0(whereAmI, "data/gapminderDataFiveYear.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)


