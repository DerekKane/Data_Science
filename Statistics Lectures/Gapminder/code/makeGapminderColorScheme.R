library(RColorBrewer)                   # will use for color-coding
# continent

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

## continent-level info
cDat <- with(gDat, tapply(country, continent, function(x) length(unique(x))))
cDat <- data.frame(continent = I(names(cDat)),
                   nCountries = cDat)
rownames(cDat) <- NULL
(nCont <- nrow(cDat))
cDat
##   continent nCountries
## 1    Africa         52
## 2  Americas         25
## 3      Asia         33
## 4    Europe         30
## 5   Oceania          2

## map continent and country into colors

## choose a range of colors for each continent
display.brewer.all(type = "div")

colorAnchors <-
  list(Africa = brewer.pal(n = 11, 'PuOr')[1:5], # orange/brown/gold
       Americas = brewer.pal(n = 11, 'RdYlBu')[1:5],     # red
       Asia = brewer.pal(n = 11, 'PRGn')[1:5],           # purple
       Europe = brewer.pal(n = 11, 'PiYG')[11:7],        # green
       Oceania = brewer.pal(n = 11, 'RdYlBu')[11:10])    # blue

## turn those into a palette big enough to cover each country in a
## continent
countryColors <- lapply(seq_len(nCont), function(i) {
  yo <- droplevels(subset(gDat, continent == cDat$continent[i]))
  countriesBigToSmall <- rev(levels(reorder(yo$country, yo$pop, max)))
  colorFun <- colorRampPalette(colorAnchors[[i]])
  return(data.frame(continent = cDat$continent[i],
                    country = I(countriesBigToSmall),
                    color = I(colorFun(length(countriesBigToSmall)))))
})
names(countryColors) <- cDat$continent

## each element of countryColors is a data.frame
## within it, each row is a country
str(countryColors[['Europe']])
## 'data.frame':	30 obs. of  3 variables:
##  $ continent:Class 'AsIs'  chr [1:30] "Europe" "Europe" "Europe" "Europe" ...
##  $ country  :Class 'AsIs'  chr [1:30] "Germany" "Turkey" "France" "United King..
##  $ color    :Class 'AsIs'  chr [1:30] "#276419" "#2C6A1A" "#31701B" "#36771C" ..

countryColors[['Europe']]

## retain the first or darkest color to represent the whole continent
cDat$color <- sapply(countryColors, function(z) z$color[1])

## I would like to stack these up, row-wise, into a data.frame that
## holds my color scheme
countryColors <- do.call(rbind, countryColors)
rownames(countryColors) <- NULL
str(countryColors)
## 'data.frame':	142 obs. of  3 variables:
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ country  :Class 'AsIs'  chr [1:142] "Nigeria" "Egypt" "Ethiopia" "Congo, De..
##  $ color    :Class 'AsIs'  chr [1:142] "#7F3B08" "#833D07" "#873F07" "#8B4107"..
peek(countryColors)

## make a nice figure of my color scheme

## fiddly parameters that control printing of country names
charLimit <- 12
xFudge <- 0.05
jCex <- 0.75

op <- par(mar = c(1, 4, 6, 1) + 0.1)
plot(c(0, nCont), c(0, 1), type = "n",
     xlab = "", ylab="", xaxt = "n", yaxt = "n", bty = "n",
     main = "Gapminder Color Scheme")
for(i in seq_len(nCont)) {
  thisCont <- cDat$continent[i]
  nCols <- cDat$nCountries[cDat$continent == thisCont]
  yFudge <- 0.1/nCols
  foo <- seq(from = 0, to = 1, length = nCols + 1)
  rect(xleft = i - 1,
       ybottom = foo[-(nCols + 1)],
       xright = i,
       ytop = foo[-1],
       col = countryColors$color[countryColors$continent == thisCont])
  text(x = i - 1 + xFudge,
       y = foo[-(nCols + 1)] + yFudge,
       labels = substr(countryColors$country[countryColors$continent == thisCont],
                       1, charLimit),
       adj = c(0, 0), cex = jCex)
}
mtext(cDat$continent, side = 3, at = seq_len(nCont) - 0.5)
mtext(c("smallest\npop", "largest\npop"),
      side = 2, at = c(0.9, 0.1), las = 1)
par(op)
## I like it!

dev.print(pdf,
          file = paste0(whereAmI,"figs/bryan-a01-colorScheme.pdf"),
          width = 7, height = 10)


write.table(countryColors,
            paste0(whereAmI, "data/gapminderCountryColors.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)

write.table(cDat,
            paste0(whereAmI, "data/gapminderContinentColors.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)


