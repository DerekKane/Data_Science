## code snippets for demonstrating how to work with colors, esp. in
## base R graphics
## uses the gapminder data as example

## WARNING: This file does not present a coherent analysis.  It's a
## series of random snippets to support a lecture.  In some cases I
## deliberately write BAD code, to make a pedagogical point.  You have
## been warned.

## IN PARTICULAR, I'm being very sloppy about par().  For example, I'm
## not being careful to store the original state and restore it.
## Sorry about that.  It's stuff like this that made me switch to
## lattice.

## this is not a great function, but is better than having this
## repetitive command littering up the script
jPrintToPdf <- function(j) {
  figFileName <- paste0("basicColorDemo-",
                        formatC(j, width = 2, flag = "0"), ".pdf")
  cat("writing", figFileName, "\n")
  dev.print(pdf,
            paste0(whereAmI, "figs/basicColorDemo/", figFileName),
            width = 7, height = 7)
}


## grab the Gapminder data to use in examples
whereAmI <- "/Users/jenny/teaching/STAT545A/examples/gapminder/"

## data import from local file
gDat <- read.delim(paste0(whereAmI,
                          "data/gapminderDataFiveYear.txt"))

## reach out and touch the data
str(gDat)
## 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3  ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...

## take a random sample of countries
nC <- 8

## code used initially
##(countriesToKeep <- as.character(sample(levels(gDat$country),
##                                        size = nC)))
## sadly I failed to set.seed() and can never draw this same random
## sample again :( so I'm hard-wiring here
countriesToKeep <- c("Iran", "Hong Kong, China", "Jordan", "Congo, Dem. Rep.",
                     "Bangladesh", "South Africa", "Sierra Leone", "Malaysia")
jYear <- 2007

jDat <-
  droplevels(subset(gDat, country %in% countriesToKeep & year == jYear))
jDat <- jDat[order(jDat$gdpPercap),]
str(jDat)
## 'data.frame':	8 obs. of  6 variables:
##  $ country  : Factor w/ 8 levels "Bangladesh","Congo, Dem. Rep.",..: 2 7 1 5 8..
##  $ year     : int  2007 2007 2007 2007 2007 2007 2007 2007
##  $ pop      : num  6.46e+07 6.14e+06 1.50e+08 6.05e+06 4.40e+07 ...
##  $ continent: Factor w/ 2 levels "Africa","Asia": 1 1 2 2 1 2 2 2
##  $ lifeExp  : num  46.5 42.6 64.1 72.5 49.3 ...
##  $ gdpPercap: num  278 863 1391 4519 9270 ...
jDat
##               country year       pop continent lifeExp  gdpPercap
## 336  Congo, Dem. Rep. 2007  64606759    Africa  46.462   277.5519
## 1356     Sierra Leone 2007   6144562    Africa  42.568   862.5408
## 108        Bangladesh 2007 150448339      Asia  64.062  1391.2538
## 816            Jordan 2007   6053193      Asia  72.535  4519.4612
## 1416     South Africa 2007  43997828    Africa  49.339  9269.6578
## 732              Iran 2007  69453570      Asia  70.964 11605.7145
## 948          Malaysia 2007  24821286      Asia  74.241 12451.6558
## 672  Hong Kong, China 2007   6980412      Asia  82.208 39724.9787



jXlim <- c(200, 50000)
jYlim <- c(21, 84)
i <- 0


par(las = 1, pch = 16, cex = 1.5,
    mar = c(4, 4, 2, 1))


(i <- i + 1)
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     main = "Start your engines ...")
jPrintToPdf(i)


(i <- i + 1)
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = "red", main = 'col = "red"')
jPrintToPdf(i)


(i <- i + 1)
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = c("red", "green"),
     main = 'col = c("red", "green")')
jPrintToPdf(i)

(i <- i + 1)
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = 1:nC,
     main = 'col = 1:nC')
with(jDat,
     text(x = gdpPercap, y = lifeExp, pos = 1))
jPrintToPdf(i)

palette()


(i <- i + 1)
jColors <- c('chartreuse3', 'cornflowerblue',
             'darkgoldenrod1', 'peachpuff3',
             'mediumorchid2', 'turquoise3',
             'wheat4', 'slategray2')
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = jColors,
     main = 'col = jColors')
jPrintToPdf(i)


colors()




library(RColorBrewer)

par(cex = 1)
display.brewer.all()
dev.print(pdf,
          file = paste0(whereAmI,"figs/RColorBrewerPalettes.pdf"),
          width = 7, height = 9)

display.brewer.pal(n = 8, name = 'Dark2')
dev.print(pdf,
          file = paste0(whereAmI,"figs/RColorBrewerDark2.pdf"),
          width = 7, height = 3)

display.brewer.all(type = "div")
dev.print(pdf,
          file = paste0(whereAmI,"figs/RColorBrewerDivPalettes.pdf"),
          width = 7, height = 7)


par(cex = 2)

(i <- i + 1)
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = brewer.pal(n = 8, name = "Dark2"),
     main = 'col = brewer.pal(n = 8, name = "Dark2")',
     cex.main = 0.75)
jPrintToPdf(i)


(jColors <- brewer.pal(n = 8, name = "Dark2"))
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = jColors,
     main = 'col = brewer.pal(n = 8, name = "Dark2")',
     cex.main = 0.75)


## adding a factor that I want to convey via color
(jLevels <- paste0("grp", 1:3))
##jDat$group <- factor(sample(jLevels, nC, replace = TRUE))
## oops ... hard-wiring the result I got randomly earlier
jDat$group <- factor(paste0("grp", c(1, 1, 1, 2, 2, 1, 3, 3)))
jDat
(jColors <- data.frame(group = jLevels,
                       color = I(brewer.pal(n = 3, name = 'Dark2'))))

jColors$color[match(jDat$group, jColors$group)]

(i <- i + 1)
plot(lifeExp ~ gdpPercap, jDat, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = jColors$color[match(jDat$group, jColors$group)],
     main = 'col = jColors$color[match(jDat$group, jColors$group)]',
     cex.main = 0.5)
legend(x = 'bottomright',
       legend = as.character(jColors$group),
       col = jColors$color, pch = 16, bty = 'n', xjust = 1)
jPrintToPdf(i)

(i <- i + 1)
jDatVersion2 <- merge(jDat, jColors)
jDatVersion2[c('country','gdpPercap','lifeExp','group','color')]
plot(lifeExp ~ gdpPercap, jDatVersion2, log = 'x',
     xlim = jXlim, ylim = jYlim,
     col = color,
     main = 'col = jDatVersion2$color',
     cex.main = 1)
legend(x = 'bottomright',
       legend = as.character(jColors$group),
       col = jColors$color, pch = 16, bty = 'n')
jPrintToPdf(i)

