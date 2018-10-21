library(robustbase)                     # ltsReg(), lmrob()
library(RColorBrewer)

## grab the Gapminder data to use as an example
whereWasI <- "/Users/jenny/teaching/2011/STAT545A/examples/gapminder/"

whereAmI <- "/Users/jenny/teaching/2011/STAT545A/examples/robustRegression/"

## Gapminder data import
gDat <- read.delim(jPaste(whereWasI, "data/gapminderDataFiveYear.txt"))
str(gDat)
## 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3  ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ gdpPercap: num  779 821 853 836 740 ...

## lifeExp vs. time
## hoping to find a country that is begging for robust regression!

pdf(jPaste(whereAmI, "figs/lifeExpVsYear-allCountries.pdf"),
    width = 10, height = 7)

xyplot(lifeExp ~ year | country, gDat,
       layout = c(3, 2), type = c("p", "r", "g"))

dev.off()

## cambodia, rwanda, swaziland are all possibilities
## but am going with rwanda
hDat <- droplevels(subset(gDat, country == "Rwanda",
                          select = c(year, lifeExp, country, continent)))
str(hDat)
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  40 41.5 43 44.1 44.6 ...
##  $ country  : Factor w/ 1 level "Rwanda": 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 1 level "Africa": 1 1 1 1 1 1 1 1 1 1 ...

(n <- nrow(hDat))                      # n = 12, how quaint!
jYlim <- c(22, 53)                     # hold constant from here on

xyplot(lifeExp ~ year, hDat, type = c("p", "r", "g"),
       ylim = jYlim)

dev.print(pdf, jPaste(whereAmI, "figs/rwandaRaw.pdf"),
          width = 7, height = 4.5)

## there are two outliers
## flag for possible removal and visual emphasis
hDat$outlier <- FALSE
hDat$outlier[hDat$year %in% c(1992, 1997)] <- TRUE

## plain vanilla linear model
(yearMin <- min(hDat$year))             # 1952
lsFit <- lm(lifeExp ~ I(year - yearMin), hDat)
coef(lsFit)
##      (Intercept) I(year - yearMin) 
##      42.74194872       -0.04583147
summary(lsFit)

## now exclude the outliers
lsFitAlt <- lm(lifeExp ~ I(year - yearMin), hDat,
               subset = !outlier)
coef(lsFitAlt)
##      (Intercept) I(year - yearMin) 
##      41.99422084        0.07408486
summary(lsFitAlt)

## preparing to plot ... define a helper function
## will use inside custom panel functions to plot the various fits
jPanel <- function(jFit, jMeth, jCol = NULL, ...) {
  if(is.null(jCol)) {
    theColor <- jColor[jMeth]
  } else {
    theColor <- jCol
  }
  panel.xyplot(jYear, predict(jFit,
                              newdata = data.frame(year = jYear)),
               type = "l", col = theColor, ...)
}

## will use as the xVals when I predict from the various fits
(jYear <- with(hDat, sort(unique(year))))

## associate colors with the approaches
theMethods <- c("leastSq", "leastTrimSq", "MM")
display.brewer.all(type="qual")
jColor <- brewer.pal(n = length(theMethods), name = "Dark2")
names(jColor) <- theMethods

## when I want to suppress color
myGray <- "gray60"

## plot data and these first fits
plainPlot <-
  xyplot(lifeExp ~ year, hDat,
         groups = outlier,
#         auto.key = list(x = 0.1, y = 0.1, corner = c(0, 0)),
         type = c("g","p"), ylim = jYlim,
         xlab = "Year", ylab = "Life expectancy")
print(plainPlot)

dev.print(pdf, jPaste(whereAmI, "figs/rwandaShowOutliers.pdf"),
          width = 7, height = 4.5)


overlayPlot <- 
  xyplot(lifeExp ~ year, hDat,
         groups = outlier,  ylim = jYlim,
         xlab = "Year", ylab = "Life expectancy",
         panel = function(x, y, ...) {
           panel.grid(h = -1, v = -1)
           jPanel(lsFit, "leastSq", myGray)
           panel.xyplot(x, y, ...)
         })
print(overlayPlot)

dev.print(pdf, jPaste(whereAmI, "figs/rwandaLeastSquares.pdf"),
          width = 7, height = 4.5)


overlayPlotAlt <-
  xyplot(lifeExp ~ year, hDat,
         groups = outlier, ylim = jYlim,
         xlab = "Year", ylab = "Life expectancy",
         panel = function(x, y, ...) {
           panel.grid(h = -1, v = -1)
           jPanel(lsFitAlt, "leastSq", myGray)
           panel.xyplot(x, y, ...)
       })

print(overlayPlotAlt)

dev.print(pdf, jPaste(whereAmI, "figs/rwandaLeastSquaresNoOut.pdf"),
          width = 7, height = 4.5)

## minimizing the trimmed mean of the squared residuals
ltsFit <- ltsReg(lifeExp ~ I(year - yearMin), hDat)
ltsFit
##         Intercept  I(year - yearMin)  
##           40.5682             0.1947
summary(ltsFit)
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## Intercept         40.56821    0.33083  122.62 6.84e-10 ***
## I(year - yearMin)  0.19467    0.01835   10.61 0.000129 ***


ltsFitAlt <- ltsReg(lifeExp ~ I(year - yearMin), hDat,
                    subset = !outlier)
ltsFitAlt
##        Intercept  I(year - yearMin)  
##          40.5682             0.1947
summary(ltsFitAlt)
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## Intercept         40.56821    0.33083  122.62 6.84e-10 ***
## I(year - yearMin)  0.19467    0.01835   10.61 0.000129 ***


## using an MM estimator
mmFit <- lmrob(lifeExp ~ I(year - yearMin), hDat)
mmFit
##      (Intercept)  I(year - yearMin)  
##          42.0758             0.0668
summary(mmFit)
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       42.07577    0.82850  50.785 2.12e-13 ***
## I(year - yearMin)  0.06679    0.04306   1.551    0.152    

mmFitAlt <- lmrob(lifeExp ~ I(year - yearMin), hDat,
                  subset = !outlier)
mmFitAlt
##      (Intercept)  I(year - yearMin)  
##         42.01527            0.07648
summary(mmFitAlt)
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       42.01527    0.87992  47.749 4.09e-11 ***
## I(year - yearMin)  0.07648    0.03495   2.188   0.0601 .  

## gathering all the fits together
jEst <- data.frame(est = rep(0, 6),     # 6 fits
                   se = as.numeric(NA),
                   meth = factor(rep(theMethods, each = 2),
                     levels = theMethods),
                   excl = rep(c(FALSE, TRUE), 3))


## least squares
i <- 1
jEst[i, c("est", "se")] <-
  summary(lsFit)$coef["I(year - yearMin)", c("Estimate", "Std. Error")]

## least squares, exclude outliers
i <- i + 1
jEst[i, c("est", "se")] <-
  summary(lsFitAlt)$coef["I(year - yearMin)", c("Estimate", "Std. Error")]

## least trimmed squared deviation
i <- i + 1
jEst[i, c("est", "se")] <-
  summary(ltsFit)$coef["I(year - yearMin)", c("Estimate", "Std. Error")]

## least trimmed squared deviation, exclude outliers
i <- i + 1
jEst[i, c("est", "se")] <-
  summary(ltsFitAlt)$coef["I(year - yearMin)", c("Estimate", "Std. Error")]

## MM estimation
i <- i + 1
jEst[i, c("est", "se")] <-
  summary(mmFit)$coef["I(year - yearMin)", c("Estimate", "Std. Error")]

## MM estimation, exclude outliers
i <- i + 1
jEst[i, c("est", "se")] <-
  summary(mmFitAlt)$coef["I(year - yearMin)", c("Estimate", "Std. Error")]

jEst
##           est         se        meth  excl
## 1 -0.04583147 0.10968601     leastSq FALSE
## 2  0.07408486 0.02725230     leastSq  TRUE
## 3  0.19467143 0.01835124 leastTrimSq FALSE
## 4  0.19467143 0.01835124 leastTrimSq  TRUE
## 5  0.06679455 0.04305619          MM FALSE
## 6  0.07647548 0.03494911          MM  TRUE

str(jEst)
## 'data.frame':	6 obs. of  4 variables:
##  $ est   : num  -0.0458 0.0741 0.1947 0.1947 0.0668 ...
##  $ se    : num  0.1097 0.0273 0.0184 0.0184 0.0431 ...
##  $ meth  : Factor w/ 3 levels "leastSq","leastTrimSq",..: 1 1 2 2 3 3
##  $ excl: logi  FALSE TRUE FALSE TRUE FALSE TRUE

## plotting these fits
methKey <-
  list(x = 0.05, y = 0.05, corner = c(0, 0),
       text = list(levels(jEst$meth)),
       lines = list(col = jColor))

xyplot(lifeExp ~ year, hDat,
       xlab = "Year", ylab = "Life expectancy",
       ylim = jYlim,
       panel = function(x, y, ...) {
         panel.grid(h = -1, v = -1)
         jPanel(lsFit, "leastSq")
         jPanel(mmFit, "MM")
         ## sadly ltsReg offers no predict method
         panel.xyplot(hDat$year, fitted(ltsFit),
                      type = "l", col = jColor["leastTrimSq"], ...)
         panel.xyplot(x, y, ...)
       },
       key = methKey)

dev.print(pdf, jPaste(whereAmI, "figs/rwandaThreeMethods.pdf"),
          width = 8, height = 6)

xyplot(lifeExp ~ year, hDat,
       xlab = "Year", ylab = "Life expectancy",
       ylim = jYlim,
       panel = function(x, y, ...) {
         panel.grid(h = -1, v = -1)
         jPanel(lsFitAlt, "leastSq")
         jPanel(mmFitAlt, "MM")
         ## sadly ltsReg offers no predict method
         panel.xyplot(hDat$year[!hDat$outlier],
                      fitted(ltsFitAlt),
                      type = "l", col = jColor["leastTrimSq"], ...)
         panel.xyplot(x, y, ...)
       },
       key = methKey)

dev.print(pdf, jPaste(whereAmI, "figs/rwandaThreeMethodsNoOut.pdf"),
          width = 8, height = 6)

## printing estimates and se's to paste as table in Keynote
(foo <- subset(jEst, select = c(meth, excl, est, se)))
foo$est <- round(foo$est, 3)
foo$se <- round(foo$se, 4)
write.table(foo, jPaste(whereAmI, "results/ests.txt"),
            row.names = FALSE, quote = FALSE, sep = "\t")

## transitioning to bootstrap now
B <- 1000
set.seed(17)

bootIndices <- sample(1:n, n * B, replace = TRUE)

getBootDat <- function(jFit, jIndices) {
  return(sweep(matrix(resid(jFit)[bootIndices], nrow = n), 1,
               fitted(jFit), "+"))
}

bootDatLS <- getBootDat(lsFit, bootIndices)
bootDatLTS <- getBootDat(ltsFit, bootIndices)
bootDatMM <- getBootDat(mmFit, bootIndices)

bootCoefLS <- 
  apply(bootDatLS, 2, function(lifeExp) {
    coef(lm(lifeExp ~ I(jYear - yearMin)))["I(jYear - yearMin)"]
  })

str(bootCoefLS)
## num [1:1000] -0.096 -0.2053 -0.0859 0.3885 -0.151 ...

bootCoefLTS <- 
  apply(bootDatLTS, 2, function(lifeExp) {
    coef(ltsReg(lifeExp ~ I(jYear - yearMin)))["I(jYear - yearMin)"]
  })

bootCoefMM <- 
  apply(bootDatMM, 2, function(lifeExp) {
    coef(lmrob(lifeExp ~ I(jYear - yearMin)))["I(jYear - yearMin)"]
  })
## Warning messages:
## 1: In lmrob.S(x, y, control = control) :
##   S refinements did not converge (to tol=1e-07) in 200 iterations
## 2: In lmrob.S(x, y, control = control) :
##   S refinements did not converge (to tol=1e-07) in 200 iterations

## Do I see obvious problems in the MM bootstrap coef estimates?
summary(bootCoefMM)
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.28920  0.04213  0.06530  0.06592  0.08601  0.51860
nExt <- 3
(inspectMe <- which(rank(bootCoefMM) <= nExt |
                    rank(bootCoefMM) > (B - nExt)))
## [1]   4  12 116 683 803 863
lmrob(bootDatMM[ , 863] ~ I(jYear - yearMin))
## no warnings for these extreme values, so I conclude it's safe to
## proceed for today's purpose

## stack these up
bootCoef <-
  data.frame(est = c(bootCoefLS, bootCoefLTS, bootCoefMM),
             meth = factor(rep(theMethods, each = B),
               levels = theMethods))


## to help show how I did this
bootIndices[1:(3 * n)]

matrix(resid(lsFit)[bootIndices[1:(3 * n)]], nrow = n)

cbind(fitted(lsFit))

sweep(matrix(resid(lsFit)[bootIndices[1:(3 * n)]], nrow = n), 1,
      fitted(lsFit), "+")

(jBootAvgs <- tapply(bootCoef$est,
                    bootCoef$meth,
                    mean))
##     leastSq leastTrimSq          MM 
## -0.04487993  0.19367829  0.06591758 

(jBootSds <- tapply(bootCoef$est,
                    bootCoef$meth,
                    sd))
##     leastSq leastTrimSq          MM 
## 0.10100098  0.06588174  0.04937583 

getSlope <- function(jFit) coef(jFit)["I(year - yearMin)"]

(obsCoef <- c(getSlope(lsFit),
              getSlope(ltsFit), getSlope(mmFit)))
## I(year - yearMin) I(year - yearMin) I(year - yearMin) 
##       -0.04583147        0.19467143        0.06679455 

obsCoef - jBootAvgs                     # bias?
##       leastSq   leastTrimSq            MM 
## -0.0009515358  0.0009931385  0.0008769686 

(obsCoef - jBootAvgs)/jBootSds          # bias rel to sd?
##      leastSq  leastTrimSq           MM 
## -0.009421055  0.015074564  0.017761092 

obsCoef/jBootSds                        # pseudo test stat
##     leastSq leastTrimSq          MM 
##  -0.4537725   2.9548619   1.3527783 

#quantile(bootCoefs[1,], probs = c(0.025, 0.975))
#summary(lmFitQ)

jEst$seBoot <- NA
jEst$seBoot[!jEst$excl] <- jBootSds

jEst$avgBoot <- NA
jEst$avgBoot[!jEst$excl] <- jBootAvgs


## saving jEst again for use as table elsewhere
(foo <- subset(jEst, excl == FALSE, select = c(meth, est,
                                      avgBoot, se, seBoot)))
foo$est <- round(foo$est, 3)
foo$avgBoot <- round(foo$avgBoot, 3)
foo$se <- round(foo$se, 4)
foo$seBoot <- round(foo$seBoot, 4)
write.table(foo, jPaste(whereAmI, "results/estsBoot.txt"),
            row.names = FALSE, quote = FALSE, sep = "\t")


## bootstrap figures start here

## first few bootstrap datasets
xyplot(bootDatLS[,1] + bootDatLS[,2] + bootDatLS[,3] +
       bootDatLS[,4] + bootDatLS[,5] + bootDatLS[,6] ~ jYear,
       outer = TRUE, type = c("g","p","r"),
       xlab = "Year", ylab = "Life expectancy")

dev.print(pdf, paste(whereAmI, "figs/rwandaLSsixBoots.pdf", sep = ""),
          width = 8, height = 6)


densityplot(~ est | meth, bootCoef,
            layout = c(3, 1),
            xlab = "Bootstrap coefficients",
            type = c("p", "g"), n = 100)

dev.print(pdf, paste(whereAmI, "figs/bootCoefThreeMethodsPlotPoints.pdf", sep = ""),
          width = 10, height = 5)

densityplot(~ est | meth, bootCoef,
            layout = c(2, 1),
            subset = meth != "leastTrimSq",
            xlab = "Bootstrap coefficients",
            type = "g", n = 100,
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              if (packet.number() == 1) {
                jv <- jEst[1, "est"]
              } else {
                jv <- jEst[5, "est"]
              }
              panel.abline(v = jv, col = myGray)
            })

dev.print(pdf, paste(whereAmI, "figs/bootCoefTwoMethodsNoPoints.pdf", sep = ""),
          width = 10, height = 5)


densityplot(~ est | meth, bootCoef,
            layout = c(2, 1),
            subset = meth != "leastTrimSq",
            xlab = "Bootstrap coefficients",
            type = "g", n = 200,
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              if (packet.number() == 1) {
                jv <- jEst[1, "est"]
                jsd <- jEst[1, "se"]
              } else {
                jv <- jEst[5, "est"]
                jsd <- jEst[5, "se"]
              }
              panel.abline(v = jv, col = myGray)
              panel.mathdensity(args = list(mean = jv,
                                  sd = jsd), col.line = myGray)
            })

dev.print(pdf, paste(whereAmI, "figs/bootCoefTwoMethodsWithDensity.pdf", sep = ""),
          width = 10, height = 5)

densityplot(~ est | meth, bootCoef,
            layout = c(3, 1),
            xlab = "Bootstrap coefficients",
            type = "g", n = 200,
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              if (packet.number() == 1) {
                jv <- jEst[1, "est"]
                jsd <- jEst[1, "se"]
              } else if (packet.number() == 2) {
                jv <- jEst[3, "est"]
              } else {
                jv <- jEst[5, "est"]
                jsd <- jEst[5, "se"]
              }
              panel.abline(v = jv, col = myGray)
              if(packet.number() != 2) {
              panel.mathdensity(args = list(mean = jv,
                                  sd = jsd), col.line = myGray)
            }
            })

dev.print(pdf, paste(whereAmI, "figs/bootCoefThreeMethodsWithDensity.pdf", sep = ""),
          width = 10, height = 5)
