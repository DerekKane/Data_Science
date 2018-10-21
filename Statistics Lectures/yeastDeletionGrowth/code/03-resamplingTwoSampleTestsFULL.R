library(grid)                           # grid.text()
library(latticeExtra)                   # useOuterStrips()

whereAmI <-
  "/Users/jenny/teaching/2011/STAT545A/examples/yeastDeletionGrowth/"

## yeast deletion growth data
hDat <- read.delim(jPaste(whereAmI, "data/hDat.txt"))
str(hDat)                               # 5521 obs. of  4 variables

## focusing on two chromosomes
jChromo <- c(10, 11)
kDat <- droplevels(subset(hDat, chromo %in% jChromo))
str(kDat)                               # 627 obs. of  4 variables:
(chromoCounts <- table(kDat$chromo))
##  10  11 
## 325 302 
(n <- nrow(kDat))                       # 627 obs


## getting a visual overview of the data

## note: example of placing text in the panel
## commented out code might be useful for understanding what I'm doing
## and seeing alternative ways to get same result
densityplot(~ pheno | chromoPretty, kDat,
            xlab = "Growth phenotype",
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              
              ## put pre-computed info on the plot
              ## or sthg that can be computed on-the-fly
              
              ## grid.text(paste("packet.number() returns ", packet.number()),
              ## grid.text(paste(chromoCounts[packet.number()], 'genes'),
              grid.text(paste(length(x), 'genes'),                        
                        x = 0.1, y = 0.9,
                        just = c("left", "center"),
                        gp = gpar(fontfamily = "sans"))
            })

dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotPanelChr",
                 jChromo[1], "Chr", jChromo[2],".pdf"),
          width = 8, height = 6)

## consider the (non-standard) test statistic
## | avg(x) - avg(y) |

## NOTE: I'm writing code to explain the bootstrap now,
## not to showcase elegance or efficiency.

## compute the observed test statistic
(chromoMeans <- with(kDat,
                    tapply(pheno, chromo, mean)))
##       10       11 
## 8.943558 9.203379 

(obsTestStat <- abs(chromoMeans[1] - chromoMeans[2]))
## 0.2598215 

## note:example of adding a line and text
densityplot(~ pheno | chromoPretty, kDat,
            xlab = "Growth phenotype", layout = c(1,2),
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              mu <- mean(x)
              panel.abline(v = mu, lty = 'dotted')
              if(packet.number() == 1) {
                avgText <- bquote(bar(x) == .(round(mu, 2)))
              } else {
                avgText <- bquote(bar(y) == .(round(mu, 2)))
              }
              grid.text(avgText, x = 0.1, y = 0.9,
                        just = c("left", "center"))
            })

dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotPanelChr",
                 jChromo[1], "Chr", jChromo[2],"WithMean.pdf"),
          width = 5, height = 8)

## enter the world of the null hypothesis
## generate one bootstrap sample
set.seed(12)
z <- kDat$pheno
xStar <- sample(z, size = chromoCounts[1], replace = TRUE)
yStar <- sample(z, size = chromoCounts[2], replace = TRUE)

c(mean(xStar), mean(yStar))
## [1] 8.980664 9.062216

(bootTestStat <- abs(mean(xStar) - mean(yStar)))
##         11 
## 0.08155161 

plot1 <- densityplot(~pheno, kDat)

## easier to plot if I store the bootstrap data set differently,
## i.e. "stack it" into a data.frame
bootDat <- with(kDat,
                data.frame(pheno = c(xStar, yStar),
                           chromoPretty))

plot2 <-
  densityplot( ~ pheno | chromoPretty, bootDat,
              xlab = "Bootstrap data", layout = c(1, 2),
              panel = function(x, ...) {
                panel.densityplot(x, ...)
                mu <- mean(x)
                panel.abline(v = mu, lty = "dotted")
                if(packet.number() == 1) {
                  avgText <- bquote(bar(x) == .(round(mu, 2)))
                } else {
                  avgText <- bquote(bar(y) == .(round(mu, 2)))
                }
                grid.text(avgText, x = 0.1, y = 0.9,
                          just = c("left", "center"))
              })

print(plot1, pos = c(0, 0.25, 0.5, 0.75), more = TRUE)
print(plot2, pos = c(0.5, 0, 1, 1), more = FALSE)

dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotBootDataExampleTiny.pdf"),
          width = 8, height = 8)

## ten bootstrap samples
## NOTE: I do not recommend an explicit for loop in real life! Doing
## here just for clarity!
set.seed(23)
B <- 10
bootTestStat <- rep(NA, B)
for(i in 1:B) {
  xStar <- sample(z, size = chromoCounts[1], replace = TRUE)
  yStar <- sample(z, size = chromoCounts[2], replace = TRUE)
  bootTestStat[i] <- abs(mean(xStar) - mean(yStar))
}
bootTestStat
##  [1] 0.007943383 0.109329811 0.138009880 0.127795231 0.261677793 0.039274346
##  [7] 0.090984481 0.018088425 0.035747594 0.007489110

mean(bootTestStat >= obsTestStat)
## [1] 0.1

densityplot(~ bootTestStat,
            xlab = "bootstrap test stats = |avg x* - avg y*|",
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              panel.abline(v = obsTestStat, lty = 'dotted')
              grid.text(paste("bootstrap p-value =",
                              round(mean(bootTestStat >= obsTestStat), 2)),
                        x = 0.9, y = 0.9,
                        just = c("right", "center"))
            })

dev.print(pdf,
          jPaste(whereAmI, "figs/densityplotBootTestStatAbsMeanDiffTiny.pdf"),
          width = 8, height = 6)

## increasing B, putting more emphasis on how I would implement the
## bootstrap in actual practice
set.seed(101)
B <- 10000
bootDat <-
  matrix(sample(kDat$pheno, size = B * n, replace = TRUE),
         nrow = n, ncol = B)
str(bootDat)
## num [1:627, 1:10000] 9.73 6.15 9.96 9.4 9.46 ...

## let's look at the first 3 bootstrap datasets

## good example of data reshaping for the purposes of plotting
bootDatToPlot <- stack(as.data.frame(bootDat[ ,1:3]))
str(bootDatToPlot)
## 'data.frame':	1881 obs. of  2 variables:
##  $ values: num  9.73 6.15 9.96 9.4 9.46 ...
##  $ ind   : Factor w/ 3 levels "V1","V2","V3": 1 1 1 1 1 1 1 1 1 1 ...
bootDatToPlot <-                              # tidy up
  data.frame(chromoPretty = kDat$chromoPretty, # exploit recycling
             bootIter = unclass(bootDatToPlot$ind),
             pheno = bootDatToPlot$values)
peek(bootDatToPlot)

jPlot <-
  densityplot(~ pheno | factor(bootIter) * chromoPretty, bootDatToPlot,
              xlab = "(Bootstrap) Growth phenotype", layout = c(2,3),
              panel = function(x, ...) {
                panel.densityplot(x, ...)
                mu <- mean(x)
                panel.abline(v = mu, lty = 'dotted')
                grid.text(bquote(mu == .(round(mu, 2))),
                          x = 0.1, y = 0.9,
                          just = c("left", "center"))
              })
useOuterStrips(jPlot)

dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotBootDataExample.pdf"),
          width = 8, height = 6)

## function to compute my test statistic
computeAbsDiffMeans <- function(jResp, jFact) {
  groupMeans <- tapply(jResp, jFact, mean)
  return(abs(groupMeans[1] - groupMeans[2]))
}

## applying that function to the observed data to make sure I get same
## value as before
obsTestStat                             # 0.2598215 
(obsTestStat <-
 with(kDat, computeAbsDiffMeans(pheno, chromo)))
## 0.2598215 .... YES, AS BEFORE

## applying that function to each boostrap dataset
## note use of system.time to see how something takes
system.time(
    bootTestStats <-
            apply(bootDat, 2,
                  computeAbsDiffMeans, jFact = kDat$chromo)
            )                           # 7 - 8 seconds for me

bootTestStats[1:3]
## 0.17465224 0.06890944 0.03129661

## bootstrap p-value
mean(bootTestStats >= obsTestStat)
## [1]  0.0172

t.test(pheno ~ chromo, kDat)$p.value
## [1] 0.01940612
wilcox.test(pheno ~ chromo, kDat)$p.value
## [1] 0.001156313

## empirical distribution of these bootstrap test stats
densityplot( ~ bootTestStats,
            xlab = expression(group("|", bar(x) - bar(y),"|")),
            main = "Bootstrap test statistics",
            plot.points = FALSE, n = 200, ref = TRUE,
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              panel.abline(v = obsTestStat, lty = 'dotted')
            })

dev.print(pdf,
          jPaste(whereAmI, "figs/densityplotBootTestStatAbsMeanDiff.pdf"),
          width = 8, height = 6)

## using the bootstrap data to get empirical null dist'ns for other
## test stats

## | median(x) - median(y) |
computeAbsDiffMedians <- function(jResp, jFact) {
  groupMedians <- tapply(jResp, jFact, median)
  return(abs(groupMedians[1] - groupMedians[2]))
}

## | Welch's t statistic |
computeAbsWelch <- function(jResp, jFact) {
  return(abs(t.test(jResp ~ jFact)$statistic))
}

## Welch's t statistic
computeWelch <- function(jResp, jFact) {
  return(t.test(jResp ~ jFact)$statistic)
}

## Kolmogorov-Smirnov test stat
computeKs <- function(jResp, jFact) {
  jList <- split(jResp, jFact)
  return(ks.test(x = jList[[1]], y = jList[[2]])$statistic)
}

## Wilcoxon test stat
computeWilcoxon <- function(jResp, jFact) {
  return(wilcox.test(jResp ~ jFact)$statistic)
}

## using these various test statistics with the bootstrap data
## each statement might take up to ~10 secs
bootAbsDiffMean <-
  apply(bootDat, 2, computeAbsDiffMeans, jFact = kDat$chromo)
bootWelch <- apply(bootDat, 2, computeWelch, jFact = kDat$chromo)
bootAbsWelch <- apply(bootDat, 2, computeAbsWelch, jFact = kDat$chromo)
bootAbsDiffMed <-
  apply(bootDat, 2, computeAbsDiffMedians, jFact = kDat$chromo)
bootKs <- apply(bootDat, 2, computeKs, jFact = kDat$chromo)
## will get lots of warnings about ties ... so be it
bootWilcoxon <- apply(bootDat, 2, computeWilcoxon, jFact = kDat$chromo)

## gather bootstrap test statistics together
bootStats <-
  data.frame(absDiffMean = bootAbsDiffMean,
             absDiffMedian = bootAbsDiffMed,
             absWelch = bootAbsWelch,
             welch = bootWelch,
             ks = bootKs,
             wilcoxon = bootWilcoxon)
str(bootStats)
## 'data.frame':	10000 obs. of  6 variables:
##  $ absDiffMean  : num  0.1747 0.0689 0.0313 0.0668 0.0106 ...
##  $ absDiffMedian: num  0.2314 0.0349 0.1574 0.1195 0.1023 ...
##  $ absWelch     : num  1.5645 0.6022 0.2911 0.6073 0.0943 ...
##  $ welch        : num  -1.5645 -0.6022 -0.2911 0.6073 -0.0943 ...
##  $ ks           : num  0.072 0.0534 0.0741 0.0776 0.062 ...
##  $ wilcoxon     : num  46193 48418 46866 50942 47513 ...

## now I want to use the bootstrap test stats to get p-values for
## observed test statistics

## recall the chromosome-specific means ...
chromoMeans
##       10       11 
## 8.943558 9.203379

## ... and do same for median
(chromoMedians <- with(kDat,
                      tapply(pheno, chromo, median)))
##       10       11 
## 9.482804 9.804809 

## a list version of kDat is useful for the KS test which lacks a
## formula interface
kList <- with(kDat, split(pheno, chromo))
str(kList)
## List of 2
##  $ 10: num [1:325] 10.29 10.54 6.93 10.64 10.07 ...
##  $ 11: num [1:302] 10.57 10.54 6.83 8.63 9.4 ...


## this data.frame will have 1 row for each test statistic under
## consideration and columns to hold a description of the test
## statistic, the observed value, the classical p-value and the
## bootstrap p-value
statSigRes <-
  data.frame(testStat = I(c("abs diff means", "abs diff medians",
               "abs welch t", "welch t", "kolmogorov-smirnov",
               "wilcoxon")),
             obsStats =
             c(absDiffMean = abs(chromoMeans[1] - chromoMeans[2]),
               absDiffMedian = abs(chromoMedians[1] - chromoMedians[2]),
               absWelch = abs(t.test(pheno ~ chromo, kDat)$statistic),
               welch = t.test(pheno ~ chromo, kDat)$statistic,
               ks = ks.test(x = kList[[1]], y = kList[[2]])$statistic,
               wilcoxon = wilcox.test(pheno ~ chromo, kDat)$statistic),
             pValClassical = 1,
             pValBootstrap = 1)
rownames(statSigRes) <- statSigRes$testStat
statSigRes

## ugly but helpful to get oriented
densityplot(~ absDiffMean + absDiffMedian + absWelch + welch + ks +
            wilcoxon, bootStats,
            outer = TRUE, scales = "free")

## computing bootstrap p-values

## test stats where p-value is a RIGHT tail probability
statSigRes["abs diff means", "pValBootstrap"] <- 
  mean(bootStats$absDiffMean >= statSigRes["abs diff means", "obsStats"])

statSigRes["abs diff medians", "pValBootstrap"] <- 
  mean(bootStats$absDiffMedian >= statSigRes["abs diff medians", "obsStats"])

statSigRes["abs welch t", "pValBootstrap"] <- 
  mean(bootStats$absWelch >= statSigRes["abs welch t", "obsStats"])

statSigRes["kolmogorov-smirnov", "pValBootstrap"] <- 
  mean(bootStats$ks >= statSigRes["kolmogorov-smirnov", "obsStats"])

## test stats where p-value is LEFT + RIGHT tail probability
statSigRes["welch t", "pValBootstrap"] <- 
  mean(abs(bootStats$welch) >= abs(statSigRes["welch t", "obsStats"]))

## hint: the Wilcoxon U statistic is approximately normally
## distributed with mean (n1 * n2) /2
statSigRes["wilcoxon", "pValBootstrap"] <-
  mean(abs(bootStats$wilcoxon - prod(chromoCounts)/2) >=
       abs(statSigRes["wilcoxon", "obsStats"] - prod(chromoCounts)/2))

## computing "classical" p-values

## test stats with no "classical" p-value
statSigRes[c("abs diff means", "abs diff medians", "abs welch t"),
           "pValClassical"] <- NA

## test stats with a "classical" p-value
statSigRes[c("welch t", "kolmogorov-smirnov", "wilcoxon"),
           "pValClassical"] <- 
  round(c(t.test(pheno ~ chromo, kDat)$p.value,
          ks.test(x = kList[[1]], y = kList[[2]])$p.value,
          wilcox.test(pheno ~ chromo, kDat)$p.value), 4)

statSigRes$obsStats <- round(statSigRes$obsStats, 3)

statSigRes
##           testStat  obsStats pValClassical pValBootstrap
##     abs diff means     0.260            NA        0.0172
##   abs diff medians     0.322            NA        0.0140
##        abs welch t     2.344            NA        0.0170
##            welch t    -2.344        0.0194        0.0170
## kolmogorov-smirnov     0.134        0.0073        0.0051
##           wilcoxon 41710.000        0.0012        0.0011

jWriteTable(statSigRes,
            jPaste(whereAmI, "results/statSigRes.txt"),
            sep = "\t")


showOneBootstrapTest <-
  function(jBootTestStats, jObsTestStat, jPval,
           jDesc = "", jXlab = "test statistic",
           writeToFile = FALSE) {
    print(densityplot( ~ jBootTestStats,
                      xlab = jXlab,
                      plot.points = FALSE, n = 200, ref = TRUE,
                      panel = function(x, ...) {
                        panel.densityplot(x, ...)
                        panel.abline(v = jObsTestStat, lty = 'dotted')
                        jText <- paste(jDesc, "\n",
                                       "obs test stat =",
                                       round(jObsTestStat, 2), "\n",
                                       "p-value =", round(jPval, 4), "\n")
                        grid.text(jText,
                                  x = 0.9, y = 0.9,
                                  just = c("right", "center"),
                                  gp = gpar(fontfamily = "sans"))
                      }))
    if (writeToFile) {
      yo <- deparse(substitute(jBootTestStats))
      dev.print(pdf,
                jPaste(whereAmI, "figs/densityplotBootTest-", yo, ".pdf"),
                width = 8, height = 6)
    }
  }

with(bootStats,
     showOneBootstrapTest(absDiffMean,
                          statSigRes["abs diff means", "obsStats"],
                          statSigRes["abs diff means", "pValBootstrap"],
                          "abs. diff. of means",
                          jXlab = expression(group("|",
                              bar(x) - bar(y),"|")),
                          TRUE))

with(bootStats,
     showOneBootstrapTest(absDiffMedian,
                          statSigRes["abs diff medians", "obsStats"],
                          statSigRes["abs diff medians", "pValBootstrap"],
                          "| median(x) - median(y) |",
                          jXlab = "Abs. value of difference in medians",
                          TRUE))

with(bootStats,
     showOneBootstrapTest(absWelch,
                          statSigRes["abs welch t", "obsStats"],
                          statSigRes["abs welch t", "pValBootstrap"],
                          "| Welch's t stat |",
                          jXlab = "Abs. value of Welch's t statistic",
                          TRUE))

with(bootStats,
     showOneBootstrapTest(welch,
                          c(1, -1) * statSigRes["welch t", "obsStats"],
                          statSigRes["welch t", "pValBootstrap"],
                          "Welch's t stat",
                          jXlab = "Welch's t statistic",
                          TRUE))

with(bootStats,
     showOneBootstrapTest(ks,
                          statSigRes["kolmogorov-smirnov", "obsStats"],
                          statSigRes["kolmogorov-smirnov", "pValBootstrap"],
                          "KS test stat",
                          jXlab = "Kolmogorov-Smirnov test statistic",
                          TRUE))

with(bootStats,
     showOneBootstrapTest(wilcoxon,
                          prod(chromoCounts)/2 +
                          c(1, -1) * (statSigRes["wilcoxon", "obsStats"] -
                                      prod(chromoCounts)/2),
                          statSigRes["wilcoxon", "pValBootstrap"],
                          "Wilcoxon test stat",
                          jXlab =
                          "Wilcoxon test statistic, i.e. sum of 1 group's ranks",
                          TRUE))

