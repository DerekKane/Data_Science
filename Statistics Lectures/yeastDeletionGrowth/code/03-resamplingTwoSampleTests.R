library(grid)                           # necessary for the particular
                                        # way I add text to panels
                                        # within lattice plots
library(latticeExtra)                   # useOuterStrips()

whereAmI <-
  "/Users/jenny/teaching/2011/STAT545A/examples/yeastDeletionGrowth/"

## yeast deletion growth data
hDat <- read.delim(jPaste(whereAmI, "data/hDat.txt"))
str(hDat)                               # 5521 obs. of  4 variables

## NOTE:
## I'm writing code to explain the bootstrap now,
## not to showcase elegance or efficiency.

## focusing on two chromosomes
jChromo <- c(10, 11)
kDat <- droplevels(subset(hDat, chromo %in% jChromo))
str(kDat)                               # 627 obs. of  4 variables:
(chromoCounts <- table(kDat$chromo))

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

## compute the observed test statistic
(chromoMeans <- with(kDat,
                    tapply(pheno, chromo, mean)))
(obsTestStat <- abs(chromoMeans[1] - chromoMeans[2]))

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
## one bootstrap sample
set.seed(12)
z <- kDat$pheno
xStar <- sample(z, size = chromoCounts[1], replace = TRUE)
yStar <- sample(z, size = chromoCounts[2], replace = TRUE)
bootDat <- with(kDat,
                data.frame(pheno = c(xStar, yStar),
                           chromo, chromoPretty))
(bootMeans <- with(bootDat,
                   tapply(pheno, chromo, mean)))
##       10       11 
## 8.980664 9.062216 

(bootTestStat <- abs(diff(bootMeans)))
##         11 
## 0.08155161 

plot1 <- densityplot(~pheno, kDat)

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
B <- 10
bootTestStat <- rep(NA, B)
for(i in 1:B) {
  xStar <- sample(z, size = chromoCounts[1], replace = TRUE)
  yStar <- sample(z, size = chromoCounts[2], replace = TRUE)
  bootTestStat[i] <- abs(mean(xStar) - mean(yStar))
}
bootTestStat  
mean(bootTestStat >= obsTestStat)

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
