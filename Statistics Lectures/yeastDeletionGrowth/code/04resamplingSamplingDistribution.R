#library(lattice)                        # loads automaically for JB
library(boot)                           

whereAmI <-
  "/Users/jenny/teaching/2010/STAT545A/examples/yeastDeletionGrowth/"

## yeast deletion growth data
hDat <- read.delim(jPaste(whereAmI, "data/hDat.txt"))
str(hDat)                               # 5521 obs. of  4 variables

## bootstrap estimation of estimator's standard error
## loosely following example in V&R p. 133
jChromo <- 11                           # choose 1 chromo
x <- hDat$pheno[hDat$chromo == jChromo]
(nx <- length(x))                       # 302 for chr11
(jMedian <- median(x))                  # 9.80 for chr11

## plain densityplot
densityplot(~ x,
            xlab = "Growth phenotype",
            main = paste("Chromosome", jChromo),
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              mu <- median(x)
              panel.abline(v = mu, lty = 'dotted')
            })
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotChr", jChromo, ".pdf"),
          width = 8, height = 6)

## preparing to do inference about the median
estDens <- density(x, n = 200)
class(estDens)
names(estDens)
(fHatAtMedian <- estDens$y[which.min(abs(estDens$x - jMedian))])
(theorStdErr <- sqrt(1 / (4 * nx * fHatAtMedian^2)))

## densityplot w/ median detail added
jDefaultCol <- trellis.par.get()$superpose.symbol$col[1]

densityplot(~ x,
            main = paste("Chromosome", jChromo),
            xlab = "Growth Phenotype", n = 200,
            panel = function(...) {
              panel.densityplot(...)
              panel.abline(v = jMedian, lty = "dashed", col = jDefaultCol)
              panel.abline(h = fHatAtMedian, lty = "dotted", col = jDefaultCol)
            },
            key = list(x = 0.05, y = 0.95, corner = c(0,1),
              lines = list(col = rep(jDefaultCol, 3),
                lty = c("solid","dashed","dotted")),
              text = list(c("observed distribution",
                "observed median","estimated density at median"))))

dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotChr", jChromo, "Adorned.pdf"),
          width = 8, height = 6)

## carrying out the bootstrap 'by hand'
B <- 1000
bootData <-
  matrix(sample(x, size = B * nx, replace = TRUE),
         nrow = nx, ncol = B)
bootTestStat <- apply(bootData, 2, median)

## bootstrap est of std err vs the asymptotic result
(bootStdErr <- sqrt(var(bootTestStat)))
theorStdErr

## looking at bias
mean(bootTestStat)
jMedian
abs(mean(bootTestStat) - jMedian)/bootStdErr

## comparing bootstrap sampling distribution to the asymptotic
## distribution
jGray <- "grey70"

densityplot( ~ bootTestStat,
            panel = function(...) {
              panel.mathdensity(args = list(mean = jMedian,
                                  sd = theorStdErr),
                                col.line = jGray, n = 200)
              panel.densityplot(...)
              panel.abline(v = jMedian, lty = "dashed", col = jDefaultCol)
            },
            ylim = c(-0.2, 9.5), adjust = 0.8,
            main = paste("Sample median for chromosome", jChromo),
            xlab = "Bootstrap sample medians",
            key = list(x = 0.05, y = 0.95, corner = c(0,1),
              lines = list(col = c(jDefaultCol, jGray, jDefaultCol),
                lty = c("solid","solid","dashed")),
              text = list(c("bootstrap distribution",
                "asymptotic distribution","observed median"))))

dev.print(pdf,
          jPaste(whereAmI, "figs/bootVsTheorySampleMedian", jChromo, ".pdf"),
          width = 8, height = 5)

## carrying out the bootstrap w/ professional help
bootRes <- boot(x, function(z, i) median(z[i]), R = 1000)
class(bootRes)
names(bootRes)
bootRes
jMedian
mean(bootTestStat) - jMedian
bootStdErr

boot.ci(bootRes, conf = c(0.90, 0.95), type = "all")

plot(bootRes)
dev.print(pdf,
          paste(whereAmI, "figs/bootPackagePlot", jChromo, ".pdf", sep = ""),
          width = 8, height = 5)

## obtaining some bootstrap confidence intervals 'by hand'
confLevel <- 0.90
alpha <- 1 - confLevel

## percentile confidence interval
(jennyPercCI <- quantile(bootTestStat, probs = c(alpha / 2, 1 - alpha / 2)))
boot.ci(bootRes, conf = confLevel, type = "perc")

densityplot(~ bootTestStat,
            main = paste("Sample median for chromosome", jChromo),
            xlab = "Bootstrap sample medians", n = 300,
            panel = function(...) {
              panel.densityplot(...)
              panel.abline(v = jennyPercCI, lty = "dashed", col = jDefaultCol)
            })

dev.print(pdf,
          jPaste(whereAmI, "figs/bootPercentileConfInt", jChromo, ".pdf"),
          width = 8, height = 5)


## 'basic' confidence interval
ctrdBootMedians <- bootTestStat - jMedian
(critVals <- quantile(ctrdBootMedians, probs = c(alpha / 2, 1 - alpha / 2)))
c(jMedian - critVals[2], jMedian - critVals[1])
boot.ci(bootRes, conf = confLevel, type = "basic")

## impact of B of estimation of standard error
manyB <- c(20, 50, 100, 200, 500, 1000)

## I'm being a bit sloppy here, i.e. using x and nx, etc. from the
## environment; also hardwired for t(F) = median
## all of that would, ideally, be passed as arguments
getBootstrapStdErr <- function(myB) {
  bootData <-
    matrix(sample(x, size = myB * nx, replace = TRUE),
           nrow = nx, ncol = myB)
  return(sqrt(var(apply(bootData, 2, median))))
}

m <- 10                                  # reps per B
## I've never really known how to indent with system.time ...
system.time(
effectOfB <- sapply(manyB,
                    function(BB) replicate(m, getBootstrapStdErr(BB)))
            )                           # never more than 2 sec for me
colnames(effectOfB) <- paste("B",manyB, sep = "")
head(effectOfB)
effectOfB <- stack(as.data.frame(effectOfB))
effectOfB$B <- rep(manyB, each = m)
head(effectOfB)

xyplot(values ~ B, effectOfB,
       type = c('p','smooth'), ylab = "Bootstrap std err",
       main = "Effect of B on variability of bootstrap standard error")
dev.print(pdf,
          jPaste(whereAmI, "figs/effectOfB", jChromo, ".pdf"),
          width = 8, height = 6)

## testing if the median = a particular value
(medianNull <- median(hDat$pheno))
jMedian

## one 'classical' way to test this
binom.test(sum(x < medianNull), nx)
binom.test(sum(x >= medianNull), nx)

## carrying out the bootstrap 'by hand'

## very explicit demo of "enforce the null"!
xNull <- x - jMedian + medianNull

foo <-
  data.frame(pheno = c(x, xNull),
             origin = factor(rep(c("observed","null-ified"),
               each = nx),
               levels = c("observed","null-ified")))

densityplot(~ pheno, foo, groups = origin,
            auto.key = TRUE)
dev.print(pdf,
          jPaste(whereAmI, "figs/nullifiedPhenos", jChromo, ".pdf"),
          width = 8, height = 6)

## conduct the bootstrap
B <- 1000
bootData2 <-
  matrix(sample(xNull, size = B * nx, replace = TRUE),
         nrow = nx, ncol = B)
bootTestStat2 <- apply(bootData2, 2, median)

## I don't like hard-wiring ylim like this ...
densityplot(~ bootTestStat2,
            panel = function(...) {
              panel.abline(v = jMedian, lty = "dashed", col = jDefaultCol)
              panel.densityplot(...)
            },
            xlab = "Sample median", xlim = c(8.8, 9.85),
            main = "Bootstrap null distribution of sample median",
            key = list(x = 0.05, y = 0.95, corner = c(0,1),
              lines = list(col = jDefaultCol,
                lty = "dashed"),
              text = list("observed median")))

dev.print(pdf,
          jPaste(whereAmI, "figs/bootNullDistMedian", jChromo, ".pdf"),
          width = 8, height = 6)

summary(bootTestStat2)
jMedian
binom.test(sum(x < medianNull), nx)$p.value
1/B

(obsDiff <- abs(jMedian - medianNull))
summary(bootTestStat2 - medianNull)

sum(abs(bootTestStat2 - medianNull) >= obsDiff)
mean(abs(bootTestStat2 - medianNull) >= obsDiff)
