library(RColorBrewer)
library(latticeExtra)                   # ecdfplot (!)

whereAmI <-
  "/Users/jenny/teaching/2011/STAT545A/examples/yeastDeletionGrowth/"

## function that returns result from 4 different "two groups" tests
## for a given pair of chromosomes
source(jPaste(whereAmI, "code/10-twoGroupTests.R"))

## yeast deletion growth data
hDat <- read.delim(jPaste(whereAmI, "data/hDat.txt"))
str(hDat)                               # 5521 obs. of  4 variables
(nC <- length(levels(hDat$chromoPretty))) # no. of chromosomes = 16

## Goal: perform a two sample t test for each pair of chromosomes
## (or maybe some other kind of test)

## almost a cheap trick ... use the built-in function!
## ttr = t test results
ttr1 <- 
  pairwise.t.test(hDat$pheno, hDat$chromoPretty,
                  pool.sd = FALSE, p.adjust.method = "none")
class(ttr1)                             # "pairwise.htest"
names(ttr1)                             # what's in it?
ttr1$p.value[1:5, 1:5]                  # most relevant output

## We will learn more by implementing this ourselves.
## Plus we can easily use a different two-sample test.
## My code here is inspired by a thread on R-help.  Search for "form
## pairs variables" and you'll find it.

## 'combn' returns indices we can use to generate all possible pairs
## of chromosomes as a 2 * (16 choose 2) matrix
choose(nC, 2)                           # 120 possible pairs
jCombos <- combn(nC, 2)
str(jCombos)                            # two cols, 120 rows
jCombos[, 1:7]                          # first several pairs
jCombos <- t(jCombos)                   # I prefer the transpose
head(jCombos)

## use 'apply()' to visit each row of jCombos, i.e. each pair of
## chromosomes, and conduct various "two groups" tests for the
## associated pair of chromosomes
tgtRes <- 
  t(apply(jCombos, 1, function(jInd) {
    foo <- twoGroupTests(chromoA = jInd[1],
                         chromoB = jInd[2],
                         hDat)
    return(sapply(foo, function(yo) return(yo$p.value)))
  }))
## get lots of warnings() from the chi-squared test
## proceed anyway ....


## bring in the chromosome pair
tgtRes <- data.frame(chromoA = jCombos[ , 1],
                     chromoB = jCombos[ , 2],
                     tgtRes)

str(tgtRes)
## 'data.frame':	120 obs. of  6 variables:
##  $ chromoA: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ chromoB: int  2 3 4 5 6 7 8 9 10 11 ...
##  $ t      : num  1.04e-05 4.86e-03 2.67e-11 8.66e-01 6.26e-05 ...
##  $ wilcox : num  7.29e-07 2.10e-03 5.39e-11 7.77e-01 1.17e-05 ...
##  $ ks     : num  2.53e-06 1.87e-02 1.26e-08 7.68e-01 2.98e-05 ...
##  $ chisq  : num  2.19e-07 5.55e-02 3.27e-09 6.26e-03 3.54e-04 ...
peek(tgtRes)

## must handle some pesky zeroes returned as p-values from KS test
summary(tgtRes$ks)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000000 0.0002148 0.0077410 0.1361000 0.1860000 0.9587000 
tgtRes[which(tgtRes$ks == 0), ]
##    chromoA chromoB            t       wilcox ks        chisq
## 43       4       5 7.906832e-22 1.154823e-23  0 4.340871e-19

## A p-value of zero is harmful, recode as a very tiny number.
tgtRes$ks[tgtRes$ks == 0] <- 2 * .Machine$double.eps

## comparing these test results to each other
round(cor(subset(tgtRes, select = c(t, wilcox, ks, chisq))), 2)

splom(~ subset(tgtRes, select = c(t, wilcox, ks, chisq)),
      xlab = "p-values from two groups tests")
dev.print(pdf,
          jPaste(whereAmI, "figs/splomTwoGroupsPvalues.pdf"),
          width = 8, height = 8)

## want to look at log(p-values) but must subset and transform 'by
## hand'
## less control over log axis handling here than in xyplot :-(
foo <- subset(tgtRes, select = c(t, wilcox, ks, chisq))
foo <- sapply(foo, log10)

round(cor(foo), 2)

splom(~ foo,
      xlab = "log10 p-values from two groups tests")
dev.print(pdf,
          jPaste(whereAmI, "figs/splomTwoGroupsLog10Pvalues.pdf"),
          width = 8, height = 8)


head(tgtRes)

## more looks at the overall distribution of the p-values for various
## "two groups" tests
ecdfplot( ~ t + wilcox + ks + chisq, tgtRes,
         auto.key = TRUE)

ecdfplot( ~ t + wilcox + ks + chisq, tgtRes,
         auto.key = TRUE, scales = list(x = list(log = 10)))

densityplot( ~ t + wilcox + ks + chisq, tgtRes,
            auto.key = TRUE, scales = list(x = list(log = 10)))
## did not save these

## to make the heatmap we have in mind, we will need to store our
## p-values just like the return value from the built-in function
## pairwise.t.test

## let's practice!
ttr2Pvals <- matrix(NA, nrow = nC - 1, ncol = nC - 1)
ttr2Pvals[lower.tri(ttr2Pvals, diag = TRUE)] <- tgtRes$t
rownames(ttr2Pvals) <- levels(hDat$chromoPretty)[-1]
colnames(ttr2Pvals) <- levels(hDat$chromoPretty)[-nC]
ttr2Pvals[1:5, 1:5]                     # are they the same?
ttr1$p.value[1:5, 1:5]
all.equal(ttr2Pvals, ttr1$p.value)      # Yes, they are the same.

## small examples to illustrate techniques useful for square,
## symmetric matrices
(foo <- matrix(NA, nrow = 4, ncol = 4))
(filler <- LETTERS[1:10])
foo[lower.tri(foo, diag = TRUE)] <- filler
foo

(foo <- matrix(NA, nrow = 4, ncol = 4))
(filler <- LETTERS[1:6])
foo[lower.tri(foo)] <- filler
foo
foo[upper.tri(foo)] <- t(foo)[upper.tri(foo)]
foo

## also, know that R has the 'dist()' class for efficient storage and
## handling of square symmetric matrices

## storing the p-values from each "two groups" test in a nC by nC
## matrix and populating all cells sensibly ... will facilitate making
## figures

## first, practice with p-values from just one test
jP <- matrix(NA, nrow = nC, ncol = nC)
jP[lower.tri(jP)] <- tgtRes$t
rownames(jP) <- colnames(jP) <- levels(hDat$chromoPretty)
diag(jP) <- 1
jP[upper.tri(jP)] <- t(jP)[upper.tri(jP)]
str(jP)
jP[1:5, 1:5]

## now scaling up to all four "two groups" tests
jP <- lapply(subset(tgtRes, select = c(t, wilcox, ks, chisq)),
             function(pvalues) {
               foo <- matrix(NA, nrow = nC, ncol = nC)
               foo[lower.tri(foo)] <- pvalues
               rownames(foo) <- colnames(foo) <- levels(hDat$chromoPretty)
               diag(foo) <- 1
               foo[upper.tri(foo)] <- t(foo)[upper.tri(foo)]
               return(foo)
             })
str(jP)                                 # list of 4 nC by nC matrices
str(jP$wilcox)
jP$wilcox[1:5, 1:5]

## Goal: present these p-values to an actual human being, who needs to
## absorb the message quickly and with as little pain as possible.

## graphs depicting the pairwise comparisons of phenotype dist'n
## shade the squares according to p-value
levelplot(jP$t, scales = list(x = list(rot = 45))) # ugly but it's a start
dev.print(pdf,
          jPaste(whereAmI, "figs/levPlot01.pdf"),
          width = 8, height = 6)

## getting a better color ramp
## RColorBrewer provides a good start in this area
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)
title("RColorBrewer palettes")

display.brewer.pal(n = 9, "GnBu")
text(x = 1:9, y = rep(1, 9), labels = 1:9)
title("RColorBrewer palette 'GnBu'")
dev.print(pdf,
          jPaste(whereAmI, "figs/RColorBrewerGnBu.pdf"),
          width = 8, height = 6)

## pick palette to focus on
brewPal <- brewer.pal(n=9,"GnBu")
colorFun <- colorRampPalette(rev(brewPal))
myCols <- colorFun(100)

## improved plot, color-wise
levelplot(jP$t, col.regions = myCols, scales = list(x = list(rot = 45)),
          xlab = "", ylab = "",
          main = "Colors based on 'GnBu' palette from RColorBrewer")
dev.print(pdf,
          jPaste(whereAmI, "figs/levPlot02.pdf"),
          width = 8, height = 6)

## including densityplots alongside is helpful
lPlot <- levelplot(jP$t, col.regions = myCols, xlab = "", ylab = "",
                   scales = list(x = list(rot = 45)))

dPlot <-
  densityplot(~ pheno | chromoPretty, hDat,
              layout = c(1, nC), plot.points = FALSE,
              strip = FALSE,
              strip.left = strip.custom(horizontal = TRUE,
                bg = NA),
              par.strip.text = list(lines = 3),
              scales = list(draw = FALSE),
              xlab = "", ylab = "")

print(dPlot, pos = c(0, 0.1, 0.2, 0.96), more = TRUE)
print(lPlot, pos = c(0.15, 0, 1, 1), more = FALSE)
dev.print(pdf,
          jPaste(whereAmI, "figs/levPlot03.pdf"),
          width = 8, height = 6)

## The plot will be easier to read if the chromosomes are ordered more
## logically.  Alphabetical order is silly here!

## I generated plots using the different orderings interactively.
## I.e. use your head when you submit the code below

## order by median phenotype
jOrd1 <- order(tapply(hDat$pheno, hDat$chromoPretty, median))
jTitle <- "Ordered by median phenotype"
jOrd <- jOrd1

## order by hierarchical clustering
jOrd2 <- order.dendrogram(as.dendrogram(hclust(dist(jP$t))))
jTitle <- "Ordered by clustering result"
jOrd <- jOrd2

## order by Jenny, inspired by above
jOrd3 <- 
  c(4, 7, 8, 6, 9, 2, 14, 12, 10, 16, 3, 15, 11, 13, 1,  5)
jTitle <- "Ordered by Jenny"
jOrd <- jOrd3

lPlot <- levelplot(jP$t[jOrd, jOrd], col.regions = myCols,
                   xlab = "", ylab = "", main = jTitle,
                   scales = list(x = list(rot = 45)))

## You can update a lattice plot object!
dPlot <- update(dPlot, index.cond = list(jOrd))

print(dPlot, pos = c(0, 0.1, 0.2, 0.96), more = TRUE)
print(lPlot, pos = c(0.15, 0, 1, 1), more = FALSE)
dev.print(pdf,
#          jPaste(whereAmI, "figs/levPlot04medianOrder.pdf"),
#          jPaste(whereAmI, "figs/levPlot04hclustOrder.pdf"),
          jPaste(whereAmI, "figs/levPlot04jennyOrder.pdf"),
          width = 8, height = 6)

## transforming the p-values to optimize mapping into the color ramp
## I want to de-emphasize differences between teeny p-values
## and between very large p-values
## and increase the visual impact of differences in (0.01, 0.10)
pvalueTicks <- c(0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1)
qnScale <- 0.6
colorkeyTicks <- qnorm(pvalueTicks * qnScale)
jTitle <- "t-test, JB order, probit transformed p-values"

lPlot <-
  levelplot(qnorm(jP$t[jOrd, jOrd] * qnScale), col.regions = myCols,
            scales = list(x = list(rot = 45)),
            xlab = "", ylab = "", main = jTitle,
            colorkey = list(col = myCols,
              labels = list(labels = pvalueTicks, at = colorkeyTicks)))

print(dPlot, pos = c(0, 0.1, 0.2, 0.96), more = TRUE)
print(lPlot, pos = c(0.15, 0, 1, 1), more = FALSE)
dev.print(pdf,
          jPaste(whereAmI, "figs/levPlot05.pdf"),
          width = 8, height = 6)

## using the p-values from the KS test
jTitle <- "KS test, JB order, probit transformed p-values"
lPlotKS <-
  levelplot(qnorm(jP$ks[jOrd, jOrd] * qnScale), col.regions = myCols,
            scales = list(x = list(rot = 45)),
            xlab = "", ylab = "", main = jTitle,
            colorkey = list(col = myCols,
              labels = list(labels = pvalueTicks, at = colorkeyTicks)))

print(dPlot, pos = c(0, 0.1, 0.2, 0.96), more = TRUE)
print(lPlotKS, pos = c(0.15, 0, 1, 1), more = FALSE)
dev.print(pdf,
          jPaste(whereAmI, "figs/levPlot06.pdf"),
          width = 8, height = 6)

## most statistically significant differences
head(tgtRes[order(tgtRes$t), ], 15)
