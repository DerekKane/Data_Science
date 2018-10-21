whereAmI <-
  "/Users/jenny/teaching/2011/STAT545A/examples/yeastDeletionGrowth/"

gDat <- read.delim(jPaste(whereAmI, "data/gDat.txt"))

## basic description of dataset
str(gDat)                               # 5666 observations
## 'data.frame':	5666 obs. of  4 variables:
##  $ geneDel     : Factor w/ 5521 levels "YAL001C","YAL002W",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ chromo      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ chromoPretty: Factor w/ 16 levels "A / I","B / II",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ pheno       : num  9.39 9.4 10.38 10.54 8.65 ...

peek(gDat)

## basic counts relating to genes
table(table(gDat$geneDel))
## output:
##    1    2 
## 5376  145 
## 5376 genes appear exactly once
##  145 appears exactly twice
## 5376 + 145 = 5521 = number of levels in 'geneDel' factor

## basic counts relating to chromosomes
## cbind just makes it print nicer to screen, IMO
cbind(table(gDat$chromoPretty))
##          [,1]
## A / I      87
## B / II    370
## C / III   135
## D / IV    835
## E / V     248
## F / VI    105
## G / VII   561
## H / VIII  221
## I / IX    203
## J / X     326
## K / XI    302
## L / XII   500
## M / XIII  442
## N / XIV   373
## O / XV    506
## P / XVI   452

## it never hurts to check this
sum(table(gDat$chromoPretty))           # 5666
nrow(gDat)                              # 5666
## sometimes you discover NAs or other strange things, when these two
## numbers don't match

## this table is useful enough to store in an object
gFreqTable <-
  data.frame(obsCount = table(gDat$chromoPretty))
names(gFreqTable) <- c("chromo","nObs")
gFreqTable

## what if we want to count the number of unique genes on each
## chromosome?
## 'tapply' applies a function to a ragged array
gFreqTable$uniqGeneCount <- 
  cbind(tapply(gDat$geneDel, gDat$chromoPretty,
               function(x) {
                 return(length(unique(x)))
               }))
gFreqTable

## understand duplicated geneDels
gFreqTable$nDelGenes <- with(gFreqTable, nObs - uniqGeneCount)
gFreqTable

## facilitate putting this table in a report
write.table(gFreqTable,
            file = jPaste(whereAmI, "results/chromoFreq.txt"),
            quote = FALSE, row.names = FALSE, sep = "\t")
## see class notes for tips on ways to populate Word / LaTex / Excel
## tables with data from R

## delving deeper into these duplicated genes
geneFreq <- table(gDat$geneDel)
geneFreqReplicated <- subset(geneFreq, geneFreq > 1)
table(geneFreqReplicated)               # all appear exactly twice
duplicatedGenes <- names(geneFreqReplicated)
gDatDuplicates <- subset(gDat, geneDel %in% duplicatedGenes)
str(gDatDuplicates)
gDatDuplicates <- gDatDuplicates[order(gDatDuplicates$geneDel),]
head(gDatDuplicates, n = 20)
## aha! the row pairs seem to be exact duplicates!
## checking that carefully ...
firstInstance <- seq(along = duplicatedGenes) * 2 - 1
secondInstance <- firstInstance + 1
summary(gDatDuplicates$pheno[firstInstance] -
        gDatDuplicates$pheno[secondInstance])
## the phenotypes are exactly the same!!!
## must figure out why this is happening ... error in data provided by
## Giaever et al or in my pre-processing?
## for now, eliminate the second instance of these geneDels

## I am exploiting the fact that 'match' returns the position of the
## *first* match!
rowsToDelete <- match(duplicatedGenes, gDat$geneDel)
length(rowsToDelete)                    # 145, as expected
hDat <- gDat[-rowsToDelete,]
str(hDat)
## 'data.frame':	5521 obs. of  4 variables:
##  $ geneDel     : Factor w/ 5521 levels "YAL001C","YAL002W",..: 1 2 3 4 5 6 7 8..
##  $ chromo      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ chromoPretty: Factor w/ 16 levels "A / I","B / II",..: 1 1 1 1 1 1 1 1 1 1 ..
##  $ pheno       : num  9.39 9.4 10.38 10.54 8.65 ...

peek(hDat)

## storing the cleaned dataset for later use
write.table(hDat,
            jPaste(whereAmI, "data/hDat.txt"),
            quote = FALSE, row.names = FALSE, sep = "\t")            

dotplot(table(hDat$chromoPretty),
        origin = 0, type = c("p", "h"),
        xlab = "# genes")
dev.print(pdf,
          jPaste(whereAmI, "figs/genesPerChromoDotplot.pdf"),
          width = 6, height = 8)


## BEGIN: making figures
densityplot(~pheno, hDat,
            xlab = "Growth phenotype", plot.points = FALSE)
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotLattice.pdf"),
          width = 8, height = 6)

plot(density(hDat$pheno),
     xlab = "Growth phenotype", main = "")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotBase.pdf"),
          width = 8, height = 6)

histogram(~ pheno, hDat, xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoHistogramLattice.pdf"),
          width = 8, height = 6)

plot(hist(hDat$pheno),
     xlab = "Growth phenotype", main = "")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoHistogramBase.pdf"),
          width = 8, height = 6)

densityplot( ~ pheno | chromoPretty, hDat,
            plot.points = FALSE, layout = c(4,4),
            xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotByChromo4by4.pdf"),
          width = 8, height = 6)

histogram(~pheno | chromoPretty, hDat,
          layout = c(4,4), nint = 35,
          xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoHistogramByChromo4by4.pdf"),
          width = 8, height = 6)

bwplot(chromoPretty ~ pheno, hDat,
       xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoBwplotByChromo.pdf"),
          width = 8, height = 6)

boxplot(pheno ~ chromoPretty, data = hDat,
        xlab = "Growth phenotype", horizontal = TRUE)
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoBoxplotByChromoBase.pdf"),
          width = 8, height = 6)

bwplot(chromoPretty ~ pheno, hDat,
       xlab = "Growth phenotype",
       panel = panel.violin)
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoViolinplotByChromo.pdf"),
          width = 8, height = 6)

bwplot(chromoPretty ~ pheno, hDat,
       panel = function(..., box.ratio) {
         panel.violin(..., col = "transparent",
                      varwidth = FALSE, box.ratio = box.ratio)
         panel.bwplot(..., fill = NULL, box.ratio = .1)
       })
dev.print(pdf, jPaste(whereAmI, "figs/phenoBwxAndViolinplotByChromo.pdf"),
          height = 9, width = 6)

## END: making basic figures


## BEGIN: compare dist'n on chromo 4 and 5
## making a sub-data.frame
iDat <- subset(hDat, chromo %in% 4:5)
str(iDat)                    # 'data.frame':	1002 obs. of  4 variables
table(iDat$chromo)
##   4   5 
## 754 248 

## highlighting issues around unused factor levels
table(iDat$chromoPretty)

densityplot( ~ pheno, iDat, groups = chromoPretty,
            plot.points = FALSE, auto.key = TRUE,
            xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotSuperposeChr4Chr5Ugly.pdf"),
          width = 8, height = 6)

## dropping unused factor levels
## See Chapter 5 of Spector (2008)
iDat <- droplevels(iDat)
levels(iDat$chromoPretty)
table(iDat$chromoPretty)

## compare via densityplot
densityplot(~pheno | chromoPretty, iDat,
            plot.points = FALSE, xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotPanelChr4Chr5.pdf"),
          width = 8, height = 6)

## remake superposed plot
densityplot(~pheno, iDat, groups = chromoPretty,
            plot.points = FALSE, auto.key = TRUE,
            xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoDensityplotSuperposeChr4Chr5.pdf"),
          width = 8, height = 6)

## t test
t.test(pheno ~ chromo, iDat)

## wilcoxon or mann-whitney test
wilcox.test(pheno ~ chromo, iDat)

## sadly, Kolmogorov-Smirnov test has no formula interface
jDat <- split(iDat$pheno, iDat$chromoPretty)
str(jDat)
## List of 2
##  $ D / IV: num [1:754] 9.78 8.23 9.82 7.62 8.24 ...
##  $ E / V : num [1:248] 10.57 8.62 10.6 10.57 8.93 ...

## alternative way to get same result:
## jDat <- list(chr4 = iDat$pheno[iDat$chromo == 4],
##              chr5 = iDat$pheno[iDat$chromo == 5])
ks.test(x = jDat[[1]], y = jDat[[2]])

## chi-square test
(jBreaks <- hist(iDat$pheno)$breaks)    # plot is nice by-product!
(jCounts <- sapply(jDat, function(jPheno) {
  hist(jPheno, breaks = jBreaks, plot = FALSE)$counts
}))
rownames(jCounts) <- 
  paste(jBreaks[-length(jBreaks)], jBreaks[-1], sep = " - ")
addmargins(jCounts)
chisq.test(jCounts)

histogram(~ pheno | chromoPretty, iDat,
          breaks = jBreaks, xlab = "Growth phenotype",
          layout = c(2, 1))
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoHistogramChiSquareBreaks.pdf"),
          width = 8, height = 6)

histogram(~ pheno, iDat,
          breaks = jBreaks, xlab = "Growth phenotype")
dev.print(pdf,
          jPaste(whereAmI, "figs/phenoHistogramChiSquareBreaksOverall.pdf"),
          width = 8, height = 6)

## extracting specific results and storing together

## first, packaging the chi-square test as a function
jChiSquareTest <- function(x, y) {
  jBreaks <- hist(c(x,y), plot = FALSE)$breaks
  jCounts <- cbind(hist(x, breaks = jBreaks, plot = FALSE)$counts,
                   hist(y, breaks = jBreaks, plot = FALSE)$counts)
  return(chisq.test(jCounts))
}

## understanding a hypothesis test result: t test
tTestResult <- t.test(pheno ~ chromo, iDat)
class(tTestResult)                      # "htest"
names(tTestResult)
tTestResult$statistic
tTestResult$p.value
tTestResult$estimate
tTestResult$conf.int

## understanding a hypothesis test result: ks test
ksTestResult <- ks.test(x = jDat[[1]], y = jDat[[2]])
class(ksTestResult)
names(ksTestResult)                     # quite different from t test
ksTestResult$statistic
ksTestResult$p.value
ksTestResult$estimate                   # not present
ksTestResult$conf.int                   # not present

## using a list to hold results of 4 different tests
jTestResults <-
  list(t = t.test(pheno ~ chromo, iDat),
       wilcox = wilcox.test(pheno ~ chromo, iDat),
       ks = ks.test(x = jDat[[1]], y = jDat[[2]]),
       chisq = jChiSquareTest(jDat[[1]], jDat[[2]]))
sapply(jTestResults, class)             # all htest
(jPvals <- sapply(jTestResults, function(yo) return(yo$p.value)))


## storing and accessing non-rectangular analytical results
## also useful for storing results when 'source()'ing R commands

## sending your R output to a file
## using a different pair of chromsomes for fun
kDat <- subset(hDat, chromo %in% 6:7)
kDat <- refactor(kDat)                  # jb personal function
                                        # cleans up unused factors for
                                        # all factors in a data.fram

sink(jPaste(whereAmI, "results/sinkDemo.txt"))

t.test(pheno ~ chromo, kDat)
wilcox.test(pheno ~ chromo, kDat)
ks.test(kDat$pheno[kDat$chromo == 6], kDat$pheno[kDat$chromo == 7])

sink()

