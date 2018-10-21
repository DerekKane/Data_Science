## conduct 4 different "two groups" tests where 'chromo' variable
## encodes the groups
twoGroupTests <-
  function(chromoA = 1,
           chromoB = 2,
           data) {

    jChiSquareTest <- function(x, y) {
      jBreaks <- hist(c(x,y), plot = FALSE)$breaks
      jCounts <- cbind(hist(x, breaks = jBreaks, plot = FALSE)$counts,
                       hist(y, breaks = jBreaks, plot = FALSE)$counts)
      return(chisq.test(jCounts))
    }

    tinyDat <- droplevels(subset(data, chromo %in% c(chromoA, chromoB)))
    tinyList <- with(tinyDat, split(pheno, chromoPretty))
    testResults <-
      list(t = t.test(pheno ~ chromo, tinyDat),
           wilcox = wilcox.test(pheno ~ chromo, tinyDat),
           ks = ks.test(x = tinyList[[1]], y = tinyList[[2]]),
           chisq = jChiSquareTest(tinyList[[1]], tinyList[[2]]))
    return(testResults)
  }
