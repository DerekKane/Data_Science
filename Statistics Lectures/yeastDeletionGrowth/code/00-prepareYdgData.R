## WARNING: stale code, but maybe still interesting?
## including for completeness
library(lattice)

whereWasI <- "/Users/jenny/research/phenomics/giaever2004/"

load(paste(whereWasI,"data/normOBplateAnno.rda",sep=""))
ls()                                    # gDat
str(gDat)

## retain only the qnLog data
gDat <- gDat[grep("ORF|qnLog", names(gDat))]
str(gDat)

## parse ORF name to recover the chromosome number
gDat$chromo <- match(substr(gDat$ORF, 2, 2),LETTERS)
table(gDat$chromo)

gDat$chromoHyb <-
  factor(paste(LETTERS[gDat$chromo], "/", as.roman(gDat$chromo)))

## average over the 36 arrays
gDat$qnLog37 <- rowMeans(gDat[grep("qnLog[0-9]+",names(gDat))])

#density plots for each chromosome and globally
densityplot(~ qnLog37 | chromoHyb, gDat, plot.points = FALSE,
            main =
            "Smooth histograms of the phenotypes on each chromosome",
            layout = c(4,4))

## retaining only the average
gDat <- gDat[c("ORF","chromo","chromoHyb","qnLog37")]
str(gDat)
head(gDat)
gDat[sample(nrow(gDat),10),]

## better variable names
names(gDat) <- c("geneDel","chromo","chromoPretty","pheno")
str(gDat)
gDat[sample(nrow(gDat),10),]

## convert gene to character
gDat$geneDel <- levels(gDat$geneDel)[gDat$geneDel]
str(gDat)
gDat[sample(nrow(gDat),10),]
rownames(gDat) <- NULL

gDat[sample(nrow(gDat),10),]
head(gDat)

hDat <- subset(gDat, chromo %in% 4:5)
t.test(pheno ~ chromo, hDat)
wilcox.test(pheno ~ chromo, data = hDat)
ks.test(hDat$pheno[hDat$chromo == 4],
        hDat$pheno[hDat$chromo == 5])


## write this to file
whereAmI <- "/Users/jenny/teaching/2008/STAT545A/assignments/assmt03/"
write.table(gDat, file = paste(whereAmI, "gDat.txt", sep = ""),
            quote = FALSE, sep = "\t", row.names = FALSE)
