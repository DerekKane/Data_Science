plotGapminderOneYear <-
  function(jYear, gapDat, countryDat, contDat,
           jXlim = c(200, 58000),
           jYlim = c(21, 88),
           jXlab = "Income per person (GDP/capita, inflation-adjusted $)",
           jYlab = "Life expectancy at birth (years)",
           jLightestGray = "gray90",
           jLightGray = "gray80",
           jDarkGray = 'grey20',
           jPch = 21,
           jFontsize = 200,
           jCexDivisor = 1500
  ) {
    
    ## grid.text() is used to place year in background
    require("grid")
    
    ## map pop into circle radius
    jPopRadFun <- function(jPop) {          # make area scale with pop
      sqrt(jPop/pi)
    }
    
    ## functions needed to annotate x = gdpPercap axis
    ## wrt original scale, despite log transformation
    logTicks <- function (lim, loc = c(1, 5)) {
      ii <- floor(log10(range(lim))) + c(-1, 2)
      main <- 10^(ii[1]:ii[2])
      r <- as.numeric(outer(loc, main, "*"))
      r[lim[1] <= r & r <= lim[2]]
    }
    
    xscale.components.log10 <- function(lim, ...) {
      ans <- xscale.components.default(lim = lim, ...)
      tick.at <- logTicks(10^lim, loc = 1:9)
      tick.at.major <- logTicks(10^lim, loc = c(1, 5))
      major <- tick.at %in% tick.at.major
      ans$bottom$ticks$at <- log(tick.at, 10)
      ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
      ans$bottom$labels$at <- log(tick.at, 10)
      ans$bottom$labels$labels <- as.character(tick.at)
      ans$bottom$labels$labels[!major] <- ""
      ans$bottom$labels$check.overlap <- FALSE
      ans
    }
    
    continentKey <-
      with(contDat,
           list(x = 0.95, y = 0.05, corner = c(1, 0),
                text = list(as.character(continent)),
                points = list(pch = jPch, col = jDarkGray, fill = color)))
    
    yDat <- subset(gapDat, year == jYear)
    xyplot(lifeExp ~ gdpPercap, yDat,
           xlab = jXlab, ylab = jYlab,
           scales = list(x = list(log = 10)),
           xlim = jXlim, ylim = jYlim,
           xscale.components = xscale.components.log10,
           key = continentKey,
           panel = function(x, y, ...) {
             grid.text(jYear,
                       gp = gpar(fontsize = jFontsize, col = jLightestGray))
             panel.grid(h = -1, v = 0, col.line = jLightGray)
             panel.abline(v = log10(logTicks(c(10, 50000), loc = 1:9)),
                          col.line = jLightGray)
             panel.points(x, y, col = jDarkGray, pch = jPch,
                          fill = yDat$color,
                          cex = jPopRadFun(yDat$pop)/jCexDivisor)
           })
  }


