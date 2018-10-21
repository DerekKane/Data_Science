plotGapminderOneYear <-
  function(jYear, gapDat, contDat,
           jXlim = c(200, 50000),
           jYlim = c(21, 84),
           jXlab = "Income per person (GDP/capita, inflation-adjusted $)",
           jYlab = "Life expectancy at birth (years)",
           jLightGray = 'grey80',
           jDarkGray = 'grey20',
           gdpTicks = c(200, 400, 1000, 2000, 4000, 10000, 20000, 40000),
           lifeExpTicks = seq(from = 20, to = 85, by = 5),
           yearCex = 15
  ) {
    
    ## map pop into circle radius
    jPopRadFun <- function(jPop) {
      sqrt(jPop/pi)
    }
    
    plot(lifeExp ~ gdpPercap, gapDat, subset = year == jYear,
         log = 'x', xlim = jXlim, ylim = jYlim,
         xaxt = "n", yaxt = "n", type = "n",
         xlab = jXlab, ylab = jYlab)
    abline(v = gdpTicks, col = jLightGray)
    abline(h = lifeExpTicks, col = jLightGray)
    text(x = sqrt(prod(jXlim)), y = mean(jYlim),
         jYear, adj = c(0.5, 0.5), cex = yearCex, col = jLightGray)
    axis(side = 1, at = gdpTicks, labels = gdpTicks)
    axis(side = 2, at = lifeExpTicks, labels = lifeExpTicks, las = 1)
    with(subset(gapDat, year == jYear),
         symbols(x = gdpPercap, y = lifeExp,
                 circles = jPopRadFun(pop), add = TRUE, 
                 inches = 0.7,
                 fg = jDarkGray, bg = color))
    with(contDat,
         legend(x = 'bottomright', bty = 'n',
                legend = continent, fill = color))
  }

