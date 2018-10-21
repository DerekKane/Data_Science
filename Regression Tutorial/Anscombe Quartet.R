# Anscombe Quartet Example

mydata <- anscombe
attach(mydata)

##########################################################################

# load the package

library(ggplot2)

##########################################################################

# Notice that the mean values of the Y variables are all identical

summary(mydata)

#-------------------------------------------------------------------------

# Create the plot and regression for the 1st Chart

chart1 <- plot(x1,y1)

lm1 <- lm(y1~x1, data=mydata)
summary(lm1)

abline(coefficients(lm1))


#-------------------------------------------------------------------------
# Create the plot and regression for the 2nd Chart

chart2 <- plot(x2,y2)

lm2 <- lm(y2~x2, data=mydata)
summary(lm2)

abline(coefficients(lm2))

#-------------------------------------------------------------------------
# Create the plot and regression for the 3rd Chart

chart3 <- plot(x3,y3)

lm3 <- lm(y3~x3, data=mydata)
summary(lm3)

abline(coefficients(lm3))

#-------------------------------------------------------------------------
# Create the plot and regression for the 4th Chart

chart4 <- plot(x4,y4)

lm4 <- lm(y4~x4, data=mydata)
summary(lm4)

abline(coefficients(lm4))

##########################################################################

??anscombe


require(stats); require(graphics)
summary(anscombe)

##-- now some "magic" to do the 4 regressions in a loop:
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

## See how close they are (numerically!)
sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)
