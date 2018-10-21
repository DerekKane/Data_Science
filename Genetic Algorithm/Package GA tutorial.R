
library(GA)

data("eurodist", package="datasets")
D <- as.matrix(eurodist)

eurodist

# The fitness function

# The fitness function to me maximized can be definied as the reciproical 
# of the tour length.

tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour,2)[,2:1]
  sum(distMatrix[route])
}

tspFitness <- function(tour, ...) 1/tourLength(tour, ...)

GA <- ga(type= "permutation", fitness= tspFitness, distMatrix=D, min=1, max= attr(eurodist, "Size"), popSize=50, maxiter = 5000, run=500, pmutation=0.2)

summary(GA)

# The solutions correspond to a unique path equal to:

apply(GA@solution, 1, tourLength, D)


# This solution will show the map of the ideal route produced by the GA.

mds <- cmdscale(eurodist)
x <- mds[, 1]
y <- -mds[, 2]
plot(x,y, type= "n", asp=1, xlab="", ylab="")
abline(h= pretty(range(x), 10), v= pretty(range(y), 10), col="light gray")
tour <- GA@solution[1,]
tour <- c(tour, tour[1])
n <- length(tour)
arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length=0.15, angle=25, col = "steelblue", lwd=2)
text(x,y, labels(eurodist), cex=0.8)



