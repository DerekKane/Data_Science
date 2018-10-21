##############################################################
# Golden Ratio - Fibonacci Sequence
##############################################################


golden.ratio = (sqrt(5) + 1)/2
fibonacci.angle = 360/(golden.ratio ^2)

c=1
num_points <- 630
x=rep(0, num_points)
y=rep(0, num_points)

for (n in 1:num_points) {
  r=c*sqrt(n)
  theta=fibonacci.angle*(n)
  x[n]=r*cos(theta)
  y[n]=r*sin(theta)
}

plot(x,y,axes=FALSE,ann=FALSE,pch=19,cex=1)


##############################################################
# Golden Flower
##############################################################

t=1/2*(1+sqrt(5))
n=2^9
c=seq(1,n)

x=sqrt(c)*cos(2*pi*t*c)
y=sqrt(c)*sin(2*pi*t*c)

plot(x,y, pch=19, cex=1.5, col="red4")

# To draw a 34-spiral for Fibonacci number 34 starting from 1

plot(x,y,pch=1, cex=1)
v=seq(1,n,34)
lines(x[v],y[v])



# To draw a 8 & 13 spiral for Fibonacci number 8 starting from 1

plot(x,y,pch=19, cex=0.7, col="blue")

v=seq(1,n,8)
lines(x[v],y[v], col="red")

v=seq(2,n,8)
lines(x[v],y[v], col="red")

v=seq(3,n,8)
lines(x[v],y[v], col="red")

v=seq(4,n,8)
lines(x[v],y[v], col="red")

v=seq(5,n,8)
lines(x[v],y[v], col="red")

v=seq(6,n,8)
lines(x[v],y[v], col="red")

v=seq(7,n,8)
lines(x[v],y[v], col="red")

v=seq(8,n,8)
lines(x[v],y[v], col="red")

v=seq(1,n,13)
lines(x[v],y[v], col="green")

v=seq(2,n,13)
lines(x[v],y[v], col="green")

v=seq(3,n,13)
lines(x[v],y[v], col="green")

v=seq(4,n,13)
lines(x[v],y[v], col="green")

v=seq(5,n,13)
lines(x[v],y[v], col="green")

v=seq(6,n,13)
lines(x[v],y[v], col="green")

v=seq(7,n,13)
lines(x[v],y[v], col="green")

v=seq(8,n,13)
lines(x[v],y[v], col="green")

v=seq(9,n,13)
lines(x[v],y[v], col="green")

v=seq(10,n,13)
lines(x[v],y[v], col="green")

v=seq(11,n,13)
lines(x[v],y[v], col="green")

v=seq(1,n,13)
lines(x[v],y[v], col="green")

v=seq(12,n,13)
lines(x[v],y[v], col="green")

v=seq(13,n,13)
lines(x[v],y[v], col="green")