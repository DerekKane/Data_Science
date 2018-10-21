# http://mathforum.org/library/drmath/view/51760.html
# http://blogs.mathworks.com/cleve/2013/05/27/golden-spiral/

# https://timwolverson.wordpress.com/2014/02/08/plot-a-fibonacci-spiral-in-excel/

r = a*e^(k*@)

# r = radius (distance from origin)
# @ = theta  (angle of the graph to open up)
# a = constant (user defined)
# k = constant (user defined)


a = 0.1
phi = (sqrt(5) + 1)/2
theta = 360/(golden.ratio^2)


r = a*exp((2/pi)*log(phi)*theta)

r = a*phi^((2/pi)*theta)

  
x=rep(0,num_points)
y=rep(0,num_points)


num_points=15
for (n in 1:num_points) {
  r= a*exp((2/pi)*log(phi)*theta)
  theta=fibonacci.angle*(n)
  x[n]=r*cos(theta)
  y[n]=r*sin(theta)
}

plot(x,y,type="l",axes=FALSE,ann=FALSE,pch=19,cex=1)

  
  
  


##############################################################

phi = (sqrt(5) + 1)/2
theta = 30
# theta = 360/(phi^2)
a = 0.001
b = log(theta)/90

theta.seq <- seq(0:theta)
x=rep(0,31)
y=rep(0,31)



for (n in theta.seq) {
  rad= n*pi/180
  x[n]=a*cos(rad)*exp(b*n)
  y[n]=a*sin(rad)*exp(b*n)
}

plot(x,y,type="l",axes=FALSE,ann=FALSE,pch=19,cex=1)
  






fibonacci.angle=360/(golden.ratio^2)


x=rep(0,num_points)
y=rep(0,num_points)


num_points=15
for (n in 1:num_points) {
  r=
  theta=fibonacci.angle*(n)
  x[n]=r*cos(theta)
  y[n]=r*sin(theta)
}

plot(x,y,type="l",axes=FALSE,ann=FALSE,pch=19,cex=1)
