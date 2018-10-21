####################################################################
# Fibonacci Sequence
####################################################################

len = 10
fibvals = numeric(len)
fibvals[1] = 1
fibvals[2] = 1
for (i in 3:len) { 
  fibvals[i] = fibvals[i-1] + fibvals[i-2]
} 

# The 3 in 3:len just means do this loop starting with the third item in the vector. So the 
# first time we execute the loop, i = 3, and we sum fibvals[1] and fibvals[2].


# This is the fibonnaci sequence
fibvals

plot(fibvals)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternative Function

fibo<-function(x) {
  len = x
  x = numeric(len)
  x[1] = 1
  x[2] = 1
  for (i in 3:len) {
    x[i] = x[i-1] + x[i-2]
  }
  return(max(x))
}

fibo(100)

####################################################################
# Golden Spiral
####################################################################

# Golden spiral formula

r = e^(a*theta)

# alternative formula

a = (2/pi) ln theta

theta = (1+sqrt(5))/2

pi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot


t = seq(0:0.01:(20*pi))

x = ((exp(0.1*t))*(cos(t)))
y = ((exp(0.1*t))*(sin(t)))

plot(x,y)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# better plot

t = e^