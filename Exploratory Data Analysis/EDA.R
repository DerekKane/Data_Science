Exploratory Data Analysis

#################################################################
# Power Function
#################################################################

x=seq(1,5,0.5)
y=3*x^2
plot(x,y)

x2 <- log(x) 
y2 <- log(y)

df <- rbind(x2,y2)

#################################################################
# Log Transformation

plot(x2,y2)

res <- lm(log(y) ~ log(x))
res

log(3)


#################################################################

primates=read.table(file=file.choose(),header=TRUE)
primates

plot(RMR~Weight,data=primates)



x=seq(1,5,0.5)
y=3*2^x
plot(x,y)




library("datasets")

mydata <- cars
