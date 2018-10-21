#####################################################################
# Leverage Points

library(car)

mydata <- Prestige
mydata


head(mydata)

r=lm(prestige~women+income+education, mydata)

summary(r)

# This shows the diagnostics including leverage points

plot(r)

cooks.distance(r)

# This is how to create the variable with Cooks Distance

mydata$Leverage <- cooks.distance(r)

# this creates a plot with Cooks Distance

windows()
with(mydata, plot(income,cooks.distance(r)))

# This allows for me to identify specific points on the graph by clicking on them.

identify(mydata$income, cooks.distance(r))
