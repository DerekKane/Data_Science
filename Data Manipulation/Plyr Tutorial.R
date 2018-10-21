##############################################################
# Getting Started with plyr tutorial
##############################################################

# http://www.stat.ubc.ca/~jenny/STAT545A/block04_dataAggregation.html

# Get the dataset

## data import from URL
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)

# Basic sanity check that the import has gone well

str(gDat)


# Data Aggregation

# Optional: Save a small snippet of information. 

snippet <- subset(gDat, country == "Canada")

#########################################################################
# Load the plyr package

# install.packages("plyr", dependencies = TRUE)

library(plyr)

# Let's say we want to get the maximum life expectancy for each continent.

maxLeByCont <- ddply(gDat, ~ continent, summarize, maxLifeExp = max(lifeExp))

# Lets study the return value

str(maxLeByCont)

levels(maxLeByCont$continent)


# So we got a data.frame back, with one observation per continent, and two variables: the maximum life expectancies and the continent, as a factor, 
# with the same levels in the same order, as for the input data.frame gDat. 

# summarize() or its synonym summarise() is a function provided by plyr that creates a new data.frame from an old one.

# Lets compute the minimum GDP now.

minGDPByCont <- ddply(gDat, ~ continent, summarize, minGDPExp = min(gdpPercap))

# Here's how I would count the number of countries in this dataset for each continent.

ddply(gDat, ~continent, summarize, nUniqCountries = length(unique(country)))

# Here is another way to do the same thing that doesn't use summarize() at all:

ddply(gDat, ~ continent,
      function(x) return(c(nUniqCountries = length(unique(x$country)))))

# You don't have to compute just one thing for each sub-data.frame, nor are you limited to computing on just one variable. 

ddply(gDat, ~ continent, summarize,
      minLifeExp = min(lifeExp), maxLifeExp = max(lifeExp),
      medGdpPercap = median(gdpPercap))

############################################################################
# Plyr with linear regression

# Now I want to do something more complicated. I want to fit a linear regression 
# for each country, modelling life expectancy as a 
# function of the year and then retain the estimated intercepts and slopes.

jCountry <- "France"  # pick, but do not hard wire, an example

jDat <- subset(gDat, country == jCountry)  # temporary measure!

jFit <- lm(lifeExp ~ year, jDat)

summary(jFit)

# Sanity check the model.

yearMin <- min(gDat$year)

#  I think it makes more sense for the intercept to correspond 
# to life expectancy in 1952, the earliest date in our dataset.

jFit <- lm(lifeExp ~ I(year - yearMin), jDat)

summary(jFit)

# An intercept around 68 years makes much more common sense and is also supported by our plot. 
# What is this jFit object and how can I get stuff out of it?

class(jFit)

mode(jFit)

## str(jFit) # too ugly to print here but you should look
names(jFit)

jFit$coefficients

coef(jFit)

# We have achieved our goal for this specific country -- we've gotten its intercept 
# and slope. Now we need to package that as a function (we will talk 
# about functions properly later, but this should be fairly self-explanatory).

jFun <- function(x) coef(lm(lifeExp ~ I(year - yearMin), x))
jFun(jDat)  # trying out our new function ... yes still get same numbers

# I hate the names of these return values. Good names pay off downstream, so I will enhance my function.

jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}

jFun(jDat)  # trying out our improved function ... yes still get same numbers

# It's always a good idea to try out a function on a few small examples.

jFun(subset(gDat, country == "Canada"))
jFun(subset(gDat, country == "Uruguay"))
jFun(subset(gDat, country == "India"))

# It seems like we are ready to scale up by placing this function inside a ddply() call.

jCoefs <- ddply(gDat, ~country, jFun)
str(jCoefs)

tail(jCoefs)

# We did it! By the time we've packaged the computation in a function, 
# the call itself is deceptively simple. 
# To review, here's the script I would save from our work in this section:

## realistically, you would read the data from a local file

gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)

## str(gDat) here when working interactively
yearMin <- min(gDat$year)
jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}

## jFun(subset(gDat, country == 'India')) to see what it does
jCoefs <- ddply(gDat, ~country, jFun)

# Finally, let's present this information attractively in a table. 

# install.packages("xtable", dependencies = TRUE)
library(xtable)

set.seed(916)

mydata <- jCoefs[sample(nrow(jCoefs), size = 15), ]
mydata <- xtable(mydata)
print(mydata, type = "html", include.rownames = FALSE)

# Two easy improvments to make this table more useful are
#     include the continent information
#     sort it rationally


# This is the normal function.
jCoefs <- ddply(gDat, ~country, jFun)

# Here is the ddply function with 2 grouped levels, country and continent.

jCoefs <- ddply(gDat, ~country + continent, jFun)
str(jCoefs)

tail(jCoefs)

# Now, prior to making the HTML table, we will sort the data.frame, so it starts with the country with the shortest 
# life expectancy in 1952, and goes to the largest.

set.seed(916)
mydata <- jCoefs[sample(nrow(jCoefs), size = 15), ]
mydata <- arrange(mydata, intercept)
## mydata <- mydata[order(mydata$intercept), ] # an uglier non-plyr way

mydata <- xtable(mydata)
print(mydata, type = "html", include.rownames = FALSE)
