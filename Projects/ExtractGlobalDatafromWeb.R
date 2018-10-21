# http://www.r-bloggers.com/global-indicator-analyses-with-r/

# This is some code to demonstrate how to pull data from the World Bank Economic Indicators
# Data API. When used in combination with information on the World Bank Data Portal this code
# provides easy access to thousand of data points.

# install.packages("WDI")
# install.packages("countrycode")


library(WDI)
library(ggplot2)
library(countrycode) 

# Use the WDIsearch function to get a list of fertility rate indicators

indicatorMetaData <- WDIsearch("Fertility rate", field="name", short=FALSE) 

# Define a list of countries for which to pull data

countries <- c("United States", "Britain", "Sweden", "Germany") 

# Convert the country names to iso2c format used in the World Bank data

iso2cNames <- countrycode(countries, "country.name", "iso2c") 

# Pull data for each countries for the first two fertility rate indicators, for the years 2001 to 2011

wdiData <- WDI(iso2cNames, indicatorMetaData[1:2,1], start=2001, end=2011) 

# Pull out indicator names
indicatorNames <- indicatorMetaData[1:2, 1] 

# Create trend charts for the first two indicators

for (indicatorName in indicatorNames) {pl<- ggplot(wdiData, aes(x=year, y=wdiData[,indicatorName], group=country, color=country))+geom_line(size=1)+scale_x_continuous(name="Year", breaks=c(unique(wdiData[,"year"])))+scale_y_continuous(name=indicatorName)+scale_linetype_discrete(name="Country")+theme(legend.title=element_blank())+ggtitle(paste(indicatorMetaData[indicatorMetaData[,1]==indicatorName, "name"], "\n"))  
                                       ggsave(paste(indicatorName, ".jpg", sep=""), pl)}