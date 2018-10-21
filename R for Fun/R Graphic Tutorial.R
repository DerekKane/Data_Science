# ****Introduction****

# Data analysis is like an interview.  In any interview, the interviewer hopes to use a series of 
# questions in order to discover a story.  The questions the interviewer asks, of course, are 
# subjectively chosen.  As such, the story that one interviewer gets out of an interviewee might 
# be fairly different from the story that another interviewer gets out of the same person.  In the 
# same way, the commands (and thus the analysis) below are not the only way of analyzing the data.  
# When you understand what the commands are doing, you might decide to take a different approach 
# to analyzing the data.  Please do so, and be sure to share what you find!

# ****Dataset Background****

# The datasets that we will be working with all relate to council areas in scotland (roughly equivalent 
# to provinces).  The one which I have labeled 'main' has numbers representing the number of drug 
# related deaths by council area, with most of its columns containing counts that relate to specific 
# drugs. It also contains geographical coordinates of the council areas, in latitude and longitude.  
# The one which I have labeled 'pop' contains population numbers.
# The rest of the datasets contain numbers relating to problems with crime, education, employment, 
# health, and income.  The datasets contain proportions in them, such that values closer to 1 indicate 
# that the council area is more troubled, while values closer to 0 indicate that the council area is 
# less troubled in that particular way.

# P.S. If you haven't figured out already, any time a hash symbol begins a line, it means that I'm 
# writing a comment to you, rather than writing out code.

setwd("C:/Users/Derek/Documents/RPackages/R for Fun")

# Loading all the datasets

main = read.csv("2012-drugs-related-cx.csv")
pop = read.csv("scotland pop by ca.csv")
crime = read.csv("most_deprived_datazones_by_council_(crime)_2012.csv")
edu = read.csv("most_deprived_datazones_by_council_(education)_2012.csv")
emp = read.csv("most_deprived_datazones_by_council_(employment)_2012.csv")
health = read.csv("most_deprived_datazones_by_council_(health)_2012.csv")
income = read.csv("most_deprived_datazones_by_council_(income)_2012.csv")

# Indexing the data

names(main)
main$Council.area
main$Council.area[1:10]
main[1:10,1]

# Merging other relevant data with the main dataset

main = merge(main, pop[,c(2,3)], by.x="Council.area", by.y="Council.area", all.x=TRUE)
main = merge(main, crime[,c(1,4)], by.x="Council.area", by.y="label", all.x=TRUE)
main = merge(main, edu[,c(1,4)], by.x="Council.area", by.y="label", all.x=TRUE)
main = merge(main, emp[,c(1,4)], by.x="Council.area", by.y="label", all.x=TRUE)
main = merge(main, health[,c(1,4)], by.x="Council.area", by.y="label", all.x=TRUE)
main = merge(main, income[,c(1,4)], by.x="Council.area", by.y="label", all.x=TRUE)

# Weighting the number of drug related deaths by the population of each council area

main$All.drug.related.deaths_perTenK = (main$All.drug.related.deaths / (main$Population/10000))

# A histogram of the number of drug related deaths per 10,000 people

hist(main$All.drug.related.deaths_perTenK, col="royal blue")

# A Simple scatterplot

plot(All.drug.related.deaths_perTenK ~ prop_income, data=main)

# A Scatterplot matrix

pairs(~All.drug.related.deaths_perTenK + Latitude + Longitude + prop_crime + prop_education + prop_employment + prop_income + prop_health, data=main)

# Summary stats of all the variables in the dataset

summary(main)

# Simple summary stats of one variable at a time

mean(main$All.drug.related.deaths)
median(main$All.drug.related.deaths_perTenK)

# Here we do a median split of the longitudes of the council areas, resulting in an 'east' and 'west' group

main$LongSplit = cut(main$Longitude, breaks=quantile(main$Longitude, c(0,.5,1)), include.lowest=TRUE, right=FALSE, ordered_result=TRUE, labels=c("East", "West"))

# Let's examine the number of records that result in each group:

table(main$LongSplit)

# Now we do a median split of the latitudes of the council areas, resulting in a 'north' and 'south' group

main$LatSplit = cut(main$Latitude, breaks=quantile(main$Latitude, c(0,.5,1)), include.lowest=TRUE, right=FALSE, ordered_result=TRUE, labels=c("South", "North"))

# Now let's summarise some of the statistics according to our north-south east-west dimensions:

# install.packages("dplyr")
library(dplyr)

data_source = collect(main)
grouping_factors = group_by(source_df, LongSplit, LatSplit)
deaths_by_area = summarise(grouping_factors, median.deathsptk = median(All.drug.related.deaths_perTenK),
                           median.crime = median(prop_crime), median.emp = median(prop_employment),
                           median.edu = median(prop_education), num.council.areas = length(All.drug.related.deaths_perTenK))

# Examine the summary table just created

deaths_by_area

# Now we'll make some fun plots of the summary table

library(ggplot2)

qplot(LongSplit, median.deathsptk, data=deaths_by_area, facets=~LatSplit, geom="bar", stat="identity", fill="dark red",main="Median Deaths/10,000 by Area in Scotland") + theme(legend.position="none")

qplot(LongSplit, median.crime, data=deaths_by_area, facets=~LatSplit, geom="bar", stat="identity", fill="dark red",main="Median Crime Score by Area in Scotland") + theme(legend.position="none")

qplot(LongSplit, median.emp, data=deaths_by_area, facets=~LatSplit, geom="bar", stat="identity", fill="dark red",main="Median Unemployment Score by Area in Scotland") + theme(legend.position="none")

qplot(LongSplit, median.edu, data=deaths_by_area, facets=~LatSplit, geom="bar", stat="identity", fill="dark red",main="Median Education Problems Score by Area in Scotland") + theme(legend.position="none")

# ****Some Online R Resources****

# http://www.r-bloggers.com
# This is a website that aggregates the posts of people who blog about R (myself included, from time to time).  The site has been up for several years now, and boasts a total blog count of over 5,000 R Bloggers!  If something about R has been said anywhere, it's been said on this site!

# http://r.789695.n4.nabble.com/R-help-f789696.html
# The R-help listserv contains a lot of emails people have sent asking just about everything about R!  Look through and see if your question is answered there.

# http://www.introductoryr.co.uk/R_Resources_for_Beginners.html
# This page contains a lot of online books about R that will more than help get you started!

# http://stackoverflow.com/questions/tagged/r
# Stackoverflow is a great website to go to when you want to know which answers people like the best to pressing questions about R, amongst other things (the best get 'up'voted by more people, the worst.....well...)
