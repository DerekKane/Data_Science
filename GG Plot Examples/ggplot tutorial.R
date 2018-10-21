#before we Begin we must install the following packages:
# digest,memoise,plyr,stringr,reshape2, munsell, colorspace, and scales (and more)


# load the package

library(ggplot2)

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/GG Plot Examples")
#-------------------------------------------------------

# We are going to use a nice trick here and read the text file from directly from a remote url. 

filepath<-"http://bit.ly/wBBTQO"

#read in the tab delimited text file using the url() function 

myData<-read.table(file=url(filepath),header=T,sep="\t")

# Now we can look at the structure of the dataframe we just imported.

str(myData)

# A simple histogram in ggplot2

qplot(data=myData,x=BM,main="Histogram of BodyMass")

# A basic scatterplot
qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe)

# Plot like the former default plot
qplot(data=myData,x=Hab,y=var1)

# Boxplot example
qplot(data=myData,x=Hab,y=var1,geom="boxplot")

#Doing the same thing to different data subsets - Faceting
qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe,facets = Hab~Tribe)

# This isn't actually very useful in this case because there are a lot of empty factor levels.  
# I probably just want to do it for each tribe like this. Note you still need the "~".

qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe,facets = ~Tribe)

# Trend lines - adding statistical transformation layers.
# WE are going to create an object called myGG which contains the qplot data and can be referenced later.

myGG<-qplot(data=myData,x=BM,y=var1,color=Tribe,facets=~Tribe)

# Then we can ADD A LAYER to this object in which we add a smoothing statistic 
# using "method='lm'" to add an ordinary least squares trendline.

myGG<- myGG + stat_smooth(method="lm")

# To display the object "myGG", just type myGG and hit enter:


# saving graphs to file is a snap with ggsave()
ggsave("LIBMFacetsWithTrend.jpg")
