#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connect to MySQL and Export results into new table
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 

# Set working Directory 

setwd("C:/Users/Derek/Documents/RPackages/Deep Learning/")

# setwd("C:/Business Intelligence/Facebook/Archive/")


# Open File as mydata

mydata <- read.csv("Page.csv", header=TRUE)
# mydata <- read.delim("CDNOW_master.txt", header=FALSE, sep= " ")


library(RMySQL)
 
mydb = dbConnect(MySQL(), user='root', password='root!', dbname='datascience', host='localhost')
 
# List the tables and fields
  
dbListTables(mydb)

# write it back
dbWriteTable(mydb,"facebook_page",mydata,overwrite=T)

# watch out with the overwrite argument it does what it says :)

# library("UsingR") 
# mydata <- Melanoma