library(RODBC)

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/SQL Server Connection")
#-------------------------------------------------------

# Open a connection to the SQL Server Finance Data Mart

SQLServerFDM <- odbcConnect(dsn="finance", uid="Derek", pwd="password")

# This is how to import a table dbo.Business_Unit into R as a Data Frame "BUAssignment"

BUAssignment <- sqlFetch(SQLServerFDM, "Business_Unit")

# We always have to attach the data frame in R in order to make use of it later.
#-------------------------------------------------------

attach(BUAssignment)

# This is a check to see the first 10 lines of the data.

head(BUAssignment, n=10)

#-------------------------------------------------------

# Here is the code to export the table as a csv
# write.csv(BUAssignment, file = "BUAssignment.csv")

#-------------------------------------------------------

# This is sample code to append into th SQL Table
# sqlUpdate(channel=SQLServerFDM, dat=BUAssignment, tablename="Business_Unit_R", index="PSC_Code")


# Close the connection to the SQL server

odbcClose(SQLServerFDM)

#-------------------------------------------------------

# Here is a section that will use the rsqlserver package to append into
# a SQL table named Business_Unit_R.

library(rsqlserver)
library(rClr)

url = "Data Source=KANESURFACE;Initial Catalog=Finance_DataMart;Integrated Security=True;"
conn <- dbConnect('KANESURFACE',url=url)
dbBulkCopy(conn,name='Business_Unit_R',value=BUAssignment,overwrite=TRUE)
dbDisconnect(conn)