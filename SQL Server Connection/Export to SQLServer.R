######################################################################
# Integration between SQL Server and R - Schedule Job
######################################################################

# Connect to SQL Server

library(RODBC)

# Open a connection to the SQL Server Finance Data Mart

SQLServerDS <- odbcConnect(dsn="SQLServer - DataScience", uid="Derek", pwd="password")

# This is how to import a table dbo.DataScience into R as a Data Frame "mydata"

mydata <- iris

#################################################################################
# Write the Dataframe back to SQL. 
#################################################################################

write.table(mydata,"C:\\Users\\derek\\Documents\\R Scripts\\SQL Server Connection\\iris.txt",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE,append=FALSE);

sqlQuery(SQLServerDS,"BULK
         INSERT DataScience.dbo.Iris
         FROM 'C:\\Users\\derek\\Documents\\R Scripts\\SQL Server Connection\\iris.txt'
         WITH
         (
         FIELDTERMINATOR = ',',
         ROWTERMINATOR = '\\n'
         )");


# Close the connection to the SQL server

odbcClose(SQLServerDS)

