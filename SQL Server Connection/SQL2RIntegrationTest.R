#####################################################################################
# SQL to R Integration Test
#####################################################################################

library(RODBC)


# Open a connection to the SQL Server DW

RelitixSQL_DW <- odbcConnect(dsn="Relitix_DW SQLServer", uid="Derek", pwd="password")

# This is how to import a table dbo.fact_censuspop into R as a Data Frame "censuspop"

censuspop <- sqlFetch(RelitixSQL_DW, "fact_censuspop")


#################################################################################
# Write the Dataframe back to SQL. 
#################################################################################

write.table(censuspop,"C:\\Users\\derek\\Documents\\Relitix\\RelitixDW\\censuspop.txt",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE,append=FALSE);

sqlQuery(RelitixSQL_DW,"BULK
         INSERT Relitix_DW.dbo.fact_censuspop
         FROM 'C:\\Users\\derek\\Documents\\Relitix\\RelitixDW\\censuspop.txt'
         WITH
         (
         FIELDTERMINATOR = ',',
         ROWTERMINATOR = '\\n'
         )");


# Close the connection to the SQL server

odbcClose(RelitixSQL_DW)