##########################################################################
# Export data frame to MS Access
##########################################################################

# Bring in Iris data as mydata

mydata <- iris
mydata2 <- iris

##########################################################################

# Export mydata into MS Access file and create table 

library(RODBC)

# connectionMS <- odbcConnectAccess("C:/Users/dkane/Documents/R Packages/Automation Example/ExportTest.mdb")

connectionMS <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/dkane/Documents/R Scripts/Automation Example/ExportTest.mdb")


sqlSave(channel=connectionMS, dat=mydata, tablename="Mydata", rownames=F,
        safer=F, fast=F)

odbcClose(connectionMS)

