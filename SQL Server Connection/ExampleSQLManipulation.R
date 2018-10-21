# Load the SQL package
library(sqldf)

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/kane.de/Documents/RPackages/SQL Query Manipulation in R")

# Read in the First CSV File

read.csv("ExampleSQL1.csv") -> ExampleSQL1
attach(ExampleSQL1)

# This is a check to see the first 10 lines of the data.

head(ExampleSQL1, n=10)

#-------------------------------------------------------

# Here is a basic select query to select State = Alabama

sqldf("SELECT State 
      FROM ExampleSQL1 
      WHERE State='Alabama'")

# Expanded to show select State = Alabama and the other columns

sqldf("SELECT State, Region, Sales 
      FROM ExampleSQL1 
      WHERE State='Alabama'")

# Alternatively, we can use the following:

sqldf("SELECT * 
      FROM ExampleSQL1 
      WHERE State='Alabama'")

#----------------------------------------------------

# Expanded to Sum the Sales

sqldf("SELECT State,Sum(Sales) as Total 
      FROM ExampleSQL1 
      GROUP BY State 
      HAVING State='Alabama'")

# Here is the wildcard character in place of * in the WHERE Clause

sqldf("SELECT State 
      FROM ExampleSQL1 
      WHERE State like 'Ala%'")

# This is a single Wildcard character _

sqldf("SELECT State 
      FROM ExampleSQL1 
      WHERE State like 'Alabam_'")


