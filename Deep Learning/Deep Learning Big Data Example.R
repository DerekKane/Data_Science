#################################################################
# Deep Learning - Big Data Tutorial
#################################################################

# http://blog.dominodatalab.com/using-r-h2o-and-domino-for-a-kaggle-competition/

# This tutorial will show how to incorporate R for the analytics,
# the H2O data analytics platform for big data algorithms,
# and Domino to allow for multiple data processing. 

# Login to Domino - https://app.dominodatalab.com/tour

# In Windows, open the command prompt
# type - domino login

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Domino interface to R 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages("domino")

library(domino)
domino.login("deeker04", "rooster1")  

# domino.login("your_username", "your_password") 

# Follow the setup on the quick start guide to get a project folder loaded.
# http://help.dominodatalab.com/quickStart

