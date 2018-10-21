###################################################################
# Parameter Selection Tutorial
###################################################################

# Set working Directory 

setwd("C:/Users/dkane/Documents/R Scripts/Parameter Selection/") 

# Load the data

mydata <- read.csv("BankDemo.csv")


# Remove Unneeded Columns

mydata$CustomerID <- NULL
mydata$Customer.Name <- NULL

# Run basic Logistic Regression Model

m_logistic <- glm(Outcome~., family=binomial("logit"), data = mydata) 

# Run Stepwise Regression to determine variables

step_glm <- step(m_logistic) 


# This code selects the names of the labels for the stepwise results

terms <- attr(step_glm$terms,  "term.labels") 
terms2 <- paste(terms,sep = "+")

fmla <- as.formula(paste("Outcome ~ ", paste(terms2, collapse= "+")))


# Insert results as new parameters in model

m_logistic2 <- glm(fmla, family=binomial("logit"), data = mydata) 

summary(m_logistic2)

# http://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
# http://stackoverflow.com/questions/17024685/how-to-use-a-character-string-in-formula
