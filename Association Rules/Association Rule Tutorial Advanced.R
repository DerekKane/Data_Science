# Assocaition Rule Example - Advanced

######################################################################
# http://cran.at.r-project.org/web/packages/arules/vignettes/arules.pdf

setwd("C:/Users/dkane/Documents/R Packages/Association Rules")


library("arules")

data("Epub")
Epub

summary(Epub)

# this allows for us to view the timestamp data and also the transaction ID

year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)

# This will show the transactions for 2003

Epub2003 <- Epub[year == "2003"]
length(Epub2003)

image(Epub2003)

######################################################################

# this will put the list into a vertical alignment

EpubTidLists <- as(Epub, "tidLists")
EpubTidLists

as(EpubTidLists[1:3], "list")

######################################################################
######################################################################
# Here is a full example using the Adult UCI dataset.

data("AdultUCI")

# show the top 2 rows using matrix notation

AdultUCI[1:2,]

# this is how we will remove 2 unecessary columns from the dataset

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

# This section will allow for us to build subcategories based upon the 
# existing dataset for a better analysis. IMPORTANT TO UNDERSTAND 

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)),labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]],c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],c(-Inf,0,median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),labels = c("none", "low", "high"))

######################################################################

# We can flip the data set across the vertical with our new categories 
# to prepare for analysis.

Adult <- as(AdultUCI, "transactions")
Adult

summary(Adult)


# To see which items are important in the data set we can use the itemFrequencyPlot(). To
# reduce the number of items, we only plot the item frequency for items with a support greater
# than 10%.

itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)

# Next, we call the function apriori() to find all rules (the default association type for
# apriori()) with a minimum support of 1% and a confidence of 0.6.

rules <- apriori(Adult,parameter = list(support = 0.01, confidence = 0.6))

summary(rules)

# Lets review rules for individuals with low or high income.
# This will also require a lift greater than 1.2

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)

# We now have a set with rules for persons with a small income and a set for persons with
# a large income. For comparison, we inspect for both sets the three rules with the highest
# confidence (using sort()).

inspect(head(sort(rulesIncomeSmall, by = "confidence"), n = 3))
inspect(head(sort(rulesIncomeLarge, by = "confidence"), n = 3))

# Write the rules to a CSV file

write(rulesIncomeSmall, file = "data.csv", sep = ",", col.names = NA)


