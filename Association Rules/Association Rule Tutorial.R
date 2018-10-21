# Assocation Rules Tutorial

#####################################################################

setwd("C:/Users/Derek/Documents/RPackages/Association Rules")

load("C:/Users/Derek/Documents/RPackages/Association Rules/titanic.raw.rdata")

str(titanic.raw)

######################################################################

# install.packages("arulesViz")

library(arules)
library(arulesViz)

# find association rules with default settings

rules <- apriori(titanic.raw)
inspect(rules)

#---------------------------------------------------------------------

# We then set rhs=c("Survived=No", "Survived=Yes") in appearance to make sure that only
# "Survived=No" and "Survived=Yes" will appear in the rhs of rules.

# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"),control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")

inspect(rules.sorted)

#---------------------------------------------------------------------

# In the above result, rule 2 provides no extra knowledge in addition to rule 1, since rules 1 tells us that all 2nd-class children survived. Generally speaking, when a rule (such as rule 2) is a super rule of another rule (such as rule 1) and the former 
# has the same or a lower lift, the former rule (rule 2) is considered to be redundant. Below we prune redundant rules. 

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#---------------------------------------------------------------------
# Visualize Association Rules. 

library(arulesViz)

# Scatter plot
plot(rules.pruned)

# Balloon Plot

plot(rules.pruned, method="graph", control=list(type="items"))

# Parallel Coordinate Plot

plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))