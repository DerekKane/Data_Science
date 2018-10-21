###################################################################
# CLV using RFM analysis
###################################################################

# http://www.dataapple.net/?p=133


# Run the CLV Functions.R file first (remember to use the master file)

model=glm(Buy~Recency+Frequency,family=quasibinomial(link='logit'),data=train)

# Given a customer's status of Recency and Frequency, we can predict the probability of repurchasing with the above model.

pred<-predict(model,data.frame(Recency=c(0),Frequency=c(1)),type='response')

pred

# As shown in the above, a customer, say Tom, who became a new customer 
# in the most recent period (So Recency = 0, and Frequency=1), has 
# a 26% probability to purchase again in the next period (Period 1).

# calculating CLV

pred<-predict(model,data.frame(Recency=c(0),Frequency=c(2)),type='response')

pred


