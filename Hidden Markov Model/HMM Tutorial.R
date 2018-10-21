#################################################################################
# Hidden Markov Model Example
#################################################################################


# http://www.slideshare.net/ChiuYW/hidden-markov-model-stock-prediction
# https://github.com/david78k/stock/blob/master/rhmm/TWII.R
# http://stackoverflow.com/questions/11644522/getting-the-next-observation-from-a-hmm-gaussian-mixture-distribution


# install.packages("quantmod")


library(quantmod)
getSymbols("^TWII", src = "yahoo", from = "1900-01-01", to = "2015-12-31")

chartSeries(TWII, theme="black")


TWII_Subset <- window(TWII, start=as.Date("2013-01-01"), end = as.Date("2015-12-31"))
TWII_Train <- cbind(TWII_Subset$TWII.Close - TWII_Subset$TWII.Open)

# TWII_Train <- cbind(TWII_Subset$TWII.Close - TWII_Subset$TWII.Open,
#                  TWII_Subset$TWII.Volume)




#################################################################################
# Baum-Welch Algorithm
#################################################################################

library(RHmm)

# Package Located at https://r-forge.r-project.org/R/?group_id=85

# hm_model <- HMMFit(obs=TWII_Train, nStates=5)

hm_model <- HMMFit(obs=TWII_Train, dis="MIXTURE", nStates=5, nMixt=4,control=list(iter=2000))

print(hm_model)

#################################################################################
# Viterbi Algorithm
#################################################################################

VitPath <- viterbi(hm_model, TWII_Train)

#################################################################################
# Predict the known values of the HMM model
#################################################################################

TWII_Predict <- cbind(TWII_Subset$TWII.Close, VitPath$states)

# Scatterplot

chartSeries(TWII_Predict[,1])
addTA(TWII_Predict[TWII_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
addTA(TWII_Predict[TWII_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
addTA(TWII_Predict[TWII_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
addTA(TWII_Predict[TWII_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
addTA(TWII_Predict[TWII_Predict[,2]==5,1],on=1,type="p",col=10,pch=21) 

# HMMPlotSerie(TWII_Train, VitPath)

#################################################################################
# Here is the calculation of the next observation
#################################################################################

change <- sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean),nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=4,ncol=5)), m=4,n=5))


# select the most recent date from xts object


mydata <- last(TWII_Subset)



predict <- mydata$TWII.Open + change

predict$TWII.Open

# This will give a specific date
# mydata <- TWII_Subset["2014-04-01"]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Review the post on http://stackoverflow.com/questions/11644522/getting-the-next-observation-from-a-hmm-gaussian-mixture-distribution
            
# Next predicted value uses last hidden state last(v$states) 
# to get probability weights from the transition matrix a$HMM$transMat[last(v$states),] 
# for each state the distribution means a$HMM$distribution$mean are 
# weighted by proportions a$HMM$distribution$proportion, then its all multiplied 
# together and summed. So in the above case it would be as follows:
              
          
            
# sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean),
# nrow=4,ncol=5))) * (matrix(unlist(hm_model$HMM$distribution$proportion),nrow=4,ncol=5)), m=4,n=5)


# write.csv(TWII, file='C:/Users/Derek/Documents/mydata.csv', row.names=F)