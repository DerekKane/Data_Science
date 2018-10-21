# Nuclear vs Green Energy Tutorial

setwd("C:/Users/Derek/Documents/RPackages/R for Fun")

ltep = read.csv("ltep-survey-results-all.csv")

# install.packages("likert")
# install.packages("ggthemes")
# install.packages("plyr")
# install.packages("scales")


library(likert)
library(ggthemes)

# Here I flip the scoring
ltep[,13:19] = sapply(ltep[,13:19], function (x) 8 - x)
deal.w.esources = likert(ltep[,13:19])
summary(deal.w.esources)
plot(deal.w.esources, text.size=6, text.color="black") + theme(axis.text.x=element_text(colour="black", face="bold", size=14), axis.text.y=element_text(colour="black", face="bold", size=14), axis.title.x=element_text(colour="black", face="bold", size=14), plot.title=element_text(size=18, face="bold")) + ggtitle("What guidelines should Ontario use\n for its future mix of energy sources?")

library(plyr)

# following is a lot of boring code where I 
# generate numerous data frames that contain percent of 
# respondents who agree with the statement by their rated
# importance of Nuclear vs. Green Energy

# Here's the nuclear section
self.sustaining.region.by.nuke = ddply(ltep, .(Nuclear.power.is.our.best.option.), function (x) mean(x$A.region.should.be.responsible.for.generating.at.least.some.of.its.own.power. == "Agree", na.rm=TRUE))
self.sustaining.region.by.nuke = self.sustaining.region.by.nuke[1:7,]

region.buy.power.by.nuke = ddply(ltep, .(Nuclear.power.is.our.best.option.), function (x) mean(x$Regions.should.have.the.option.to.buy.all.of.their.power.from.sources.in.another.region..if.the.power.is.available. == "Agree", na.rm=TRUE))
region.buy.power.by.nuke = region.buy.power.by.nuke[1:7,]

region.resp.for.growing.demand.by.nuke = ddply(ltep, .(Nuclear.power.is.our.best.option.), function (x) mean(x$If.a.region.s.power.demand.is.growing..it.should.be.responsible.for.building.the.generation.or.transmission.to.supply.that.it.needs. == "Agree", na.rm=TRUE))
region.resp.for.growing.demand.by.nuke = region.resp.for.growing.demand.by.nuke[1:7,]

regions.make.cons.first.priority.by.nuke = ddply(ltep, .(Nuclear.power.is.our.best.option.), function (x) mean(x$Regions.should.make.conservation.their.first.priority.to.reduce.the.need.for.new.supply. == "Agree", na.rm=TRUE))
regions.make.cons.first.priority.by.nuke = regions.make.cons.first.priority.by.nuke[1:7,]

# Here's the green energy section
self.sustaining.region.by.green = ddply(ltep, .(Green.energy..e.g...wind..solar..is.our.best.option.), function (x) mean(x$A.region.should.be.responsible.for.generating.at.least.some.of.its.own.power. == "Agree", na.rm=TRUE))
self.sustaining.region.by.green = self.sustaining.region.by.green[1:7,]

region.buy.power.by.green = ddply(ltep, .(Green.energy..e.g...wind..solar..is.our.best.option.), function (x) mean(x$Regions.should.have.the.option.to.buy.all.of.their.power.from.sources.in.another.region..if.the.power.is.available. == "Agree", na.rm=TRUE))
region.buy.power.by.green= region.buy.power.by.green[1:7,]

region.resp.for.growing.demand.by.green = ddply(ltep, .(Green.energy..e.g...wind..solar..is.our.best.option.), function (x) mean(x$If.a.region.s.power.demand.is.growing..it.should.be.responsible.for.building.the.generation.or.transmission.to.supply.that.it.needs. == "Agree", na.rm=TRUE))
region.resp.for.growing.demand.by.green = region.resp.for.growing.demand.by.green[1:7,]

regions.make.cons.first.priority.by.green = ddply(ltep, .(Green.energy..e.g...wind..solar..is.our.best.option.), function (x) mean(x$Regions.should.make.conservation.their.first.priority.to.reduce.the.need.for.new.supply. == "Agree", na.rm=TRUE))
regions.make.cons.first.priority.by.green = regions.make.cons.first.priority.by.green[1:7,]

# in this section I alternate between getting each data frame ready for plotting
# and obviously plotting the data frame.  I liked the yellow and green that I used in the plot
# to represent Nuclear vs. Green energy :)

library(ggplot2)
library(scales)
self.sustaining.region = rbind(data.frame(Importance=seq(1,7),Pct.Agree=self.sustaining.region.by.nuke$V1, Energy=rep("Nuclear Energy",7)), data.frame(Importance=seq(1,7),Pct.Agree=self.sustaining.region.by.green$V1, Energy=rep("Green Energy", 7)))

ggplot(self.sustaining.region, aes(x=as.factor(Importance), y=Pct.Agree, fill=Energy)) + geom_bar(stat="identity",width=.5) + scale_y_continuous(labels=percent, name="Percent Who Agree") + scale_x_discrete(name="Rated Level of Importance (1-7 = Low to High)") +  facet_grid(~Energy) + scale_fill_manual(values=c("khaki1","forest green")) + ggtitle("A Region Should be Responsible for Generating at Least Some of Its Own Power\n(Agreement by Rated Importance of Nuclear vs. Green Energy)") + theme(axis.text.x=element_text(colour="black", face="bold", size=14), axis.text.y=element_text(colour="black", face="bold", size=14), axis.title.x=element_text(colour="black", face="bold", size=14), axis.title.y=element_text(colour="black", face="bold", size=14), plot.title=element_text(size=18, face="bold"), strip.text=element_text(size=14), legend.title=element_text(size=14), legend.text=element_text(size=14))

region.buy.power = rbind(data.frame(Importance=seq(1,7),Pct.Agree=region.buy.power.by.nuke$V1, Energy=rep("Nuclear Energy",7)), data.frame(Importance=seq(1,7),Pct.Agree=region.buy.power.by.green$V1, Energy=rep("Green Energy", 7)))

ggplot(region.buy.power, aes(x=as.factor(Importance), y=Pct.Agree, fill=Energy)) + geom_bar(stat="identity",width=.5) + scale_y_continuous(labels=percent, name="Percent Who Agree") + scale_x_discrete(name="Rated Level of Importance (1-7 = Low to High)") +  facet_grid(~Energy) + scale_fill_manual(values=c("khaki1","forest green")) + ggtitle("Regions Should Have the Option to Buy All of their Power\nfrom Sources in Another Region if Available\n(Agreement by Rated Importance of Nuclear vs. Green Energy)") + theme(axis.text.x=element_text(colour="black", face="bold", size=14), axis.text.y=element_text(colour="black", face="bold", size=14), axis.title.x=element_text(colour="black", face="bold", size=14), axis.title.y=element_text(colour="black", face="bold", size=14), plot.title=element_text(size=18, face="bold"), strip.text=element_text(size=14), legend.title=element_text(size=14), legend.text=element_text(size=14))

region.resp.for.growing.demand = rbind(data.frame(Importance=seq(1,7),Pct.Agree=region.resp.for.growing.demand.by.nuke$V1, Energy=rep("Nuclear Energy",7)), data.frame(Importance=seq(1,7),Pct.Agree=region.resp.for.growing.demand.by.green$V1, Energy=rep("Green Energy", 7)))

ggplot(region.resp.for.growing.demand, aes(x=as.factor(Importance), y=Pct.Agree, fill=Energy)) + geom_bar(stat="identity",width=.5) + scale_y_continuous(labels=percent, name="Percent Who Agree") + scale_x_discrete(name="Rated Level of Importance (1-7 = Low to High)") +  facet_grid(~Energy) + scale_fill_manual(values=c("khaki1","forest green")) + ggtitle("If a Region's Power Demand is Growing\nIt Should be Responsible for Building the Transmission/Supply it Needs\n(Agreement by Rated Importance of Nuclear vs. Green Energy)") + theme(axis.text.x=element_text(colour="black", face="bold", size=14), axis.text.y=element_text(colour="black", face="bold", size=14), axis.title.x=element_text(colour="black", face="bold", size=14), axis.title.y=element_text(colour="black", face="bold", size=14), plot.title=element_text(size=18, face="bold"), strip.text=element_text(size=14), legend.title=element_text(size=14), legend.text=element_text(size=14))

regions.make.cons.first.priority = rbind(data.frame(Importance=seq(1,7),Pct.Agree=regions.make.cons.first.priority.by.nuke$V1, Energy=rep("Nuclear Energy",7)), data.frame(Importance=seq(1,7),Pct.Agree=regions.make.cons.first.priority.by.green$V1, Energy=rep("Green Energy", 7)))

ggplot(regions.make.cons.first.priority, aes(x=as.factor(Importance), y=Pct.Agree, fill=Energy)) + geom_bar(stat="identity",width=.5) + scale_y_continuous(labels=percent, name="Percent Who Agree") + scale_x_discrete(name="Rated Level of Importance (1-7 = Low to High)") +  facet_grid(~Energy) + scale_fill_manual(values=c("khaki1","forest green")) + ggtitle("Regions Should Make Conservation Their First Priority \nto Reduce the Need for New Supply\n(Agreement by Rated Importance of Nuclear vs. Green Energy)") + theme(axis.text.x=element_text(colour="black", face="bold", size=14), axis.text.y=element_text(colour="black", face="bold", size=14), axis.title.x=element_text(colour="black", face="bold", size=14), axis.title.y=element_text(colour="black", face="bold", size=14), plot.title=element_text(size=18, face="bold"), strip.text=element_text(size=14), legend.title=element_text(size=14), legend.text=element_text(size=14))

