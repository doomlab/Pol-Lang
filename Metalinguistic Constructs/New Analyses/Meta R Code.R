library(lmerTest);library (car); library (mlogit);library(lme4);library(nlme);library(psych);library(Matrix);library(lattice);library (memisc);library(ggplot2);library(lmtest);library(gvlma)

setwd("C:/Users/Kayla/OneDrive/under review/Metalinguistic Constructs/New Analyses")
##for the lab computer
setwd("C:/Users/Doom-Lab/OneDrive/under review/Metalinguistic Constructs/New Analyses")

house=read.csv("House.csv", header=T)
senate=read.csv("Senate.csv", header=T)
foreign=read.csv("ForeignAffairsCommittee.csv", header=T)
options(scipen=999)

###Create interaction to calculate effect size for interaction
HouseInteraction<-(house$PartyAffiliation*house$War)
SenateInteraction<-(senate$PartyAffiliation*senate$War)
ForeignInteraction<-(foreign$PartyAffiliation*foreign$War)

###House-Psychological Distancing
nullnestedHouse<-lmer(PsychDistance~(1|Name), data=house, REML=FALSE, na.action=na.omit)
psychDistanceHouse<-lmer(PsychDistance~PartyAffiliation*War+(1|Name), data=house, REML=FALSE, na.action=na.omit)
psychDistancePA_House<-lmer(PsychDistance~PartyAffiliation+(1|Name), data=house, REML=FALSE, na.action=na.omit)
psychDistanceW_House<-lmer(PsychDistance~War+(1|Name), data=house, REML=FALSE, na.action=na.omit)
psychDistanceI_House<-lmer(PsychDistance~HouseInteraction+(1|Name), data=house, REML=FALSE, na.action=na.omit)


#Deviance
deviances<-data.frame(Venue=c(rep("House",2), rep("Senate",2), rep("ForeignAff",2)), Model = rep(c("Null", "Model"), 6), Construct=rep(c("PsychDistance", "PsychDistance", "ComplexThink", "ComplexThink"), 3), Deviance=rep(0,12))
nullDeviance1<-(summary(nullnestedHouse)$logLik)*-2
modelDeviance1<-(summary(psychDistanceHouse)$logLik)*-2
deviances[1,4]<-nullDeviance1
deviances[2,4]<-modelDeviance1

##Effect Sizes for House-Psychological Distancing
pdES<-data.frame(Construct=rep("Psychological Distancing",12), Venue=c(rep("House", 4), rep("Senate", 4), rep("Foreign Affairs Committee", 4)),Model=rep(c("Overall", "Party", "Action Taken", "Interaction"), 3), Rsq=rep(0, 12))
nullnestedH<-(print(VarCorr(nullnestedHouse), comp="Variance"))
nullnestedHRes<-as.numeric(nullnestedH[2,3])
psychDisH<-(print(VarCorr(psychDistanceHouse), comp="Variance"))
psychDisHRes<-as.numeric(psychDisH[2,3])
psychDisHOverall<-(nullnestedHRes-psychDisHRes)/nullnestedHRes
pdES[1, 4]<-psychDisHOverall
psychDisHP<-(print(VarCorr(psychDistancePA_House), comp="Variance"))
psychDisHPARes<-as.numeric(psychDisHP[2,3])
psychDisHPA<-(nullnestedHRes-psychDisHPARes)/nullnestedHRes
pdES[2, 4]<-psychDisHPA
psychDisHW<-(print(VarCorr(psychDistanceW_House), comp="Variance"))
psychDisHWRes<-as.numeric(psychDisHW[2,3])
psychDisHAT<-(nullnestedHRes-psychDisHWRes)/nullnestedHRes
pdES[3, 4]<-psychDisHAT
psychDisHInt<-(print(VarCorr(psychDistanceI_House), comp="Variance"))
psychDisHIRes<-as.numeric(psychDisHInt[2,3])
psychDisHI<-(nullnestedHRes-psychDisHIRes)/nullnestedHRes
pdES[4, 4]<-psychDisHI


###House-Complex Thinking
nullnestedHouse<-lmer(ComplexThink~(1|Name), data=house, REML=FALSE, na.action=na.omit)
complexThinkHouse<-lmer(ComplexThink~PartyAffiliation*War+(1|Name), data=house, REML=FALSE, na.action=na.omit)
complexThinkPA_House<-lmer(ComplexThink~PartyAffiliation+(1|Name), data=house, REML=FALSE, na.action=na.omit)
complexThinkW_House<-lmer(ComplexThink~War+(1|Name), data=house, REML=FALSE, na.action=na.omit)
complexThinkI_House<-lmer(ComplexThink~HouseInteraction+(1|Name), data=house, REML=FALSE, na.action=na.omit)

#Deviance
nullDeviance2<-(summary(nullnestedHouse)$logLik)*-2
modelDeviance2<-(summary(complexThinkHouse)$logLik)*-2
deviances[3,4]<-nullDeviance2
deviances[4,4]<-modelDeviance2

##Effect Sizes for House-Complex Thinking
ctES<-data.frame(Construct=rep("Complex Thinking",12), Venue=c(rep("House", 4), rep("Senate", 4), rep("Foreign Affairs Committee", 4)), Model=rep(c("Overall", "Party", "Action Taken", "Interaction"), 3), Rsq=rep(0, 12))
nullnestedH<-(print(VarCorr(nullnestedHouse), comp="Variance"))
nullnestedHRes<-as.numeric(nullnestedH[2,3])
complexThinkH<-(print(VarCorr(complexThinkHouse), comp="Variance"))
complexThinkHRes<-as.numeric(complexThinkH[2,3])
complexThinkHOverall<-(nullnestedHRes-complexThinkHRes)/nullnestedHRes
ctES[1, 4]<-complexThinkHOverall
complexThinkHP<-(print(VarCorr(complexThinkPA_House), comp="Variance"))
complexThinkHPARes<-as.numeric(complexThinkHP[2,3])
complexThinkHPA<-(nullnestedHRes-complexThinkHPARes)/nullnestedHRes
ctES[2, 4]<-complexThinkHPA
complexThinkHW<-(print(VarCorr(complexThinkW_House), comp="Variance"))
complexThinkHWRes<-as.numeric(complexThinkHW[2,3])
complexThinkHAT<-(nullnestedHRes-complexThinkHWRes)/nullnestedHRes
ctES[3, 4]<-complexThinkHAT
complexThinkHInt<-(print(VarCorr(complexThinkI_House), comp="Variance"))
complexThinkHIRes<-as.numeric(complexThinkHInt[2,3])
complexThinkHI<-(nullnestedHRes-complexThinkHIRes)/nullnestedHRes
ctES[4, 4]<-complexThinkHI

###Senate-Psychological Distancing
nullnestedSenate<-lmer(PsychDistance~(1|Name), data=senate, REML=FALSE, na.action=na.omit)
psychDistanceSenate<-lmer(PsychDistance~PartyAffiliation*War+(1|Name), data=senate, REML=FALSE, na.action=na.omit)
psychDistancePA_Senate<-lmer(PsychDistance~PartyAffiliation+(1|Name), data=senate, REML=FALSE, na.action=na.omit)
psychDistanceW_Senate<-lmer(PsychDistance~War+(1|Name), data=senate, REML=FALSE, na.action=na.omit)
psychDistanceI_Senate<-lmer(PsychDistance~SenateInteraction+(1|Name), data=senate, REML=FALSE, na.action=na.omit)

#Deviance
nullDeviance3<-(summary(nullnestedSenate)$logLik)*-2
modelDeviance3<-(summary(psychDistanceSenate)$logLik)*-2
deviances[5,4]<-nullDeviance3
deviances[6,4]<-modelDeviance3

##Effect Sizes for Senate-Psychological Distancing
nullnestedS<-(print(VarCorr(nullnestedSenate), comp="Variance"))
nullnestedSRes<-as.numeric(nullnestedS[2,3])
psychDisS<-(print(VarCorr(psychDistanceSenate), comp="Variance"))
psychDisSRes<-as.numeric(psychDisS[2,3])
psychDisSOverall<-(nullnestedSRes-psychDisSRes)/nullnestedSRes
pdES[5, 4]<-psychDisSOverall
psychDisSP<-(print(VarCorr(psychDistancePA_Senate), comp="Variance"))
psychDisSPARes<-as.numeric(psychDisSP[2,3])
psychDisSPA<-(nullnestedSRes-psychDisSPARes)/nullnestedSRes
pdES[6, 4]<-psychDisSPA
psychDisSW<-(print(VarCorr(psychDistanceW_Senate), comp="Variance"))
psychDisSWRes<-as.numeric(psychDisSW[2,3])
psychDisSAT<-(nullnestedSRes-psychDisSWRes)/nullnestedSRes
pdES[7, 4]<-psychDisSAT
psychDisSInt<-(print(VarCorr(psychDistanceI_Senate), comp="Variance"))
psychDisSIRes<-as.numeric(psychDisSInt[2,3])
psychDisSI<-(nullnestedSRes-psychDisSIRes)/nullnestedSRes
pdES[8, 4]<-psychDisSI

###Senate-Complex Thinking
nullnestedSenate<-lmer(ComplexThink~(1|Name), data=senate, REML=FALSE, na.action=na.omit)
complexThinkSenate<-lmer(ComplexThink~PartyAffiliation*War+(1|Name), data=senate, REML=FALSE, na.action=na.omit)
complexThinkPA_Senate<-lmer(ComplexThink~PartyAffiliation+(1|Name), data=senate, REML=FALSE, na.action=na.omit)
complexThinkW_Senate<-lmer(ComplexThink~War+(1|Name), data=senate, REML=FALSE, na.action=na.omit)
complexThinkI_Senate<-lmer(ComplexThink~SenateInteraction+(1|Name), data=senate, REML=FALSE, na.action=na.omit)

#Deviance
nullDeviance4<-(summary(nullnestedSenate)$logLik)*-2
modelDeviance4<-(summary(complexThinkSenate)$logLik)*-2
deviances[7,4]<-nullDeviance4
deviances[8,4]<-modelDeviance4

##Effect Sizes for Senate-Complex Thinking
nullnestedS<-(print(VarCorr(nullnestedSenate), comp="Variance"))
nullnestedSRes<-as.numeric(nullnestedS[2,3])
complexThinkS<-(print(VarCorr(complexThinkSenate), comp="Variance"))
complexThinkSRes<-as.numeric(complexThinkS[2,3])
complexThinkSOverall<-(nullnestedSRes-complexThinkSRes)/nullnestedSRes
ctES[5, 4]<-complexThinkSOverall
complexThinkSP<-(print(VarCorr(complexThinkPA_Senate), comp="Variance"))
complexThinkSPARes<-as.numeric(complexThinkSP[2,3])
complexThinkSPA<-(nullnestedSRes-complexThinkSPARes)/nullnestedSRes
ctES[6, 4]<-complexThinkSPA
complexThinkSW<-(print(VarCorr(complexThinkW_Senate), comp="Variance"))
complexThinkSWRes<-as.numeric(complexThinkSW[2,3])
complexThinkSAT<-(nullnestedSRes-complexThinkSWRes)/nullnestedSRes
ctES[7, 4]<-complexThinkSAT
complexThinkSInt<-(print(VarCorr(complexThinkI_Senate), comp="Variance"))
complexThinkSIRes<-as.numeric(complexThinkSInt[2,3])
complexThinkSI<-(nullnestedSRes-complexThinkSIRes)/nullnestedSRes
ctES[8, 4]<-complexThinkSI

#Post hoc
demsSen=subset(senate, PartyAffiliation==1)
repSen=subset(senate, PartyAffiliation==0)
postDems=lmer(ComplexThink~War+(1|Name), data=demsSen, REML=FALSE, na.action=na.omit)
postRep=lmer(ComplexThink~War+(1|Name), data=repSen, REML=FALSE, na.action=na.omit)



###Foreign Affairs Committee-Psychological Distancing
nullnestedForeign<-lmer(PsychDistance~(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
psychDistanceForeign<-lmer(PsychDistance~PartyAffiliation*War+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
psychDistancePA_Foreign<-lmer(PsychDistance~PartyAffiliation+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
psychDistanceW_Foreign<-lmer(PsychDistance~War+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
psychDistanceI_Foreign<-lmer(PsychDistance~ForeignInteraction+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)


#Deviance
nullDeviance5<-(summary(nullnestedForeign)$logLik)*-2
modelDeviance5<-(summary(psychDistanceForeign)$logLik)*-2
deviances[9,4]<-nullDeviance5
deviances[10,4]<-modelDeviance5

##Effect Sizes for Foreign Affairs Committee-Psychological Distancing
nullnestedF<-(print(VarCorr(nullnestedForeign), comp="Variance"))
nullnestedFRes<-as.numeric(nullnestedF[2,3])
psychDisF<-(print(VarCorr(psychDistanceForeign), comp="Variance"))
psychDisFRes<-as.numeric(psychDisF[2,3])
psychDisFOverall<-(nullnestedFRes-psychDisFRes)/nullnestedFRes
pdES[9, 4]<-psychDisFOverall
psychDisFP<-(print(VarCorr(psychDistancePA_Foreign), comp="Variance"))
psychDisFPARes<-as.numeric(psychDisFP[2,3])
psychDisFPA<-(nullnestedFRes-psychDisFPARes)/nullnestedFRes
pdES[10, 4]<-psychDisFPA
psychDisFW<-(print(VarCorr(psychDistanceW_Foreign), comp="Variance"))
psychDisFWRes<-as.numeric(psychDisFW[2,3])
psychDisFAT<-(nullnestedFRes-psychDisFWRes)/nullnestedFRes
pdES[11, 4]<-psychDisFAT
psychDisFInt<-(print(VarCorr(psychDistanceI_Foreign), comp="Variance"))
psychDisFIRes<-as.numeric(psychDisFInt[2,3])
psychDisFI<-(nullnestedFRes-psychDisFIRes)/nullnestedFRes
pdES[12, 4]<-psychDisFI

#Post-hoc
demsFor=subset(foreign, PartyAffiliation==1)
repFor=subset(foreign, PartyAffiliation==0)
postDems=lmer(PsychDistance~War+(1|Name), data=demsFor, REML=FALSE, na.action=na.omit)
postRep=lmer(PsychDistance~War+(1|Name), data=repFor, REML=FALSE, na.action=na.omit)

###Foreign Affairs Committee-Complex Thinking
nullnestedForeign<-lmer(ComplexThink~(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
complexThinkForeign<-lmer(ComplexThink~PartyAffiliation*War+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
complexThinkPA_Foreign<-lmer(ComplexThink~PartyAffiliation+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
complexThinkW_Foreign<-lmer(ComplexThink~War+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)
complexThinkI_Foreign<-lmer(ComplexThink~ForeignInteraction+(1|Name), data=foreign, REML=FALSE, na.action=na.omit)


#Deviance
nullDeviance6<-(summary(nullnestedForeign)$logLik)*-2
modelDeviance6<-(summary(complexThinkForeign)$logLik)*-2
deviances[11,4]<-nullDeviance6
deviances[12,4]<-modelDeviance6

##Effect Sizes for Foreign Affairs Committee-Complex Thinking
nullnestedF<-(print(VarCorr(nullnestedForeign), comp="Variance"))
nullnestedFRes<-as.numeric(nullnestedF[2,3])
complexThinkF<-(print(VarCorr(complexThinkForeign), comp="Variance"))
complexThinkFRes<-as.numeric(complexThinkF[2,3])
complexThinkFOverall<-(nullnestedFRes-complexThinkFRes)/nullnestedFRes
ctES[9, 4]<-complexThinkFOverall
complexThinkFP<-(print(VarCorr(complexThinkPA_Foreign), comp="Variance"))
complexThinkFPARes<-as.numeric(complexThinkFP[2,3])
complexThinkFPA<-(nullnestedFRes-complexThinkFPARes)/nullnestedFRes
ctES[10, 4]<-complexThinkFPA
complexThinkFW<-(print(VarCorr(complexThinkW_Foreign), comp="Variance"))
complexThinkFWRes<-as.numeric(complexThinkFW[2,3])
complexThinkFAT<-(nullnestedFRes-complexThinkFWRes)/nullnestedFRes
ctES[11, 4]<-complexThinkFAT
complexThinkFInt<-(print(VarCorr(complexThinkI_Foreign), comp="Variance"))
complexThinkFIRes<-as.numeric(complexThinkFInt[2,3])
complexThinkFI<-(nullnestedFRes-complexThinkFIRes)/nullnestedFRes
ctES[12, 4]<-complexThinkFI

deviances
pdh<-as.data.frame(summary(psychDistanceHouse)$coefficients)
pdhci<-as.data.frame(confint(psychDistanceHouse, method="profile"))
cth<-as.data.frame(summary(complexThinkHouse)$coefficients)
cthci<-as.data.frame(confint(complexThinkHouse, method="profile"))
pds<-as.data.frame(summary(psychDistanceSenate)$coefficients)
pdsci<-as.data.frame(confint(psychDistanceSenate, method="profile"))
cts<-as.data.frame(summary(complexThinkSenate)$coefficients)
ctsci<-as.data.frame(confint(complexThinkSenate, method="profile"))
pdf<-as.data.frame(summary(psychDistanceForeign)$coefficients)
pdfci<-as.data.frame(confint(psychDistanceForeign, method="profile"))
ctf<-as.data.frame(summary(complexThinkForeign)$coefficients)
ctfci<-as.data.frame(confint(complexThinkForeign, method="profile"))
pdES
ctES



###Graphs
houseCT<-ggplot(house, aes(factor(War), ComplexThink, fill=factor(PartyAffiliation)))
houseCT<-houseCT+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.95), width=0.2)+labs(x="Policy Decision", y="Complex Thinking", fill="PartyAffiliation")
houseCT<-houseCT+scale_fill_manual(values=c("light grey", "dark grey"), labels=c("Republican", "Democrat"), name="Party Affiliation")
houseCT+theme_classic()

housePD<-ggplot(house, aes(factor(War), PsychDistance, fill=factor(PartyAffiliation)))
housePD<-housePD+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.95), width=0.2)+labs(x="Policy Decision", y="Psychological Distancing", fill="PartyAffiliation")
housePD<-housePD+scale_fill_manual(values=c("light grey", "dark grey"))
housePD+theme_classic()

senateCT<-ggplot(senate, aes(factor(War), ComplexThink, fill=factor(PartyAffiliation)))
senateCT<-senateCT+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.95), width=0.2)+labs(x="Policy Decision", y="Complex Thinking", fill="PartyAffiliation")
senateCT<-senateCT+scale_fill_manual(values=c("light grey", "dark grey"))
senateCT+theme_classic()

senatePD<-ggplot(senate, aes(factor(War), PsychDistance, fill=factor(PartyAffiliation)))
senatePD<-senatePD+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.95), width=0.2)+labs(x="Policy Decision", y="Psychological Distancing", fill="PartyAffiliation")
senatePD<-senatePD+scale_fill_manual(values=c("light grey", "dark grey"))
senatePD+theme_classic()

foreignCT<-ggplot(foreign, aes(factor(War), ComplexThink, fill=factor(PartyAffiliation)))
foreignCT<-foreignCT+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.95), width=0.2)+labs(x="Policy Decision", y="Complex Thinking", fill="PartyAffiliation")
foreignCT<-foreignCT+scale_fill_manual(values=c("light grey", "dark grey"))
foreignCT+theme_classic()

foreignPD<-ggplot(foreign, aes(factor(War), PsychDistance, fill=factor(PartyAffiliation)))
foreignPD<-foreignPD+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90), width=0.2)+labs(x="Policy Decision", y="Psychological Distancing", fill="PartyAffiliation")
foreignPD<-foreignPD+scale_fill_manual(values=c("light grey", "dark grey"))
foriegnPD+theme_classic()


ct00 = subset(house, War==0 & PartyAffiliation==0)
ct01 = subset(house, War==0 & PartyAffiliation==1)
ct10 = subset(house, War==1 & PartyAffiliation==0)
ct11 = subset(house, War==1 & PartyAffiliation==1)

ct00 = subset(senate, War==0 & PartyAffiliation==0)
ct01 = subset(senate, War==0 & PartyAffiliation==1)
ct10 = subset(senate, War==1 & PartyAffiliation==0)
ct11 = subset(senate, War==1 & PartyAffiliation==1)

ct00 = subset(foreign, War==0 & PartyAffiliation==0)
ct01 = subset(foreign, War==0 & PartyAffiliation==1)
ct10 = subset(foreign, War==1 & PartyAffiliation==0)
ct11 = subset(foreign, War==1 & PartyAffiliation==1)

m00 = mean(ct00$ComplexThink)
m01 = mean(ct01$ComplexThink)
m10 = mean(ct10$ComplexThink)
m11 = mean(ct11$ComplexThink)
ll00 = m00 - (qt(.975, df = nrow(ct00)-1)*(sd(ct00$ComplexThink)/sqrt(nrow(ct00))))
ul00 = m00 + (qt(.975, df = nrow(ct00)-1)*(sd(ct00$ComplexThink)/sqrt(nrow(ct00))))
ll01 = m01 - (qt(.975, df = nrow(ct01)-1)*(sd(ct01$ComplexThink)/sqrt(nrow(ct01))))
ul01 = m01 + (qt(.975, df = nrow(ct01)-1)*(sd(ct01$ComplexThink)/sqrt(nrow(ct01))))
ll10 = m10 - (qt(.975, df = nrow(ct10)-1)*(sd(ct10$ComplexThink)/sqrt(nrow(ct10))))
ul10 = m10 + (qt(.975, df = nrow(ct10)-1)*(sd(ct10$ComplexThink)/sqrt(nrow(ct10))))
ll11 = m11 - (qt(.975, df = nrow(ct11)-1)*(sd(ct11$ComplexThink)/sqrt(nrow(ct11))))
ul11 = m11 + (qt(.975, df = nrow(ct11)-1)*(sd(ct11$ComplexThink)/sqrt(nrow(ct11))))


sumD = data.frame(Venue=character(0),Policy=character(0),Party=character(0),Mean=numeric(0), LL=numeric(0), UL=numeric(0))
sumR = data.frame(Venue=character(0),Policy=character(0),Party=character(0),Mean=numeric(0), LL=numeric(0), UL=numeric(0))

sumD = matrix(,nrow=0, ncol=6, dimnames=list(c(),c("Venue","Policy", "Party", "Mean", "LL", "UL")))
sumR = matrix(,nrow=0, ncol=6, dimnames=list(c(),c("Venue","Policy", "Party", "Mean", "LL", "UL")))

ahh = function(data, variable, venue, policy, party, dataframe)
{
  mean = mean(data[[variable]])
  ll = mean - (qt(.975, df = nrow(data)-1)*(sd(data[[variable]])/sqrt(nrow(data))))
  ul = mean + (qt(.975, df = nrow(data)-1)*(sd(data[[variable]])/sqrt(nrow(data))))
  x1 = c(venue, policy, party, mean, ll, ul)
  sum = (rbind(as.matrix(dataframe), x1))
  return (sum)
}

sumR = ahh(ct00, "ComplexThink", "House", "No Action Taken", "Republican", sumR)
sumD = ahh(ct01, "ComplexThink", "House", "No Action Taken", "Democrat", sumD)
sumR = ahh(ct10, "ComplexThink", "House", "Action Taken", "Republican", sumR)
sumD = ahh(ct11, "ComplexThink", "House", "Action Taken", "Democrat", sumD)

sumR = ahh(ct00, "ComplexThink", "Senate", "No Action Taken", "Republican", sumR)
sumD = ahh(ct01, "ComplexThink", "Senate", "No Action Taken", "Democrat", sumD)
sumR = ahh(ct10, "ComplexThink", "Senate", "Action Taken", "Republican", sumR)
sumD = ahh(ct11, "ComplexThink", "Senate", "Action Taken", "Democrat", sumD)

sumR = ahh(ct00, "ComplexThink", "Foreign Affairs", "No Action Taken", "Republican", sumR)
sumD = ahh(ct01, "ComplexThink", "Foreign Affairs", "No Action Taken", "Democrat", sumD)
sumR = ahh(ct10, "ComplexThink", "Foreign Affairs", "Action Taken", "Republican", sumR)
sumD = ahh(ct11, "ComplexThink", "Foreign Affairs", "Action Taken", "Democrat", sumD)

library(forestplot)
forestplot(mean = as.matrix(cbind(sumR$Mean, sumD$Mean)), 
           lower = as.matrix(cbind(sumR$LL, sumD$LL)),
           upper = as.matrix(cbind(sumR$UL, sumD$LL)),
           labeltext = as.matrix(sumR$Policy),
           #legend = c("No Action Taken", "Action Taken"),
           clip = c(-4,4),
           xticks = c(-4,-3,-2,-1,0,1,2,3,4),
           boxsize = 0.1,
           col=fpColors(box=c("dark grey","black")),
           xlab = "Complex Thinking",
           txt_gp = fpTxtGp(xlab=gpar(cex=1), label=gpar(cex=1), ticks=gpar(cex=0.9)),
           lwd.ci = 2,
           new_page=T)