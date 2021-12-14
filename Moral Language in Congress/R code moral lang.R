library(lmerTest);library (car); library (mlogit);library(lme4);library(nlme);library(psych);library(Matrix);library(lattice);library (memisc);library(ggplot2);library(lmtest);library(gvlma);library(reshape2)
setwd("C:/Users/Kayla/OneDrive/projects/Political Language/")

data = read.csv("Moral Language in Congress/congress_moral_nona.csv", header=TRUE)
options(scipen=999)

harm = subset(data, Foundation=="Harm")
fair = subset(data, Foundation=="Fairness")
ingroup = subset(data, Foundation=="Ingroup")
authority = subset(data, Foundation=="Authority")
purity = subset(data, Foundation=="Purity")

dataC = dcast(data, ID+Region+Name+Party+Date+Venue~Foundation, value.var="Coverage")
houseC = subset(dataC, Venue=="House")
senateC = subset(dataC, Venue=="Senate")
faC = subset(dataC, Venue=="House Hearings"|Venue=="Senate Hearings")
dataR = dcast(data, ID+Region+Name+Party+Date+Venue~Foundation, value.var="Reference")
houseR = subset(dataR, Venue=="House")
senateR = subset(dataR, Venue=="Senate")
faR = subset(dataR, Venue=="House Hearings"|Venue=="Senate Hearings")

###Predicting Party
##Using number of References as predictors
##All Venues
nested<-glmer(Party~(1|Name), data=dataC, family=binomial, na.action=na.omit)
houseRef<-glmer(Party~Harm+Fairness+Ingroup+Authority+Purity+(1|Name), data=dataC, family=binomial, na.action=na.omit)
se <- sqrt(diag(vcov(houseRef)))
tab <- cbind(Est = fixef(houseRef), LL = fixef(houseRef) - 1.96 * se, UL = fixef(houseRef) + 1.96 *se)
tab
exp(tab)
##Using number of Coverage as predictors
##All Venues
nested<-glmer(Party~(1|Name), data=dataR, family=binomial, na.action=na.omit)
houseRef<-glmer(Party~Harm+Fairness+Ingroup+Authority+Purity+(1|Name), data=dataR, family=binomial, na.action=na.omit)
se <- sqrt(diag(vcov(houseRef)))
tab <- cbind(Est = fixef(houseRef), LL = fixef(houseRef) - 1.96 * se, UL = fixef(houseRef) + 1.96 *se)
tab
exp(tab)



##Harm Models
nestedH<-glmer(Reference~(1|Name), data=harm, family=poisson, na.action=na.omit)
harmModel<-glmer(Reference~Party+(1|Name), data=harm, family=poisson, na.action=na.omit)
harmCoef<-as.matrix(summary(harmModel)$coefficients)
harmCI<-confint(harmModel, method="profile")

devianceValues<-data.frame(Foundation=c("Null Nested", "Harm", "Null Nested","Fairness", "Null Nested","Ingroup", "Null Nested","Authority", "Null Nested","Purity"), devianceLik=rep(0, 10))
devianceValues[1,2]<-(summary(nestedH)$logLik)*-2
devianceValues[2,2]<-(summary(harmModel)$logLik)*-2

##Effect Sizes for Harm
effectsizes<-data.frame(Foundation=c("Harm", "Fairness", "Ingroup", "Authority", "Purity"), Rsq=rep(0, 5))
nestedHR<-(print(VarCorr(nestedH), comp="Variance"))
nestedHRes<-as.numeric(nestedHR[1,3])
harmR<-(print(VarCorr(harmModel), comp="Variance"))
harmRes<-as.numeric(harmR[1,3])
harmES<-(nestedHRes-harmRes)/nestedHRes
effectsizes[1,2]<-harmES


##Fairness Models
nestedF<-glmer(Reference~(1|Name), data=fair, family=poisson, na.action=na.omit)
fairnessModel<-glmer(Reference~Party+(1|Name), data=fair, family=poisson, na.action=na.omit)
fairnessCoef<-as.matrix(summary(fairnessModel)$coefficients)
fairnessCI<-confint(fairnessModel, method="profile")
devianceValues[3,2]<-(summary(nestedF)$logLik)*-2
devianceValues[4,2]<-(summary(fairnessModel)$logLik)*-2

##Effect Size for Fairness
nestedFR<-(print(VarCorr(nestedF), comp="Variance"))
nestedFRes<-as.numeric(nestedFR[1,3])
fairnessR<-(print(VarCorr(fairnessModel), comp="Variance"))
fairnessRes<-as.numeric(fairnessR[1,3])
fairnessES<-(nestedFRes-fairnessRes)/nestedFRes
effectsizes[2,2]<-fairnessES

##Ingroup Models
nestedI<-glmer(Reference~(1|Name), data=ingroup, family=poisson, na.action=na.omit)
ingroupModel<-glmer(Reference~Party+(1|Name), data=ingroup, family=poisson, na.action=na.omit)
ingroupCoef<-as.matrix(summary(ingroupModel)$coefficients)
ingroupCI<-confint(ingroupModel, method="profile")
devianceValues[5,2]<-(summary(nestedI)$logLik)*-2
devianceValues[6,2]<-(summary(ingroupModel)$logLik)*-2

##Effect Size for Fairness
nestedIR<-(print(VarCorr(nestedI), comp="Variance"))
nestedIRes<-as.numeric(nestedIR[1,3])
ingroupR<-(print(VarCorr(ingroupModel), comp="Variance"))
ingroupRes<-as.numeric(ingroupR[1,3])
ingroupES<-(nestedIRes-ingroupRes)/nestedIRes
effectsizes[3,2]<-ingroupES

##Authority Models
nestedA<-glmer(Reference~(1|Name), data=authority, family=poisson, na.action=na.omit)
authorityModel<-glmer(Reference~Party+(1|Name), data=authority, family=poisson, na.action=na.omit)
authorityCoef<-as.matrix(summary(authorityModel)$coefficients)
authorityCI<-confint(authorityModel, method="profile")
devianceValues[7,2]<-(summary(nestedA)$logLik)*-2
devianceValues[8,2]<-(summary(authorityModel)$logLik)*-2

##Effect Size for Authority
nestedAR<-(print(VarCorr(nestedA), comp="Variance"))
nestedARes<-as.numeric(nestedAR[1,3])
authorityR<-(print(VarCorr(authorityModel), comp="Variance"))
authorityRes<-as.numeric(authorityR[1,3])
authorityES<-(nestedARes-authorityRes)/nestedARes
effectsizes[4,2]<-authorityES

##Purity Models
nestedP<-glmer(Reference~(1|Name), data=purity, family=poisson, na.action=na.omit)
purityModel<-glmer(Reference~Party+(1|Name), data=purity, family=poisson, na.action=na.omit)
purityCoef<-as.matrix(summary(purityModel)$coefficients)
purityCI<-confint(purityModel, method="profile")
devianceValues[9,2]<-(summary(nestedP)$logLik)*-2
devianceValues[10,2]<-(summary(purityModel)$logLik)*-2

##Effect Size for Purity
nestedPR<-(print(VarCorr(nestedP), comp="Variance"))
nestedPRes<-as.numeric(nestedPR[1,3])
purityR<-(print(VarCorr(purityModel), comp="Variance"))
purityRes<-as.numeric(purityR[1,3])
purityES<-(nestedPRes-purityRes)/nestedPRes
effectsizes[5,2]<-purityES

