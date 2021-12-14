library (car)
library (mlogit)
library(lme4)
library(nlme)
library(psych)
library(Matrix)
library(lattice)
library (memisc)
setwd("C:/Users/Kayla/Dropbox/Kayla war ling analysis/SWPA")
likData<-as.data.set(spss.system.file("C:/Users/Kayla/Dropbox/Kayla war ling analysis/House of Representatives/LIKdata.sav"), use.value.labels=TRUE, to.data.frame=TRUE) 
options(scipen=999)

##mac stuff
setwd("~/Dropbox/Kayla war ling analysis/SWPA")
likData<-as.data.set(spss.system.file("~/Dropbox/Kayla war ling analysis/House of Representatives/LIKdata.sav"), use.value.labels=TRUE, to.data.frame=TRUE) 
options(scipen=999)


null<-glm(OpposeSupport~1, data=likData, family=binomial, na.action=na.omit)
nullnested1<-glmer(OpposeSupport~(1|Name), data=likData, family=binomial, na.action=na.omit)
nullnested2<-glmer(OpposeSupport~(1|Name)+(1|Region), data=likData, family=binomial, na.action=na.omit)
nullnested3<-glmer(OpposeSupport~(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
summary(null)
summary(nullnested1)
summary(nullnested2)
summary(nullnested3)
anova(null, nullnested1)
anova(null, nullnested2)
anova(nullnested2, nullnested3)
LinPro<-glmer(OpposeSupport~WPS+Sixltr+Dic+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
Pron<-glmer(OpposeSupport~i+we+you+shehe+they+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
Verb<-glmer(OpposeSupport~auxverb+past+present+future+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
Other<-glmer(OpposeSupport~article+adverb+preps+conj+negate+quant+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
SocEmo<-glmer(OpposeSupport~social+posemo+negemo+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
CogMec<-glmer(OpposeSupport~insight+cause+discrep+tentat+certain+inhib+incl+excl+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
Rel<-glmer(OpposeSupport~motion+space+time+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)
PerCon<-glmer(OpposeSupport~work+achieve+money+relig+death+(1|Name)+(1|Region)+(1|PartyAffiliation), data=likData, family=binomial, na.action=na.omit)


####Run the whole block! change predict(name of glmer here, leave rest)
yhats=predict(PerCon, na.action=na.exclude) ##gets the y values for each person
prob_adj=1/(1+exp(-yhats)) ##changes the y values into probabilities
threshold= 0.5 ### cut off for likelihood

###got the graphs to work!
plot(prob_adj,jitter(likData$OpposeSupport,0.05))
abline(v=threshold,col="red")
plot(density(na.omit(prob_adj)))
abline(v=threshold,col="red")
boxplot(prob_adj~as.integer(likData$OpposeSupport),ylab="probability",xlab="Oppose Support")
abline(h=threshold,col="red")


###this is the classification table
type=ifelse(prob_adj<=threshold,0,1)
###if else statement ... if prob_adj less than or equal to threshold, then 0, else 1
table(as.integer(likData$OpposeSupport))
table(type)
tt=table(na.omit(as.integer(likData$OpposeSupport)),type)
tt
false.pos.rate =tt[[3]]/(tt[[1]]+tt[[3]])
true.pos.rate = 1-false.pos.rate
false.neg.rate =tt[[2]]/(tt[[2]]+tt[[4]])
true.neg.rate = 1-false.neg.rate
cat("threshold=",threshold,"false pos rate=",round(false.pos.rate,4),"false neg rate=",round(false.neg.rate,4),"\n")
cat("threshold=",threshold,"true pos rate=",round(true.pos.rate,4),"true neg rate=",round(true.neg.rate,4),"\n")



###summary anovas
anova(nullnested3, LinPro)
anova(nullnested3, Pron)
anova(nullnested3, Verb)
anova(Other,nullnested3)
anova(nullnested3, SocEmo)
anova(nullnested3, CogMec)
anova(nullnested3, Rel)
anova(nullnested3, PerCon)
summary(Pron)
se <- sqrt(diag(vcov(Pron)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(Pron), LL = fixef(Pron) - 1.96 * se, UL = fixef(Pron) + 1.96 *se)
tab
exp(tab)
summary(Other)
se2 <- sqrt(diag(vcov(Other)))
# table of estimates with 95% CI
tab2 <- cbind(Est = fixef(Other), LL = fixef(Other) - 1.96 * se2, UL = fixef(Other) + 1.96 *se2)
tab2
exp(tab2)
summary(SocEmo)
se <- sqrt(diag(vcov(SocEmo)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(SocEmo), LL = fixef(SocEmo) - 1.96 * se, UL = fixef(SocEmo) + 1.96 *se)
tab
exp(tab)
summary(CogMec)
se <- sqrt(diag(vcov(CogMec)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(CogMec), LL = fixef(CogMec) - 1.96 * se, UL = fixef(CogMec) + 1.96 *se)
tab
exp(tab)
summary(Rel)
se <- sqrt(diag(vcov(Rel)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(Rel), LL = fixef(Rel) - 1.96 * se, UL = fixef(Rel) + 1.96 *se)
tab
exp(tab)
summary(PerCon)
se <- sqrt(diag(vcov(PerCon)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(PerCon), LL = fixef(PerCon) - 1.96 * se, UL = fixef(PerCon) + 1.96 *se)
tab
exp(tab)


se <- sqrt(diag(vcov(Pron)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(Pron), LL = fixef(Pron) - 1.96 * se, UL = fixef(Pron) + 1.96 *se)
exp(tab)