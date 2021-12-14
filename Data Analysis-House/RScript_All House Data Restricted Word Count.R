install.packages("car")
install.packages("mlogit");install.packages("mlogit");install.packages("lme4"); install.packages("psych");install.packages("Matrix");install.packages("lattice");install.packages("memisc")
library (car); library (mlogit)
library(lme4)
library(nlme)
library(psych)
library(Matrix);library(lattice)
library (memisc)
setwd("C:/Users/Kayla/Dropbox/Kayla war ling analysis/House of Representatives")

houseData<-as.data.set(spss.system.file("AllHouseRestrictedWordCount_1.sav"), use.value.labels=TRUE, to.data.frame=TRUE) 
options(scipen=999)

null<-glm(PartyAffiliation~1, data=houseData, family=binomial, na.action=na.omit)
nullnested1<-glmer(PartyAffiliation~(1|Name), data=houseData, family=binomial, na.action=na.omit)
nullnested2<-glmer(PartyAffiliation~(1|Name)+(1|Region), data=houseData, family=binomial, na.action=na.omit)

summary(null)
summary(nullnested1)
summary(nullnested2)

anova(null, nullnested1)
anova(null, nullnested2)

LinPro<-glmer(PartyAffiliation~WPS+Sixltr+Dic+(1|Name), data=houseData, family=binomial, na.action=na.omit)
Pron<-glmer(PartyAffiliation~i+we+you+shehe+they+(1|Name), data=houseData, family=binomial, na.action=na.omit)
Verb<-glmer(PartyAffiliation~auxverb+past+present+future+(1|Name), data=houseData, family=binomial, na.action=na.omit)
Other<-glmer(PartyAffiliation~article+adverb+preps+conj+negate+quant+(1|Name), data=houseData, family=binomial, na.action=na.omit)
SocEmo<-glmer(PartyAffiliation~social+posemo+negemo+(1|Name), data=houseData, family=binomial, na.action=na.omit)
CogMec<-glmer(PartyAffiliation~insight+cause+discrep+tentat+certain+inhib+incl+excl+(1|Name), data=houseData, family=binomial, na.action=na.omit)
Rel<-glmer(PartyAffiliation~motion+space+time+(1|Name), data=houseData, family=binomial, na.action=na.omit)
PerCon<-glmer(PartyAffiliation~work+achieve+money+relig+death+(1|Name), data=houseData, family=binomial, na.action=na.omit)
anova(nullnested1, LinPro)
anova(nullnested1, Pron)
anova(nullnested1, Verb)
anova(Other,nullnested1)
anova(nullnested1, SocEmo)
anova(nullnested1, CogMec)
anova(nullnested1, Rel)
anova(nullnested1, PerCon)
summary(Pron)
summary(PerCon)
summary(Rel)

d2h<-glmer(PartyAffiliation~Dimension2High+(1|Name), data=houseData, family=binomial, na.action=na.omit)
d2l<-glmer(PartyAffiliation~Dimension2Low+(1|Name), data=houseData, family=binomial, na.action=na.omit)
mds<-glmer(PartyAffiliation~cause+negemo+past+inhib+work+motion+insight+quant+(1|Name), data=houseData, family=binomial, na.action=na.omit)
meta<-glmer(PartyAffiliation~CogComplex+Depression+Age+Presidentiality+EmotionPositivity+CogProcessing+SocialOrientation+PsychDistance+(1|Name), data=houseData, family=binomial, na.action=na.omit)
SocOri<-glmer(PartyAffiliation~SocialOrientation+(1|Name), data=houseData, family=binomial, na.action=na.omit)
mds<-glmer(PartyAffiliation~Dimension1+Dimension2+(1|Name), data=houseData, family=binomial, na.action=na.omit)


anova(nullnested1,mds)
anova(nullnested1,meta)
summary (d2h)
summary (d2l)
summary (mds)
summary (meta)
summary(SocOri)

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


se <- sqrt(diag(vcov(mds)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(mds), LL = fixef(mds) - 1.96 * se, UL = fixef(mds) + 1.96 *se)
exp(tab)

se <- sqrt(diag(vcov(meta)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(meta), LL = fixef(meta) - 1.96 * se, UL = fixef(meta) + 1.96 *se)
exp(tab)