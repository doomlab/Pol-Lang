library (car); library (mlogit)
library(lme4)
library(nlme)
library(psych)
library(Matrix);library(lattice)
library (memisc)
library(ggplot2)
library(lmtest)
setwd("C:/Users/Kayla/OneDrive/projects/Political Language/Data")

data=read.csv("CongressDataMC_PA.csv", header=TRUE) 
options(scipen=999)
Region=factor(data$Region)
levels(Region)

###Metalinguistic COnstructs

null<-gls(CogProcess~1, data=data, method="ML", na.action=na.omit)
nullnested<-lmer(CogProcess~(1|Name), data=data, REML=FALSE, na.action=na.omit)
nullnested2<-lmer(CogProcess~(1|Name)+(1|Venue), data=data, REML=FALSE, 
                  na.action=na.omit)
summary(null)
summary(nullnested)
summary(nullnested2)
cp<-lmer(CogProcess~PartyAffiliation*Afghanistan+PartyAffiliation*Iran+
        PartyAffiliation*NorthKorea+PartyAffiliation*Russia+PartyAffiliation*
        Syria+PartyAffiliation*Kosovo+PartyAffiliation*Sudan+PartyAffiliation*
        Libya+(1|Name)+(1|Venue), data=data, na.action=na.omit)
summary(cp)
out1 <- summary(cp)
coef1 <- coef(out1)
tvalue1 <- coef1[,"t value"]
pnorm(abs(tvalue1), lower.tail=FALSE) * 2
confint(cp,method="Wald") 
bar5<-ggplot(data, aes(factor(Region),CogProcess, fill=factor(PartyAffiliation)))
bar5+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary
     (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90), 
      width=0.2)+labs(x="Region", y="Cognitive Processing", fill=
      "Party Affiliation")

null<-gls(CatThink~1, data=data, method="ML", na.action=na.omit)
nullnested<-lmer(CatThink~1+(1|Name), data=data, REML=FALSE, na.action=na.omit)
nullnested2<-lmer(CatThink~1+(1|Name)+(1|Venue), data=data, REML=FALSE, 
            na.action=na.omit)
summary(null)
summary(nullnested)
summary(nullnested2)
ct<-lmer(CatThink~PartyAffiliation*Afghanistan+PartyAffiliation*Iran+
        PartyAffiliation*NorthKorea+PartyAffiliation*Russia+PartyAffiliation*
        Syria+PartyAffiliation*Kosovo+PartyAffiliation*Sudan+PartyAffiliation*
        Libya+(1|Name)+(1|Venue), data=data, na.action=na.omit)
summary(ct)
out1 <- summary(ct)
coef1 <- coef(out1)
tvalue1 <- coef1[,"t value"]
pnorm(abs(tvalue1), lower.tail=FALSE) * 2
confint(ct,method="profile") 
bar5<-ggplot(data, aes(factor(Region),CatThink, fill=factor(PartyAffiliation)))
bar5+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary
      (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),
       width=0.2)+labs(x="Region", y="CatThink", fill="PartyAffiliation")

null<-gls(ComplexThink~1, data=data, method="ML", na.action=na.omit)
nullnested<-lmer(ComplexThink~1+(1|Name), data=data, REML=FALSE, 
                 na.action=na.omit)
nullnested2<-lmer(ComplexThink~1+(1|Name)+(1|Venue), data=data, REML=FALSE, 
                  na.action=na.omit)
summary(null)
summary(nullnested)
summary(nullnested2)
cmt<-lmer(ComplexThink~PartyAffiliation*Afghanistan+PartyAffiliation*Iran+
          PartyAffiliation*NorthKorea+PartyAffiliation*Russia+PartyAffiliation
          *Syria+PartyAffiliation*Kosovo+PartyAffiliation*Sudan+PartyAffiliation
          *Libya+(1|Name)+(1|Venue), data=data, na.action=na.omit)
summary(cmt)
out1 <- summary(cmt)
coef1 <- coef(out1)
tvalue1 <- coef1[,"t value"]
pnorm(abs(tvalue1), lower.tail=FALSE) * 2
confint(cmt,method="profile") 
bar5<-ggplot(data, aes(factor(Region),ComplexThink, fill=factor(PartyAffiliation)))
bar5+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary
      (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),
       width=0.2)+labs(x="Region", y="ComplexThink", fill="PartyAffiliation")

null<-gls(PsychDistance~1, data=data, method="ML", na.action=na.omit)
nullnested<-lmer(PsychDistance~1+(1|Name), data=data, REML=FALSE, 
             na.action=na.omit)
nullnested2<-lmer(PsychDistance~1+(1|Name)+(1|Venue), data=data, REML=FALSE, 
            na.action=na.omit)
summary(null)
summary(nullnested)
summary(nullnested2)
pd<-lmer(PsychDistance~PartyAffiliation*Afghanistan+PartyAffiliation*Iran+
        PartyAffiliation*NorthKorea+PartyAffiliation*Russia+PartyAffiliation*
        Syria+PartyAffiliation*Kosovo+PartyAffiliation*Sudan+PartyAffiliation*
        Libya+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(pd)
out1 <- summary(pd)
coef1 <- coef(out1)
tvalue1 <- coef1[,"t value"]
pnorm(abs(tvalue1), lower.tail=FALSE) * 2
confint(pd,method="profile") 
bar5<-ggplot(data, aes(factor(Region),PsychDistance, fill=factor(PartyAffiliation)))
bar5+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary
      (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),
       width=0.2)+labs(x="Region", y="PsychDistance", fill="PartyAffiliation")

null<-gls(Honesty~1, data=data, method="ML", na.action=na.omit)
nullnested<-lmer(Honesty~1+(1|Name), data=data, REML=FALSE, na.action=na.omit)
nullnested2<-lmer(Honesty~1+(1|Name)+(1|Venue), data=data, REML=FALSE, 
                  na.action=na.omit)
summary(null)
summary(nullnested)
summary(nullnested2)
ht<-lmer(Honesty~PartyAffiliation*Afghanistan+PartyAffiliation*Iran+
        PartyAffiliation*NorthKorea+PartyAffiliation*Russia+PartyAffiliation*
        Syria+PartyAffiliation*Kosovo+PartyAffiliation*Sudan+PartyAffiliation*
        Libya++(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(ht)
out1 <- summary(ht)
coef1 <- coef(out1)
tvalue1 <- coef1[,"t value"]
pnorm(abs(tvalue1), lower.tail=FALSE) * 2
confint(ht,method="profile") 
bar5<-ggplot(data, aes(factor(Region),Honesty, fill=factor(PartyAffiliation)))
bar5+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary
      (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),
       width=0.2)+labs(x="Region", y="Honesty", fill="PartyAffiliation")

####erin testing stuff
##predicted=predict(variable name, na.action=na.exclude)
##prob=1/(1+exp(-predicted))

predicted=predict(logit, na.action=na.exclude)
prob=1/(1+exp(-predicted))


###Random Test Models
d2h<-glmer(PartyAffiliation~Dimension2High+(1|Name), data=houseData, family=binomial, na.action=na.omit)
d2l<-glmer(PartyAffiliation~Dimension2Low+(1|Name), data=houseData, family=binomial, na.action=na.omit)
mds<-glmer(PartyAffiliation~cause+negemo+past+inhib+work+motion+insight+quant+(1|Name), data=houseData, family=binomial, na.action=na.omit)
meta<-glmer(PartyAffiliation~CogComplex+Depression+Age+Presidentiality+EmotionPositivity+CogProcessing+SocialOrientation+PsychDistance+(1|Name), data=houseData, family=binomial, na.action=na.omit)
SocOri<-glmer(PartyAffiliation~SocialOrientation+(1|Name), data=houseData, family=binomial, na.action=na.omit)


summary (d2h)
summary (d2l)
summary (mds)
summary (meta)
summary(SocOri)


### Effect sizes for log models 
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


###Predicting Party Affiliation - Didn't work
null<-glm(PartyAffiliation~1, data=data, family=binomial, na.action=na.omit)
nullnested1<-glmer(PartyAffiliation~(1|Name), data=data, family=binomial, na.action=na.omit)
nullnested2<-glmer(PartyAffiliation~(1|Name)+(1|Region), data=data, family=binomial, na.action=na.omit)

summary(null)
summary(nullnested1)
summary(nullnested2)

anova(null, nullnested1)
anova(null, nullnested2)

LinPro<-glmer(PartyAffiliation~WPS+Sixltr+Dic+(1|Name), data=data, family=binomial, na.action=na.omit)
Pron<-glmer(PartyAffiliation~i+we+you+shehe+they+(1|Name), data=data, family=binomial, na.action=na.omit)
Verb<-glmer(PartyAffiliation~auxverb+past+present+future+(1|Name), data=data, family=binomial, na.action=na.omit)
Other<-glmer(PartyAffiliation~article+adverb+preps+conj+negate+quant+(1|Name), data=data, family=binomial, na.action=na.omit)
SocEmo<-glmer(PartyAffiliation~social+posemo+negemo+(1|Name), data=data, family=binomial, na.action=na.omit)
CogMec<-glmer(PartyAffiliation~insight+cause+discrep+tentat+certain+inhib+incl+excl+(1|Name), data=data, family=binomial, na.action=na.omit)
Rel<-glmer(PartyAffiliation~motion+space+time+(1|Name), data=data, family=binomial, na.action=na.omit)
PerCon<-glmer(PartyAffiliation~work+achieve+money+relig+death+(1|Name), data=data, family=binomial, na.action=na.omit)
MLC<-glmer(PartyAffiliation~CogProcess+CatThink+PsychDistance+ComplexThink+(1|Name), data=data, family=binomial, na.action=na.omit)
anova(nullnested1, LinPro)
anova(nullnested1, Pron)
anova(nullnested1, Verb)
anova(Other,nullnested1)
anova(nullnested1, SocEmo)
anova(nullnested1, CogMec)
anova(nullnested1, Rel)
anova(nullnested1, PerCon)
anova(nullnested1, MLC)
summary(Pron)
summary(SocEmo)
summary(Rel)
summary(MLC)