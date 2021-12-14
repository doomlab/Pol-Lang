library(lmerTest);library (car); library (mlogit);library(lme4);library(nlme);library(psych);library(Matrix);library(lattice);library (memisc);library(ggplot2);library(lmtest);library(gvlma)

setwd("C:/Users/Kayla/OneDrive/projects/Political Language/Data")

data=read.csv("CongressDataMC_PA_Fixed.csv", header=TRUE) 
options(scipen=999)



###Models collapsing region to no war and war

###Create interaction to calculate effect size for interaction
interaction<-(data$PartyAffiliation*data$War)

###Cognitive Processing Models
nullnested2<-lmer(CogProcess~(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
cogProcess<-lmer(CogProcess~PartyAffiliation*War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
cogProcessPA<-lmer(CogProcess~PartyAffiliation+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
cogProcessW<-lmer(CogProcess~War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
cogProcessI<-lmer(CogProcess~interaction+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(nullnested2)
summary(cogProcess)
summary(cogProcessPA)
summary(cogProcessW)
summary(cogProcessI)
confint(cogProcess,method="profile") 

### Categorical Thinking Models
nullnested2<-lmer(CatThink~(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
catThink<-lmer(CatThink~PartyAffiliation*War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
catThinkPA<-lmer(CatThink~PartyAffiliation+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
catThinkW<-lmer(CatThink~War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
catThinkI<-lmer(CatThink~interaction+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(nullnested2)
summary(catThink)
summary(catThinkPA)
summary(catThinkW)
summary(catThinkI)
confint(catThink, method="profile")

### Complex Thinking model
nullnested2<-lmer(ComplexThink~(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
complexThink<-lmer(ComplexThink~PartyAffiliation*War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
complexThinkPA<-lmer(ComplexThink~PartyAffiliation+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
complexThinkW<-lmer(ComplexThink~War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
complexThinkI<-lmer(ComplexThink~interaction+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(nullnested2)
summary(complexThink)
summary(complexThinkPA)
summary(complexThinkW)
summary(complexThinkI)
confint(complexThink, method="profile")

###Psychological Distancing models
nullnested2<-lmer(PsychDistance~(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
psychDistance<-lmer(PsychDistance~PartyAffiliation*War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
psychDistancePA<-lmer(PsychDistance~PartyAffiliation+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
psychDistanceW<-lmer(PsychDistance~War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
psychDistanceI<-lmer(PsychDistance~interaction+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(nullnested2)
summary(psychDistance)
summary(psychDistancePA)
summary(psychDistanceW)
summary(psychDistanceI)
confint(psychDistance, method="profile")

### Honesty models
honesty<-lmer(Honesty~PartyAffiliation*War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(honesty)
confint(honesty, method="profile")

### Post-hoc Tests 2 x 2 models

###Create subsets for predictor variables
Republican<-subset(data, PartyAffiliation==0)
Democrat<-subset(data, PartyAffiliation==1)

NoWar<-subset(data, War==0)
War<-subset(data, War==1)

###Post hoc Cognitive Processing by Party Affiliation
phCogProcessR<-lmer(CogProcess~War+(1|Name)+(1|Venue), data=Republican, REML=FALSE, na.action=na.omit)
summary(phCogProcessR)
confint(phCogProcessR)

phCogProcessD<-lmer(CogProcess~War+(1|Name)+(1|Venue), data=Democrat, REML=FALSE, na.action=na.omit)
summary(phCogProcessD)
confint(phCogProcessD)

###Post hoc Cognitive Processing by War
phCogProcessNW<-lmer(CogProcess~PartyAffiliation+(1|Name)+(1|Venue), data=NoWar, REML=FALSE, na.action=na.omit)
summary(phCogProcessNW)
confint(phCogProcessNW)

phCogProcessW<-lmer(CogProcess~PartyAffiliation+(1|Name)+(1|Venue), data=War, REML=FALSE, na.action=na.omit)
summary(phCogProcessW)
confint(phCogProcessW)

###Post hoc Complex Thinking by Party Affiliation
phComplexThinkR<-lmer(ComplexThink~War+(1|Name)+(1|Venue), data=Republican, REML=FALSE, na.action=na.omit)
summary(phComplexThinkR)
confint(phComplexThinkR)

phComplexThinkD<-lmer(ComplexThink~War+(1|Name)+(1|Venue), data=Democrat, REML=FALSE, na.action=na.omit)
summary(phComplexThinkD)
confint(phComplexThinkD)

###Post hoc Complex Thinking by War
phComplexThinkNW<-lmer(ComplexThink~PartyAffiliation+(1|Name)+(1|Venue), data=NoWar, REML=FALSE, na.action=na.omit)
summary(phComplexThinkNW)
confint(phComplexThinkNW)

phComplexThinkW<-lmer(ComplexThink~PartyAffiliation+(1|Name)+(1|Venue), data=War, REML=FALSE, na.action=na.omit)
summary(phComplexThinkW)
confint(phComplexThinkW)

##### 2 (Party) x 2 (War) Graphs

barCogProcessing<-ggplot(data, aes(factor(War),CogProcess, fill=factor(PartyAffiliation)))
barCogProcessing+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Cognitive Processing", fill="PartyAffiliation")

barCogProcessing<-ggplot(data, aes(factor(War),CogProcess))
barCogProcessing+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Cognitive Processing")

barCatThinking<-ggplot(data, aes(factor(War), CatThink, fill=factor(PartyAffiliation)))
barCatThinking+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Categorical Thinking", fill="PartyAffiliation")

barCatThinking<-ggplot(data, aes(factor(War), CatThink))
barCatThinking+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Categorical Thinking")

barCatThinking<-ggplot(data, aes(factor(PartyAffiliation), CatThink))
barCatThinking+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="Party", y="Categorical Thinking")

barComplexThinking<-ggplot(data, aes(factor(War),ComplexThink, fill=factor(PartyAffiliation)))
barComplexThinking+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Complex Thinking", fill="PartyAffiliation")

barPsychDistancing<-ggplot(data, aes(factor(War),PsychDistance, fill=factor(PartyAffiliation)))
barPsychDistancing+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Psychological Distancing", fill="PartyAffiliation")

barPsychDistancing<-ggplot(data, aes(factor(PartyAffiliation),PsychDistance))
barPsychDistancing+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Psychological Distancing", fill="PartyAffiliation")

barPsychDistancing<-ggplot(data, aes(factor(War),PsychDistance))
barPsychDistancing+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Psychological Distancing")

barPsychDistancing<-ggplot(data, aes(factor(Venue),PsychDistance, fill=factor(War)))
barPsychDistancing+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="Venue", y="Psychological Distancing", fill="War")

barHonesty<-ggplot(data, aes(factor(War),Honesty, fill=factor(PartyAffiliation)))
barHonesty+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="War", y="Honesty", fill="PartyAffiliation")

barHonesty<-ggplot(data, aes(factor(PartyAffiliation),Honesty))
barHonesty+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="Party", y="Honesty")



### Extra shit that didn't work
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

###Converting variables into factors
Region=factor(data$Region)
levels(Region)
War=factor(data$War)
levels(War)

###Metalinguistic Constructs with all regions

###Cognitive Processing
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
           Libya+(1|Name)+(1|Venue), data=data, REML=FALSE,na.action=na.omit)
summary(cp)
confint(cp,method="profile") 

###Categorical Thinking
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
           Libya+(1|Name)+(1|Venue), data=data, REML=FALSE,na.action=na.omit)
summary(ct)
confint(ct,method="profile") 

##Complex Thinking
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
          *Libya+(1|Name)+(1|Venue), data=data, REML=FALSE,na.action=na.omit)
summary(cmt)
confint(cmt,method="profile") 

###Psychological Distancing
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
confint(pd,method="profile") 

### Honesty
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
           Libya+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(ht)
confint(ht,method="profile") 

####Post hoc
Republican<-subset(data, PartyAffiliation==0)
Democrat<-subset(data, PartyAffiliation==1)

### Kosovo-Complex Thinking
phComplexThinkR<-lmer(ComplexThink~Kosovo + (1|Name)+(1|Venue), data=Republican, REML=FALSE, na.action=na.omit) 
summary(phComplexThinkR)
confint(phComplexThinkR,method="profile")

phComplexThinkD<-lmer(ComplexThink~Kosovo + (1|Name)+(1|Venue), data=Democrat, REML=FALSE, na.action=na.omit) 
summary(phComplexThinkD)
confint(phComplexThinkD,method="profile")

### Sudan-Complex Thinking
phComplexThinkR<-lmer(ComplexThink~Sudan + (1|Name)+(1|Venue), data=Republican, REML=FALSE, na.action=na.omit) 
summary(phComplexThinkR)
confint(phComplexThinkR,method="profile")

phComplexThinkD<-lmer(ComplexThink~Sudan + (1|Name)+(1|Venue), data=Democrat, REML=FALSE, na.action=na.omit) 
summary(phComplexThinkD)
confint(phComplexThinkD,method="profile")

### Libya-Complex Thinking
phComplexThinkR<-lmer(ComplexThink~Libya + (1|Name)+(1|Venue), data=Republican, REML=FALSE, na.action=na.omit) 
summary(phComplexThinkR)
confint(phComplexThinkR,method="profile")

phComplexThinkD<-lmer(ComplexThink~Libya + (1|Name)+(1|Venue), data=Democrat, REML=FALSE, na.action=na.omit) 
summary(phComplexThinkD)
confint(phComplexThinkD,method="profile")

###Charts - Metalinguistic Constructs
barPsychDistance<-ggplot(data, aes(factor(Region),PsychDistance, fill=factor(PartyAffiliation)))
barPsychDistance+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),width=0.2)+labs(x="Region", y="PsychDistance", fill="PartyAffiliation")

barHonesty<-ggplot(data, aes(factor(Region),Honesty, fill=factor(PartyAffiliation)))
barHonesty+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary(fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90), width=0.2)+labs(x="Region", y="Honesty", fill="PartyAffiliation")

barComplexThink<-ggplot(data, aes(factor(Region),ComplexThink, fill=factor(PartyAffiliation)))
barComplexThink+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90), width=0.2)+labs(x="Region", y="ComplexThink", fill="PartyAffiliation")

barCatThink<-ggplot(data, aes(factor(Region),CatThink, fill=factor(PartyAffiliation)))
barCatThink+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90), width=0.2)+labs(x="Region", y="CatThink", fill="PartyAffiliation")

barCogProcess<-ggplot(data, aes(factor(Region),CogProcess, fill=factor(PartyAffiliation)))
barCogProcess+stat_summary(fun.y=mean,geom="bar", position="dodge")+stat_summary (fun.data=mean_cl_boot,geom="errorbar",position=position_dodge(width=0.90),  width=0.2)+labs(x="Region", y="Cognitive Processing", fill="Party Affiliation")

####Testing Stuff
fun<-lmer(PsychDistance~War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(fun)
attach(data)
plot(War,PsychDistance)

psychDistance<-lmer(PsychDistance~PartyAffiliation*War+(1|Name), data=data, REML=FALSE, na.action=na.omit)
summary(psychDistance)

psychDistance<-lmer(PsychDistance~PartyAffiliation+War*Venue+(1|Name), data=data, REML=FALSE, na.action=na.omit)
summary(psychDistance)

psychDistance<-lmer(PsychDistance~PartyAffiliation*War+(Venue|Name), data=data, REML=FALSE, na.action=na.omit)
summary(psychDistance)
confint(psychDistance, method="profile")

Senate<-subset (data, Venue==1)
House<-subset(data, Venue==2)
HouseHearing<-subset(data, Venue==3)
SenateHearing<-subset(data, Venue==4)
President<-subset(data, Venue==5)

describe(Senate$PsychDistance)
describe(House$PsychDistance)
describe(SenateHearing$PsychDistance)
describe(HouseHearing$PsychDistance)
describe(President$PsychDistance)

tapply(War$PsychDistance, War$Venue, mean)
tapply(NoWar$PsychDistance, NoWar$Venue, mean)
tapply(data$PsychDistance, data$War, mean)
summary(NoWar$PsychDistance)
summary(War$PsychDistance)
sd(NoWar$PsychDistance)
sd(War$PsychDistance)

PsychDistance2<-(data$PsychDistance+100)
psychDistancePos<-lmer(PsychDistance2~War+(1|Name)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(psychDistancePos)
tapply(PsychDistance2, data$War, mean)


psychDistanceS<-lmer(PsychDistance~War+(1|Name), data=Senate, REML=FALSE, na.action=na.omit)
summary(psychDistanceS)
confint(psychDistanceS, method="profile")

psychDistanceH<-lmer(PsychDistance~War+(1|Name), data=House, REML=FALSE, na.action=na.omit)
summary(psychDistanceH)
confint(psychDistanceH, method="profile")

psychDistanceHH<-lmer(PsychDistance~War+(1|Name), data=HouseHearing, REML=FALSE, na.action=na.omit)
summary(psychDistanceHH)
confint(psychDistanceHH, method="profile")

psychDistanceSH<-lmer(PsychDistance~War+(1|Name), data=SenateHearing, REML=FALSE, na.action=na.omit)
summary(psychDistanceSH)
confint(psychDistanceSH, method="profile")

psychDistanceP<-lmer(PsychDistance~War+(1|Name), data=President, REML=FALSE, na.action=na.omit)
summary(psychDistanceP)
confint(psychDistanceP, method="profile")


psychDistance<-lm(PsychDistance~PartyAffiliation*War, data=data, na.action=na.omit)
summary(psychDistance)

psychDistance<-lmer(PsychDistance~PartyAffiliation*War+(1|Name)+(PsychDistance|Venue)+(1|Venue), data=data, REML=FALSE, na.action=na.omit)
summary(psychDistance)

