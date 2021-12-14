bigdata <- read.csv("C:/Users/DOOM-Lab/OneDrive/projects/Political Language/Thesis/Thesis-Metalinguistic Prelim Data.csv")
//mac version
bigdata <- read.csv("~/OneDrive/projects/Political Language/Thesis/Thesis-Metalinguistic Prelim Data.csv")
attach(bigdata)

library(faraway)
library(sm)

plot(CogProcess~Dateno, bigdata, main="Cognitive Processing", xlab="Date", 
     ylab="Zscore of Cognitive Processing", pch="`")
lines(ksmooth(Dateno, CogProcess, "normal", 197.39), col="red")
lines(ksmooth(Dateno, CogProcess, "normal", 301.45), col="blue")

data_R = subset(bigdata, PartyAffiliation=="R")
hm <- hcv(data_R$Dateno, data_R$CogProcess, display="lines")
sm.regression(data_R$Dateno, data_R$CogProcess, h=hm, xlab="Date", ylab="Cognitive Processing")
hm


data_D = subset(bigdata, PartyAffiliation=="D")
hm <- hcv(data_D$Dateno, data_D$CogProcess, display="lines")
sm.regression(data_D$Dateno, data_D$CogProcess, h=hm, xlab="Date", ylab="Cognitive Processing")
hm


plot(CogProcess~Dateno, bigdata, main="Cognitive Processing", xlab="Date", 
     ylab="Zscore of Cognitive Processing", pch="`")
lines(smooth.spline(data_R$Dateno, data_R$CogProcess), col="red")
lines(smooth.spline(data_D$Dateno, data_D$CogProcess), col="blue")