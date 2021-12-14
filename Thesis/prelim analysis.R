library(faraway)

data=read.csv("C:/Users/Kayla/OneDrive/projects/Political Language/Thesis/Thesis-MetaLinguistic Prelim Data.csv", header=TRUE) 
data$Date
data(exa)
date=as.Date(data$Date)
class(date)
length(date)
cbind(data, date)
date=as.Date(data$Date)
iraq<-subset(data, Region=="Iraq")
plot(CatThink~date, data, main="Categorical Thinking", pch="*", na.action=na.omit)
lines(CatThink~date, data)
lines(ksmooth(data$date, data$CatThink, "normal", 1))
hm <- hcv(data$date, data$CatThink, display="lines")
sm.regression(data$date, data$date, h=hm, xlab="x", ylab="y")

data(exb)
plot(y~x, exb, main="Example B", pch=".")
lines(m~x, exb)

data(faithful)
plot(waiting~duration, faithful, main="Old Faithful", pch=".")

##Kernel estimators
##moving average estimator

#uniform kernel - should be jumpy/rough
plot(waiting~eruptions, faithful, main="bandwidth = .1", pch=".")
lines(ksmooth(faithful$eruptions, faithful$waiting, "normal", 0.1))

plot(waiting~duration, faithful, main="bandwidth = .5", pch=".")
lines(ksmooth(faithful$duration, faithful$waiting, "normal", 0.5))


plot(waiting~duration, faithful, main="bandwidth = 2", pch=".")
lines(ksmooth(faithful$duration, faithful$waiting, "normal", 2))

library(sm)
hm <- hcv(faithful$duration, faithful$waiting, display="lines")
sm.regression(faithful$duration, faithful$waiting, h=hm, xlab="duration", ylab="waiting")

hm <- hcv(exa$x, exa$y, display="lines")
sm.regression(exa$x, exa$y, h=hm, xlab="x", ylab="y")

hm <- hcv(exb$x, exb$y, display="lines")
sm.regression(exb$x, exb$y, h=hm, xlab="x", ylab="y")

hm <- hcv(exb$x, exb$y, display="lines",hstart=0.005)
sm.regression(exb$x, exb$y, h=0.005, xlab="x", ylab="y")

plot(waiting~duration, faithful, pch=".")
lines(smooth.spline(faithful$duration, faithful$waiting))

plot(y~x, exa, pch=".")
lines(exa$x, exa$m)
lines(smooth.spline(exa$x, exa$y), lty=2)

plot(y~x, exb, pch=".")
lines(exb$x, exb$m)
lines(smooth.spline(exb$x, exb$y), lty=2)

rhs <-function(x,c) ifelse(x>c, x-c, 0)
curve(rhs(x,0.5), 0, 1)
knots <-0:9/10
knots

dm <-outer (exa$x, knots, rhs)
matplot(exa$x, dm, type="l", col=1)

g <-lm(exa$y~dm)
plot(y~x, exa, pch=".", xlab="x", ylab="y")
lines(exa$x, predict(g))

newknots <- c(0, 0.5, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
dmn <-outer(exa$x, newknots, rhs)
gn <- lm(exa$y~dmn)
plot(y~x, exa, pch=".", xlab="x", ylab="y")
lines(exa$x, predict(gn))

library(splines)
matplot(bs(seq(0,1,length=1000),df=12),type="l", ylab="", col=1)

sml <-lm(y~bs(x,12),exa)
plot(y~x,exa,pch=".")
lines(m~x,exa)
lines(predict(sml)~x, exa, lty=2)

plot(waiting~duration, faithful, pch=".")
f <-loess(waiting~duration, faithful)
i<-order(faithful$duration)
lines(f$x[i], f$fitted[i])

plot(y~x, exa, pch=".")
lines(exa$a, exa$m, lty=1)
f<-loess(y~x, exa)
lines(f$x, f$fitted, lty=2)
f <- loess(y~x, exa, span=0.22)
lines(f$x, f$fitted, lty=5)

plot(y~x, exb, pch=".")
f<-loess(y~x, exb)
lines(f$x, f$fitted, lty=2)
f <- loess(y~x, exa, span=1)
lines(f$x, f$fitted, lty=5)
lines(exb$x, exb$m)

library(wavethresh)
wds<- wd (exa$y, filter, number = 1)
draw(wds, main="")
plot(wds, main="")

wtd <-threshold(wds, policy="manual", value=999)
fd <- wr(wtd)

plot(y~x, exa, pch=".")
lines(m~x, exa)
lines(fd~x, exa, lty=5, lwd=2)

wtd <-threshold(wds)
fd2 <-wr(wtd2)

plot(y~x, exa, pch=".")
lines(m~x, exa)
lines(fd2~x, exa, lty=5, lwd=2)

wds <-wd(exa$y)
draw(wds, main="")
plot(wds, main="")
wtd <-threshold(wds)
fd <- wr(wtd)
plot(y~x, exa, pch=".")
lines(m~x, exa)
lines(fd~x, exa, lty=2)

data(savings)
y <-savings$sr
x <- cbind(savings$pop15, savings$ddpi)
sm.regression(x,y, h=c(1,1), xlab="pop15", ylab="growth", zlab="savings rate")
sm.regression(x,y, h=c(5,5), xlab="pop15", ylab="growth", zlab="savings rate")



