# Rajan Kapoor
# STAT 608
# Sunday July 10, 2017 11:43 AM
# Marginal Model Plots
# Regression Model for Modelling defective rates as function of Temperature, Density and Rate
setwd("/Volumes/Box/MyPractice608/MarginalModelPlots")
defects <- read.table("defects.txt", header=TRUE)
attach(defects)

m1 <- lm(Defective ~ Temperature+Density+Rate)
loessfit1 <- loess(Defective ~ Temperature,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Temperature,degree=1,span=2/3)
xx <- seq(min(Temperature),max(Temperature),length=100)
par(mfrow=c(1,2))
plot(Temperature,Defective,xlab="Temperature, x1", ylab="Defective, Y")
lines(xx,predict(loessfit1,data.frame(Temperature=xx)))
plot(Temperature,m1$fitted.values,ylab=expression(hat(Y)),xlab="Temperature, x1")
lines(xx,predict(loessfit2,data.frame(Temperature=xx)))

library(alr3)
par(mfrow=c(1,1))
mmp(m1,Temperature)

par(mfrow=c(2,2))
mmp(m1,Temperature)
mmp(m1,Density)
mmp(m1,Rate)
mmp(m1,m1$fitted.values,xlab="Fitted Values")

# don't overlap => not good fit
#MMP for transformed fir
m2 <- lm(sqrt(Defective) ~ Temperature+Density+Rate)
par(mfrow=c(2,2))
mmp(m2,Temperature)
mmp(m2,Density,key="topright")
mmp(m2,Rate)
mmp(m2,m2$fitted.values,xlab="Fitted Values")

detach(defects)