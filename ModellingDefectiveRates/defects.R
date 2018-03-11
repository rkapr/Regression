# Rajan Kapoor
# STAT 608
# Sunday July 10, 2017 10:54 AM
# Regression Model for Modelling defective rates as function of Temperature, Density and Rate
setwd("/Volumes/Box/MyPractice608/ModellingDefectiveRates")
defects <- read.table("defects.txt", header=TRUE)
attach(defects)
pairs(Defective ~ Temperature+Density+Rate)

# Linear Model
# Plot of standardized residuals against predictors and fitted values
m1 <- lm(Defective ~ Temperature+Density+Rate)
par(mfrow=c(2,2))
StanRes1 <- rstandard(m1)
plot(Temperature,StanRes1,ylab="Standardized Residuals")
plot(Density,StanRes1,ylab="Standardized Residuals")
plot(Rate,StanRes1,ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")

# Plot of polynomial fit and linear fit against the data
par(mfrow=c(1,1))
fit1 <- m1$fitted.values
m2 <- lm(Defective~fit1 + I(fit1^2))
plot(fit1,Defective,xlab="Fitted Values")
fitnew <- seq(-15,60,len=76)
lines(fitnew,predict(m2,newdata=data.frame(fit1=fitnew)))
abline(lsfit(m1$fitted.values,Defective),lty=2)

# Finding lambda from inverse response plot
library(alr3)
inverse.response.plot(m1,key=TRUE)

# Box-Cox 95 pc CI
library(MASS)
boxcox(m1,lambda=seq(0.3,0.65,length=20))

# Plot of sqrt(Y) against each predictor
# Should be linear for proper fit
par(mfrow=c(2,2))
plot(Temperature,sqrt(Defective),ylab=expression(sqrt(Defective)))
plot(Density,sqrt(Defective),ylab=expression(sqrt(Defective)))
plot(Rate,sqrt(Defective),ylab=expression(sqrt(Defective)))

# Create linear reg model with sqrt(Y)
mt <- lm(sqrt(Defective) ~ Temperature+Density+Rate)
par(mfrow=c(2,2))
# Plot stand. res against predictors and fitted values
StanRest <- rstandard(mt)
plot(Temperature,StanRest,ylab="Standardized Residuals")
plot(Density,StanRest,ylab="Standardized Residuals")
plot(Rate,StanRest,ylab="Standardized Residuals")
plot(mt$fitted.values,StanRest,ylab="Standardized Residuals",xlab="Fitted Values")
# Plot of fitted values against sqrt(Y): should be linear for correct fit
par(mfrow=c(1,1))
plot(mt$fitted.values,sqrt(Defective),xlab="Fitted Values",ylab=expression(sqrt(Defective)))
abline(lsfit(mt$fitted.values,sqrt(Defective)))

# Diagnostic Plots
par(mfrow=c(2,2))
plot(mt)

summary(mt)

# Added variable plot to assess need of variable rate
library(car)
par(mfrow=c(2,2))
avPlot(mt,variable="Temperature",ask=FALSE)
avPlot(mt,variable="Density",ask=FALSE)
avPlot(mt,variable="Rate",ask=FALSE)

detach(defects)
