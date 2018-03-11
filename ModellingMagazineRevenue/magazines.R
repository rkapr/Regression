# Rajan Kapoor
# STAT 608
# Sunday July 10, 2017 11:18 AM
# Regression Model for Modelling Magazine Revenue

setwd("/Volumes/Box/MyPractice608/ModellingMagazineRevenue")
magazines <- read.csv("magazines.csv", header=TRUE)
attach(magazines)

# Using Box-Cox to transform predictor variables towards normality
library(alr3)
summary(powerTransform(cbind(AdPages,SubRevenue,NewsRevenue)~1,data=magazines))

pairs(AdRevenue~AdPages+SubRevenue+NewsRevenue)
# Now inverse response plot can be used to find g inverse
tAdPages<- log(AdPages)
tSubRevenue <- log(SubRevenue)
tNewsRevenue <- log(NewsRevenue)
m1 <- lm(AdRevenue~log(AdPages)+log(SubRevenue)+log(NewsRevenue))
library(alr3)
par(mfrow=c(1,1))
inverseResponsePlot(m1,key=TRUE)

# Using Box-Cox for simultaneous transformation
library(alr3)
summary(powerTransform(cbind(AdRevenue,AdPages,SubRevenue,NewsRevenue)~1,data=magazines))

pairs(log(AdRevenue)~log(AdPages)+log(SubRevenue)+log(NewsRevenue))

# Standardized res plots
m2 <- lm(log(AdRevenue)~log(AdPages)+log(SubRevenue)+log(NewsRevenue))
par(mfrow=c(2,2))
StanRes2 <- rstandard(m2)
plot(log(AdPages),StanRes2,ylab="Standardized Residuals")
plot(log(SubRevenue),StanRes2,ylab="Standardized Residuals")
plot(log(NewsRevenue),StanRes2,ylab="Standardized Residuals")
plot(m2$fitted.values,StanRes2,ylab="Standardized Residuals",xlab="Fitted Values")

# fitted values against transformed response variable
par(mfrow=c(1,1))
plot(m2$fitted.values,log(AdRevenue),xlab="Fitted Values")
abline(lsfit(m2$fitted.values,log(AdRevenue)))

# Diagnostic plot
par(mfrow=c(2,2))
plot(m2)

# Added variable plot
library(car)
par(mfrow=c(2,2))
avPlot(m2,variable="log(AdPages)",ask=FALSE)
avPlot(m2,variable="log(SubRevenue)",ask=FALSE)
avPlot(m2,variable="log(NewsRevenue)",ask=FALSE)

detach(magazines)
