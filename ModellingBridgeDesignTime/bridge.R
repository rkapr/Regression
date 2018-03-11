# Rajan Kapoor
# STAT 608
# Sunday July 10, 2017 11:43 AM
# Marginal Model Plots
# Regression Model for Modelling Design time of bridge in person days as function of
# Deck area of bridge (000 sq ft)
# CCost = Construction cost 
# Dwgs = Number of structural drawings
# Length = Length of bridge (ft)
# Spans = Number of spans

setwd("/Volumes/Box/MyPractice608/ModellingBridgeDesignTime")
bridge <- read.table("bridge.txt", header=TRUE)
attach(bridge)

library(alr3)
summary(powerTransform(cbind(Time,DArea,CCost,Dwgs,Length,Spans)~1,data=bridge))

m1 <- lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))

# Standard Res
StanRes1 <- rstandard(m1)
par(mfrow=c(2,3))
plot(log(DArea),StanRes1, ylab="Standardized Residuals")
plot(log(CCost),StanRes1, ylab="Standardized Residuals")
plot(log(Dwgs),StanRes1, ylab="Standardized Residuals")
plot(log(Length),StanRes1, ylab="Standardized Residuals")
plot(log(Spans),StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted values")

# Correlation
logDArea <- log(DArea)
logCCost <- log(CCost)
logDwgs <- log(Dwgs)
logLength <- log(Length)
logSpans <- log(Spans)
X <- cbind(logDArea,logCCost,logDwgs,logLength,logSpans)
c <- cor(X)
round(c,3)

# variance inflation factor
library(car)
vif(m1)

detach(bridge)