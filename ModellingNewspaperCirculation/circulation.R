# Rajan Kapoor
# STAT 608
# Sunday July 10, 2017 11:49 AM
# Regression Model for Modelling Newspaper Sunday circulation as function of Weekday circulation
# and presence of Tabloid newspaper with Serious newspaper
# Predict sunday circulation of newspaper with weekday circulation of 210,000
setwd("/Volumes/Box/MyPractice608/ModellingNewspaperCirculation")
circulation <- read.table("circulation.txt", header=TRUE, sep="\t")
attach(circulation)

# Scatter plot of data
par(mfrow=c(1,1))
plot(log(Weekday),log(Sunday),xlab="log(Weekday Circulation)",ylab="log(Sunday Circulation)",
     pch=Tabloid_with_a_Serious_Competitor+1,col=Tabloid_with_a_Serious_Competitor+1)
legend(11.6, 14.1,legend=c("0","1"),pch=1:2,col=1:2,title="Tabloid dummy variable")

# Plot of stand res
m1 <- lm(log(Sunday) ~ log(Weekday) + Tabloid_with_a_Serious_Competitor)
par(mfrow=c(2,2))
StanRes1 <- rstandard(m1)
plot(log(Weekday),StanRes1,ylab="Standardized Residuals",xlab="log(Sunday Circulation)")
plot(Tabloid_with_a_Serious_Competitor,StanRes1,ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")

# Fiited vales against y
par(mfrow=c(1,1))
plot(m1$fitted.values,log(Sunday),xlab="Fitted Values",ylab="log(Sunday Circulation)")
abline(lsfit(m1$fitted.values,log(Sunday)))

# Diagnostics
par(mfrow=c(2,2))
plot(m1)
abline(v=2*3/89,lty=2)

summary(m1)

# Predict here
predict(m1,newdata=data.frame(
  Weekday=c(210000),Tabloid_with_a_Serious_Competitor=c(1)),interval="prediction",level=0.95)
predict(m1,newdata=data.frame(
  Weekday=c(210000),Tabloid_with_a_Serious_Competitor=c(0)),interval="prediction",level=0.95)

# Added Variable Plot
library(car)
par(mfrow=c(1,2))
avPlot(m1,variable="log(Weekday)",ask=FALSE)
avPlot(m1,variable="Tabloid_with_a_Serious_Competitor",ask=FALSE)

detach(circulation)