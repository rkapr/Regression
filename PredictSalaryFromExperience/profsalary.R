# Rajan Kapoor
# STAT 608
# Sunday July 9, 2017 6:54 PM
# Regression Model for Predicting Salary from Years of Experience
setwd("/Volumes/Box/MyPractice608/PredictSalaryFromExperience")
profsalary <- read.table("profsalary.txt", header=TRUE)
attach(profsalary)
# Plot Data
plot(Experience,Salary,xlab="Years of Experience")

# Linear Model : Plot Standardized Residuals
m1 <- lm(Salary~Experience)
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
plot(Experience,StanRes1,xlab="Years of Experience", ylab="Standardized Residuals")

# Polynomial Model
# A. Plot predicted curve on actual values scatter plot
m2 <- lm(Salary~Experience + I(Experience^2))
plot(Experience,Salary,xlab="Years of Experience")
ExperienceNew <- seq(0,37,len=37)
lines(ExperienceNew,predict(m2,newdata=data.frame(Experience=ExperienceNew)))
# B. Plot Standardized Residuals
StanRes2 <- rstandard(m2)
plot(Experience,StanRes2,xlab="Years of Experience", ylab="Standardized Residuals")
# C. Plot Leverage against x
#    Leverage cutoff = 6/n = 6/143
leverage2 <- hatvalues(m2)
plot(Experience,leverage2,xlab="Years of Experience",ylab="Leverage")
abline(h=6/max(Case),lty=2)
# D. Diagnostic Curves
par(mfrow=c(2,2))
plot(m2)
# E. Predict Salary for 10 years of experience
summary(m2)
predict(m2,newdata=data.frame(Experience=c(10)),interval="prediction",level=0.95)


detach(profsalary)

