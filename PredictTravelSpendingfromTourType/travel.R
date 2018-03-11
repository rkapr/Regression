# Rajan Kapoor
# STAT 608
# Sunday July 9, 2017 7:16 PM
# Regression Model for Predicting Amount spent on travel from type of tour and age of the person
setwd("/Volumes/Box/MyPractice608/PredictTravelSpendingfromTourType")
# Dummy variable C = 1 if tour type is 'Cultural' and C = 0 if it is an Adventure tour
travel <- read.table("travel.txt",header=TRUE)
attach(travel)

# Linear Model: beta0 + beta1*x if C = 0
#               (beta0 + beta2) + (beta1 + beta3)*x if C = 1
mfull <- lm(Amount~Age+C+C:Age)
summary(mfull)
# Predict from tour type and age from there results
# C = 0: 1814.5445 - 20.3175* Age 
# C = 1: (1814.5445-1821.2337) + (-20.3175 + 40.4461)* Age

# Test the hypothesis that amount spent is not dependent on Tour type i.e
# whether C = 0 or C = 1.
# H0 := beta2 = beta3 = 0
# Fit under reduced model beta0 + beta1*x
mreduced <- lm(Amount~Age)
summary(mreduced)
# Partial F-test
anova(mreduced,mfull)
# Hand Calculation for Partial F-test
Num <- (52158945-2089377)/(923-921)
Den <- 2089377/921
F_stat <- Num/Den
# P-value of this F-statistic; if small reject null hypothesis H0 and beta2 and beta3 are
# statistically significant
# check P(f < -Num/Den) + P(f > Num/Den)
# lower.tail is TRUE is prob are P(X<=x); TRUE by default
2*pf(F_stat, df1=923, df2=921,lower.tail = F)
# Or to check at 95 pc level
qf(0.05/2,923,921)
# F-stat more than qf => reject H0 ie reduced
# Or 2*(1 - pf(F_stat, df1=923, df2=921))
# Or 2*pf(-F_stat, df1=923, df2=921)
# ?pf for help in command window

detach(travel)