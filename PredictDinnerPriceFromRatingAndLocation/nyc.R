# Rajan Kapoor
# STAT 608
# Sunday July 9, 2017 8:12 PM
# Regression Model for Predicting Dinner Price at Italian Restaurant from Consumer Ratings of Food,
# Decor and Service in the Area and location: East or West of Fifth Avenue
# Assess Impact of Service Ratings

setwd("/Volumes/Box/MyPractice608/PredictDinnerPriceFromRatingAndLocation")
# Model with all variables
nyc <- read.csv("nyc.csv", header=TRUE)
attach(nyc)

m1 <- lm(Price~Food+Decor+Service+East)
summary(m1)

# Observations
# 1. The variable Décor has the largest effect on Price since its regression coefficient
#    is largest. Note that Food, Décor and Service are each measured on the 
#    same 0 to 30 scale and so it is meaningful to compare regression coefficients.
#    The variable Décor is also the most statistically significant since its p -value is
#    the smallest of the three.
# 2. In order that the price achieved for dinner is maximized, the new restaurant
#    should be on the east of Fifth Avenue since the coefficient of the dummy variable
#    is statistically significantly larger than 0.
# 3. It does not seem possible to achieve a price premium for “setting a new
#    standard for high quality service in Manhattan” for Italian restaurants since
#    the regression coefficient of Service is not statistically significantly greater
#    than zero.

# Removing Service variable
#Regression output on page 139
m2 <- lm(Price~Food+Decor+East)
summary(m2)
#An alterntive way to obtain m2 is to use the update command
m2 <- update(m1,~.-Service)
summary(m2)

# Modelling East as dummy variable instead of continuous one
# Assessing the impact of Service
# Full ``Unrelated regression lines Model"
mfull <- lm(Price~Food+Decor+Service+East+Food:East+Decor:East+Service:East)
summary(mfull)

#Regression output on page 146
mreduced <- lm(Price~Food+Decor+East)
summary(mreduced)

#Regression output on page 146
anova(mreduced,mfull)

# p-value > 0.05 null hypothesis cannot be rejected.
# accept reduced model
# Partial-F test hand calc
# df_red - df_full = var_full - var_red = (7+1) - (3+1) = 4
# 7 and 3 are seen from F-statistic in summary of two models
#### ?? Not Working ??
Num <- (5366.5-5222.2)/(164-160.0);
Den <- 5222.2/160.0;
F_stat <- Num/Den;
2*pf(F_stat, df1=164, df2=160,lower.tail = F)
detach(nyc)
