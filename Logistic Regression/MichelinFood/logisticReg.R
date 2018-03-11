#Chapter: Logistic Regression
setwd("/Volumes/Box/MyPractice608/Logistic Regression/MichelinFood")

MichelinFood <- read.table("MichelinFood.txt", header=TRUE)
attach(MichelinFood)

#Figure 8.1 on page 266
plot(Food,proportion,ylab="Sample proportion",xlab="Zagat Food Rating")

#R output on page 267
m1 <- glm(cbind(InMichelin,NotInMichelin)~Food,family=binomial)
summary(m1)

# z-value in summary and p-value is for null hypothesis: Food coeff = 0
# Null deviance is "RSS" when beta1 = 0
# Residual Deviance is "RSS" when beta1 != 0
# Pearson deviance is similar to Residual Deviance
print(paste("Pearson's X^2 =",round(sum(residuals(m1,type="pearson")^2),3)))

x <- seq(15,28,0.05)
y <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*x)))
plot(Food,proportion,ylab="Probability of inclusion in the Michelin Guide",xlab="Zagat Food Rating")
lines(x,y)

thetahat <- m1$fitted.values
odds_ratio <- m1$fitted.values/(1-m1$fitted.values)
cbind(Food,round(thetahat,3),round(odds_ratio,3))

#p-value on page 272
# p-value for null hypothesis: Model is appropriate
pchisq(m1$deviance,m1$df.residual,lower=FALSE)

#Value of the difference in devinace and associated p-value on page 273
# p-value for null hypothesis beta1 = 0
m1$null.deviance-m1$deviance
pchisq(m1$null.deviance-m1$deviance,1,lower=FALSE)

###
# Residuals for Logistic regression: Response Res, Pearson Res, Deviance Res
###

cbind(round(residuals(m1,"response"),3),round(residuals(m1,"pearson"),3),round(residuals(m1,"deviance"),3))

# Standardized Pearson and Deviance Residuals
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)

par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-2,2))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-2,2))

detach(MichelinFood)


###
# Binary Logistic Regression
###
MichelinNY <- read.csv("MichelinNY.csv", header=TRUE)
attach(MichelinNY)

y <- InMichelin

boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")

m1 <- glm(y~Food,family=binomial(),data=MichelinNY)
summary(m1)

hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
#Alternatively we could use 
#stanresDeviance < rstandard(m1)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)

par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))


par(mfrow=c(1,1))
xx <- seq(15,28.2,0.05)
yy <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*xx)))
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
     ylab="In Michelin Guide? (0=No, 1=Yes)")
lines(xx,yy)
lines(xx,predict(loessfit1,data.frame(Food=xx)),lty=2)

par(mfrow=c(2,2))
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Decor~y, ylab="Decor Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Service~y, ylab="Service Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Price~y, ylab="Price",xlab="In Michelin Guide? (0=No, 1=Yes)")

### Marginal Model Plots for Binary Data
m2 <- glm(y~Food+Decor+Service+Price+log(Price),family=binomial(),data=MichelinNY)
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
loessfit2 <- loess(m2$fitted.values ~ Food,degree=1,span=2/3)
xx <- seq(15,28.2,0.05)
summary(m2)
par(mfrow=c(1,2))
plot(Food,y,xlab="Food Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx,predict(loessfit1,data.frame(Food=xx)))
plot(Food,m2$fitted.values,ylab=expression(hat(Y)),xlab="Food Rating, x1")
lines(xx,predict(loessfit2,data.frame(Food=xx)))

library(alr3)
mmps(m2,layout=c(2,3))

par(mfrow=c(1,1))
plot(Decor,Service,pch=y+1,col=y+1,xlab="Decor Rating",ylab="Service Rating")
abline(lsfit(Decor[y==0],Service[y==0]),lty=1,col=1)
abline(lsfit(Decor[y==1],Service[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Michelin Guide?")

### MMP does not fit for Price Service and Decor
### Product of Decor and Service variables in predictors
m3 <- glm(y~Food+Decor+Service+Price+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
mmps(m3,layout=c(2,3))
# Observe that deviance gets reduced on adding product terms
anova(m2,m3,test="Chisq")

### MMP still does not fit for Price
## Identify leverage points (> 2*(p+1)/n = 2*7/164 = 0.85)
par(mfrow=c(1,1))
hvalues <- influence(m3)$hat
stanresDeviance <- residuals(m3)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.7))
abline(v=2*7/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

summary(m3)

## Price is not significant; Removing it
m4 <- glm(y~Food+Decor+Service+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
## Check null hypothesis price is not needed
anova(m4,m3,test="Chisq")
## Check if all are statistically significant
summary(m4)

## Check validity using marginal model plots
mmps(m4,layout=c(2,3))

## check validity using standardized res and leverage values
# Cut off at twice average leverage value 2*(P+1)/N = 2*6/164
# REMOVE POINTS WITH S D RES > 2 OR <-2
par(mfrow=c(1,1))
hvalues <- influence(m4)$hat
stanresDeviance <- residuals(m4)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.35))
abline(v=2*6/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

fits4 <- m4$fitted.values
round(fits4[c(14,37,69,133,135,138,160)],3)

detach(MichelinNY)