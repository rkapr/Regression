setwd("H:/R/STAT608")

# Read csv or text file, header = TRUE
input_data <- read.table("airfares.txt",header = TRUE)
#election <- read.csv()

summary(input_data)

attach(input_data)

my.lm <- lm(y ~ x)

summary(my.lm)
plot(m1)
plot(CouponRate, BidPrice, xlab="Coupon Rate", ylab="Bid Price", pch=19)

abline(my.lm) 
identify(CouponRate, BidPrice,Case) 

plot(x,my.lm$residuals)

#Leverage values 
lm.influence(my.lm)$hat

# Normality of x and y

par(mfrow=c(3,2))
plot(density(y,bw="SJ",kern="gaussian"),type="l", main="Gaussian kernel density estimate",xlab="y")
rug(y)
boxplot(y,ylab="Y")
qqnorm(y, ylab = "Y")
qqline(y, lty = 2, col=2)
sj <- bw.SJ(x,lower = 0.05, upper = 100)
plot(density(x,bw=sj,kern="gaussian"),type="l",main="Gaussian kernel density estimate",xlab="x")
rug(x)
boxplot(x,ylab="x")
qqnorm(x, ylab = "x")
qqline(x, lty = 2, col=2)

#95% confidence intervals of slope
round(confint(m1,level=0.95),3)

#Leverage values, residuals and standard residuals
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
residual1 <- m1$residuals

# Update model with removed data
m2 <- update(m1, subset=(1:35)[-c(4,13,35)])
summary(m2)
plot(CouponRate[-c(4,13,35)],BidPrice[-c(4,13,35)])

# Std res plot 
StanRes2 <- rstandard(m2)
plot(CouponRate[-c(4,13,35)],StanRes2,xlab="Coupon Rate (%)", ylab="Standardized Residuals",xlim=c(2,14),main="Regular Bonds")
abline(h=2,lty=2)
abline(h=-2,lty=2)

# Cooks distance
cd1 <- cooks.distance(m1)
plot(CouponRate,cd1,xlab="Coupon Rate (%)", ylab="Cook's Distance")
abline(h=4/(35-2),lty=2)
identify(CouponRate,cd1,Case)
#Just for kicks, showing off the method of creating the "New" variables; gives same result as above:
#Multiply by sqrt(wi):
ynew <- sorted$Rooms/sorted$StdDev
x2new <- sorted$Crews/sorted$StdDev
x1new <- w.lm$x[,1]/sorted$StdDev  #The first column of the design matrix: a 1-vector.

w.lm2 <- lm(ynew ~ x1new + x2new - 1, x=TRUE)
