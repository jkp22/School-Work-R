## Lab 3 REGRESSION

# Advertising data set
Ad=read.csv("Advertising.csv")
fix(Ad)
Ad=Ad[,-1]
attach(Ad)
detach(Ad)
names(Ad)

# Simple Regression
ad.lmfit=lm(sales~TV)
summary(ad.lmfit)
plot(TV, sales)
abline(ad.lmfit)
names(ad.lmfit)
ad.lmfit$coefficients
abline(a=ad.lmfit$coefficients[1],b=ad.lmfit$coefficients[2],lwd=2,col=2)

# Multiple Regression
ad.lmfit2=lm(sales~.,data=Ad)
summary(ad.lmfit2)
cor(Ad)
plot(Ad)

ad.lmfit3=lm(sales~.-newspaper,data=Ad)
summary(ad.lmfit3)
confint(ad.lmfit3)
confint(ad.lmfit3, level=.99)
predict(ad.lmfit3, interval="confidence")
predict(ad.lmfit3, newdata=data.frame(TV=100, radio=20, newspaper=0),interval="confidence")
predict(ad.lmfit3, newdata=data.frame(TV=100, radio=20, newspaper=0),interval="prediction")

ad.lmfit4=lm(sales~TV+radio)
predict(ad.lmfit4, data.frame(TV=100, radio=20),interval="confidence")
predict(ad.lmfit4, data.frame(TV=100, radio=20),interval="prediction")

# Credit data set
Credit=read.csv("Credit.csv")
attach(Credit)
summary(Credit)

# qualitative variables
credit.lmfit=lm(Balance~Region)
summary(credit.lmfit)
contrasts(Region)
contr.treatment(3)
contr.treatment(3, base=3)
contrasts(Region)=contr.treatment(3,base=3)
contr.sum(3)
contrasts(Region)=contr.sum(3)
contrasts(Region)
credit.lmfit2=lm(Balance~Region)
summary(credit.lmfit2)
credit.lmfit3=lm(Balance~Credit$Region)
summary(credit.lmfit3)

# interaction terms
ad.lmfit5=lm(sales~TV+radio+TV:radio)
summary(ad.lmfit5)
summary(lm(sales~TV*radio))
summary(lm(sales~(TV+radio+newspaper)^2))
summary(lm(sales~.^2, data=Ad))

# vif() function
plot(Credit[,c(2,3,5,11)])
summary(lm(Balance~Age+Limit))
summary(lm(Balance~Rating+Limit))
install.packages("car")
library(car)
credit.mfit=lm(Balance~Age+Limit+Rating)
summary(credit.mfit)
vif(credit.mfit)

# residual plots
ad.lmfit5=lm(sales~TV+radio+TV:radio)
par(mfrow=c(2,2))
plot(ad.lmfit5)
names(ad.lmfit5)
par(mfrow=c(1,1))
plot(TV,ad.lmfit5$residuals)
hist(ad.lmfit5$residuals)
vif(ad.lmfit5)
# polynomial functions - I(), poly() functions
ad.qfit=lm(sales~TV+radio+TV:radio+I(TV^2))
summary(ad.qfit)
plot(TV, ad.qfit$residuals)
ad.pfit=lm(sales~radio+TV:radio+poly(TV,4))
summary(ad.pfit)
par(mfrow=c(2,2))
plot(ad.pfit)
par(mfrow=c(1,1))

# correlation of error terms
acf(ad.pfit$residuals)
pacf(ad.pfit$residuals)
install.packages("nlme")
library(nlme)
ad.gls=gls(sales~radio+TV:radio+poly(TV,4), correlation=corAR1())
summary(ad.gls)
ad.gls2=gls(sales~radio+TV:radio+poly(TV,4), correlation=corARMA(p=1))
summary(ad.gls2)
summary(gls(sales~radio+TV:radio+poly(TV,4)))


