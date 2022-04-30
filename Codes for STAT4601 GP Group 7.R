
#Import Data
library(TSA)
gold <- read.csv("gold_price_data.csv")
gold$Date <- strptime(gold$Date, '%m/%d/%Y')

#Stationarity and Differencing
gold_diff <- diff(log(gold$Value), 1)
par(mfrow=c(1,1))
acf(gold_diff,ci.type='ma',lag=100,main = 'ACF with first order differencing', xlab= 'lag',ylab='ACF')
pacf(gold_diff,lag=100,main = 'PACF with first order differencing',xlab= 'lag',ylab='PACF')

gold_diff2 <- diff(log(gold$Value), 2)
par(mfrow=c(1,1),mai=c(0.8,0.8,0.8,0.1))

plot(x=gold$Date[1:2986],y=gold_diff2,type = "l", main = 'Series with second order differencing (2008-2020)',
     ylab = "log of price (in dollar)", xlab = "time")
par(mfrow=c(1,1))
acf(gold_diff2,lag=100,main = 'ACF with second order differencing', xlab= 'lag',ylab='ACF')
pacf(gold_diff2,lag=100,main = 'PACF with second order differencing',xlab= 'lag',ylab='PACF')

# 1. Fit ARIMA(0,1,13).
ma13fit <- arima(log(gold$Value), order=c(0,1,13), method='ML')
ma13fit

resdma <- ma13fit$residuals
acf(resdma)
LB.test(ma13fit, lag=15, type="Ljung-Box")
LB.test(ma13fit, lag=18, type="Ljung-Box")
LB.test(ma13fit, lag=20, type="Ljung-Box")

arima1113 <- arima(log(gold$Value), order=c(1,1,13), method='ML')
arima1113
arima0114 <- arima(log(gold$Value), order=c(0,1,14), method='ML')
arima0114

# 2. Fit ARIMA(13,1,0).
ar13fit <- arima(log(gold$Value), order=c(13,1,0), method='ML')
ar13fit

resdar <- ar13fit$residuals
acf(resdar)
LB.test(ar13fit, lag=15, type="Ljung-Box")
LB.test(ar13fit, lag=18, type="Ljung-Box")
LB.test(ar13fit, lag=20, type="Ljung-Box")

arima1311 <- arima(log(gold$Value), order=c(13,1,1), method='ML')
arima1311
arima1410 <- arima(log(gold$Value), order=c(14,1,0), method='ML')
arima1410

# 3. Try small p,q.
arma11fit <- arima(log(gold$Value), order=c(1,1,1), method='ML')
arma11fit

resdarma11 <- arma11fit$residuals
acf(resdarma11)

LB.test(arma11fit, lag=5, type="Ljung-Box")
LB.test(arma11fit, lag=8, type="Ljung-Box")
LB.test(arma11fit, lag=10, type="Ljung-Box")

arima211 <- arima(log(gold$Value), order=c(2,1,1), method='ML')
arima211

resd211 <- arima211$residuals
acf(resd211)

LB.test(arima211, lag=5, type="Ljung-Box")
LB.test(arima211, lag=8, type="Ljung-Box")
LB.test(arima211, lag=10, type="Ljung-Box")

arima212 <- arima(log(gold$Value), order=c(2,1,2), method='ML')
arima212
arima311 <- arima(log(gold$Value), order=c(3,1,1), method='ML')
arima311

arima112 <- arima(log(gold$Value), order=c(1,1,2), method='ML')
arima112

resd112 <- arima112$residuals
acf(resd112)

LB.test(arima112, lag=5, type="Ljung-Box")
LB.test(arima112, lag=8, type="Ljung-Box")
LB.test(arima112, lag=10, type="Ljung-Box")

arima212 <- arima(log(gold$Value), order=c(2,1,2), method='ML')
arima212
arima113 <- arima(log(gold$Value), order=c(1,1,3), method='ML')
arima113

#Test and t fitting

arima1 <- arima(x=log(gold$Value), order=c(2,1,1), method='ML') 
arima2 <- arima(x=log(gold$Value), order=c(1,1,2), method='ML') 
arima3 <- arima(x=log(gold$Value), order=c(1,1,13), method='ML') 

#ARIMA(2,1,1)
acf(arima1$residuals,ci.type='ma',lag=100, main = 'ACF of resd. for ARIMA(2,1,1)') 

#ARIMA(1,1,2)
acf(arima2$residuals,ci.type='ma',lag=100, main = 'ACF of resd. for ARIMA(1,1,2)') 

#ARIMA(1,1,13)
acf(arima3$residuals,ci.type='ma',lag=100, main = 'ACF of resd. for ARIMA(1,1,13)') 

#ARIMA(2,1,1)

par(mfrow=c(1,2),mai=c(0.8,0.8,0.8,0.1))
qqnorm(arima1$residuals); qqline(arima1$residuals); hist(arima1$residuals,main='histogram of resd.', xlab = 'resd.') 
par(mfrow=c(1,1),mai=c(0.8,0.8,0.8,0.1))
shapiro.test(arima1$residuals)

#ARIMA(1,1,2)

par(mfrow=c(1,2),mai=c(0.8,0.8,0.8,0.1))
qqnorm(arima2$residuals); qqline(arima2$residuals); hist(arima2$residuals,main='histogram of resd.', xlab = 'resd.') 
par(mfrow=c(1,1),mai=c(0.8,0.8,0.8,0.1))
shapiro.test(arima2$residuals)

library("metRology")
library("fitdistrplus")

#ARIMA(2,1,1)
res1 = as.numeric(arima1$residuals)
fit1 <- fitdist(res1,"t.scaled",
                start=list(df=3,mean=mean(res1),sd=sd(res1)))
fit1

#ARIMA(1,1,2)
res2 = as.numeric(arima2$residuals)
fit2 <- fitdist(res2,"t.scaled",
                start=list(df=3,mean=mean(res2),sd=sd(res2)))
fit2

#ARIMA(2,1,1)
Box.test(arima1$residuals, lag =4, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima1$residuals, lag =6, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima1$residuals, lag =8, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima1$residuals, lag =10, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima1$residuals, lag =13, type = c("Ljung-Box"), fitdf =3)
Box.test(arima1$residuals, lag =20, type = c("Ljung-Box"), fitdf =3) 

#ARIMA(1,1,2)
Box.test(arima2$residuals, lag =4, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima2$residuals, lag =6, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima2$residuals, lag =8, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima2$residuals, lag =10, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima2$residuals, lag =13, type = c("Ljung-Box"), fitdf =3) 
Box.test(arima2$residuals, lag =20, type = c("Ljung-Box"), fitdf =3) 

## Forecasting
library(forecast)
forecast112 <- predict(arima112, n.ahead=5)
forecast112$pred
forecast112$se

forecast211 <- predict(arima211, n.ahead=5)
forecast211$pred
forecast211$se
