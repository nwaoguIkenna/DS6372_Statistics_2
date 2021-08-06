  AR1<-arima.sim(list(ar=c(0)),10000) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
  par(mfrow=c(1,3))
  plot(1:10000,AR1,type="l")
  acf(AR1,main="ACF")
  pacf(AR1,main="PACF")

AR1<-arima.sim(list(ar=c(0.8)),50) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:50,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

rho1<-.8
rho2<-.6
a1<-(rho1*(1-rho2)/(1-rho1^2))
a2<-(rho2-rho1^2)/(1-rho1^2)
AR2<-arima.sim(list(ar=c(a1,a2)),10000)
par(mfrow=c(1,3))
plot(1:10000,AR2,type="l")
acf(AR2)
pacf(AR2,main="PACF")

library(tseries)
library(forecast)

bills<-read.csv("~/datascience/DS6372/Unit4/Unit4HW/ElectricBill.csv")
bills$DateIndex<-1:nrow(bills)
ggplot()+geom_line(data=bills,aes(x=DateIndex,y=Bill))
attach(bills)
Acf(Bill)
Pacf(Bill)

AR1<-arima(Bill,order=c(1,0,0))
AR2<-arima(Bill,order=c(2,0,0))
AR3<-arima(Bill,order=c(3,0,0))
AR4<-arima(Bill,order=c(4,0,0))
AR5<-arima(Bill,order=c(5,0,0))
tsdisplay(residuals(AR1),lag.max=15,main="AR(1) Resid. Diagnostics")

tsdisplay(residuals(AR2),lag.max=15,main="AR(2) Resid. Diagnostics")
tsdisplay(residuals(AR3),lag.max=15,main="AR(3) Resid. Diagnostics")

tsdisplay(residuals(AR4),lag.max=15,main="AR(4) Resid. Diagnostics")

tsdisplay(residuals(AR5),lag.max=15,main="AR(5) Resid. Diagnostics")

AIC(AR1)
AIC(AR2)
AIC(AR3)
AIC(AR4)
AIC(AR5)



plot(forecast(AR4,h=5))
points(1:length(Bill),fitted(AR4),type="l",col="blue")

Bill
bills



holdout.test<-window(ts(Bill),start=36)
train<-Bill[1:35]
predictor<-as.matrix(AvgTemp[1:35])
simpleols<-arima(train,order=c(0,0,0),xreg=predictor)
tsdisplay(residuals(simpleols),lag.max=15,main="Resid. Diagnostics of OLS")


ARIMA.with.Pred<-auto.arima(train,xreg=predictor,stepwise=FALSE)
A#RIMA.with.Pred<-auto.arima(predictor,stepwise=FALSE)
#ARIMA.with.Pred<-auto.arima(train,stepwise=FALSE)
ARIMA.with.Pred


tsdisplay(residuals(ARIMA.with.Pred),lag.max=15,main="Resid. Diagnostics with AR(4)")


plot(forecast(ARIMA.with.Pred,h=5,xreg= as.matrix(AvgTemp[36:40])))
## Warning in forecast.forecast_ARIMA(ARIMA.with.Pred, h = 5, xreg =
## data.frame(predictor = AvgTemp[36:40])): xreg not required by this model,
## ignoring the provided regressors
points(1:length(train),fitted(ARIMA.with.Pred),type="l",col="blue")
points(1:40,Bill,type="l")

AR4train<-arima(train,order=c(4,0,0))
plot(forecast(AR4train,h=5))
points(1:length(train),fitted(AR4train),type="l",col="blue")
points(1:40,Bill,type="l")

accuracy(forecast(AR4train,h=5),Bill[36:40])
  

beer_reviews$review_date_time = as.POSIXct.numeric(beer_reviews$review_time,origin='1970-01-01 00:00:00',tz='EST')