
kings<-scan("https://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
summary(kings)
kingstimeseries<-ts(kings)
kingstimeseries
install.packages("TTR")
library(TTR)
hist(kings,col = 'red',main = "Histogram of Kings")
boxplot(kings,col='red',horizontal = T)

##PLOT.TS
plot.ts(kingstimeseries,col='red')
##moving average kings
kingstimeseriesSMA3<-SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3,col='red')
kingstimeseriesSMA8<-SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8,col='red')


###n=3 or n=8 is good
kingstimeseriesSMA3
e3=kingstimeseries - kingstimeseriesSMA3
e3
mse3=sum(e3^2,na.rm = T)/40
mse3

##mse 8
kingstimeseriesSMA8
e8=kingstimeseries - kingstimeseriesSMA8
mse8=sum(e8^2,na.rm = T)/35
mse8


##dATA BIRTH 
birth<-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries<-ts(birth,frequency = 12,start = c(1946,1))
birthstimeseries
summary(birthstimeseries)
summary(birth)
plot.ts(birthstimeseries,col='blue')

##birth data
birthstimeseriescomponents<-decompose(birthstimeseries)
plot(birthstimeseriescomponents,col='blue')

#birth seasonal component
birthstimeseriescomponents$seasonal
#De seasonalized birth
birthstimeseriesseasonallyadjusted<-birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted,col='blue')

##souvenir
souvenir<-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries<-ts(souvenir,frequency = 12,start = c(1987,1))
souvenirtimeseries
plot.ts(souvenirtimeseries,col='black',lwd=4)
logsouvenirtimeseries<-log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

##data souvenir
souvenirtimeseries
logsouvenirtimeseries


##Rain data
rain<-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip = 1)
rainseries<-ts(rain,start = c(1813))
rainseries

plot.ts(rainseries,col='purple',lwd=2)


#data rain
rainseriesforecasts<-HoltWinters(rainseries,beta = FALSE,gamma =FALSE)
rainseriesforecasts


#haltwinter 
rainseriesforecasts$fitted
plot(rainseriesforecasts,col='purple',lwd=3)
rainseriesforecasts$SSE

#rainseriesforecasts2<-forecast:::forecast.Holtwinters(rainseriesforecasts,h=8)
rainseriesforecasts2<-forecast:::forecast.HoltWinters(rainseriesforecasts,h=8)
rainseriesforecasts2
forecast:::plot.forecast(rainseriesforecasts2,ccol = 'purple',lwd=4)

##Holt winters rain
rainseriesforecasts2$residuals
rainseriesforecasts2$fitted
acf(rainseriesforecasts2$residuals,lag.max = 10,na.action = na.pass)
Box.test(rainseriesforecasts2$residuals,lag = 20,type = 'Ljung-Box')
plot.ts(rainseriesforecasts2$residuals,col='purple',lwd=4)
Box.test(rainseriesforecasts2$residuals,lag = 20,type = 'Ljung-Box')


##Holt-Winter:rain
library(psych)
describe(rainseriesforecasts2$residuals)
hist(rainseriesforecasts2$residuals,col='blue')

##data skirts
skirts<-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip = 5)
skirts
skirtsseries<-ts(skirts,start =c(1886))
skirtsseries
skirtsseriesforecasts<-HoltWinters(skirtsseries,gamma = FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts,col = 'purple')
plot(skirtsseries)

##Holt -Winters: skirts
skirtsseriesforecasts<-HoltWinters(skirtsseries,gamma = FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE

HoltWinters(skirtsseries,gamma = FALSE,l.start = 608, b.start = 9)
skirtsseriesforecasts2<-forecast:::forecast.HoltWinters(skirtsseriesforecasts, h=19)
forecast:::plot.forecast(skirtsseriesforecasts2)
#install.packages("uroot")
##library(uroot)

#holt winter skirts
acf(skirtsseriesforecasts2$residuals, lag.max = 20,na.action = na.pass)
plot.ts(skirtsseriesforecasts2$residuals,lwd=2,col='blue')


#box test 
Box.test(skirtsseriesforecasts2$residuals,lag=20,type = "Ljung-Box")
hist(skirtsseriesforecasts2$residuals,col='green')
describe(skirtsseriesforecasts2$residuals)


##Data : souvenir
souvenir<-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir

souvenirtimeseries<-ts(souvenir,frequency = 12,start = c(1987,1))
souvenirtimeseries
plot.ts(souvenirtimeseries,col='black',lwd=4)
logsouvenirtimeseries<-log(souvenirtimeseries)
souvenirtimeseriesforecasts<-HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts 
plot(souvenirtimeseriesforecasts, lwd=2)
souvenirtimeseriesforecasts2<- forecast:::forecast.HoltWinters(souvenirtimeseriesforecasts,h=48)
souvenirtimeseriesforecasts2
acf(souvenirtimeseriesforecasts2$residuals,lag.max = 20,na.action = na.pass)
Box.test(souvenirtimeseriesforecasts2$residuals,lag=20,type = "Ljung-Box")
plot.ts(souvenirtimeseriesforecasts2$residuals, lwd=3)
describe(souvenirtimeseriesforecasts2$residuals)

##hw is part of forecast library
install.packages("forecast")
library(forecast)

library(psych)
# Holt Winter : Souvenir  : Additive vs Multiplicative 
svnr=logsouvenirtimeseries
addt <- hw(svnr, seasonal="additive")
mult<-hw(svnr,seasonal="multiplicative")
plot(addt)
plot(mult)
par(mfrow= c(2,1))
plot(addt)
plot(mult)
par(mfrow= c(2,1))
a=HoltWinters(svnr,seasonal = "additive")
plot(a)
addt<-hw(svnr,seasonal = "additive")
plot(addt)
par(mfrow=c(2,1))
b=HoltWinters(svnr,seasonal = "multiplicative")
plot(b)
mult<-hw(svnr,seasonal = "multiplicative")
plot(mult)

##holt winter souvenir Additive vs Multiplicative
library(ggplot2)
autoplot(svnr)+autolayer(addt,series = "HW Addt Forecasts",PI=FALSE)+
  autolayer(mult,series = "HW Mult Forecasts",PI=FALSE)+
  xlab("Year")+
  ylab("Sovenir Sales")+
  ggtitle("Addtv ns Mult")+
  guides(colour=guide_legend(title = "Forecasts"))

##additive vs multiplicative
autoplot(svnr)+autolayer(addt,series = "HW Addt Forecasts",PI=TRUE, lwd=4)+
  autolayer(mult,series = "HW Mult Forecasts",PI=TRUE, lwd=3)+
  xlab("Year")+
  ylab("Sovenir Sales")+
  ggtitle("Addtv ns Mult")+
  guides(colour=guide_legend(title = "Forecasts"))


##rmse additive vs multi

accuracy(addt)
accuracy(mult)



#####Auto Regressive Integrated Moving Average
skirts<-scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat", skip = 5)
skirts
skirtsseries<-ts(skirts,start =c(1886))
plot.ts(skirtsseries,col='purple',lwd=2)
skirtsseriesdiff1<-diff(skirtsseries, differences = 1)
plot.ts(skirtsseriesdiff1,col='purple',lwd=2)
skirtsseriesdiff2<-diff(skirtsseries,differences = 2)
plot.ts(skirtsseriesdiff2,col='purple',lwd=4)


##Differencing :kings

kings<-scan("https://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
summary(kings)
kingstimeseries<-ts(kings)
kingstimeseries

plot.ts(kingstimeseries,col='red',lwd=2)
kingstimeseriesdiff1<-diff(kingstimeseries,differences = 1)
plot.ts(kingstimeseriesdiff1,col='red', lwd=4)

##ach kings difference 
acf(kingstimeseriesdiff1,lag.max = 20,col='red',lwd=4)
acf(kingstimeseriesdiff1,lag.max = 20, plot = FALSE)

#pacf kings difference 
pacf(kingstimeseriesdiff1,lag.max = 20,col='red',lwd=4)
pacf(kingstimeseriesdiff1,lag.max = 20,plot = FALSE)

##sTATIONARY CHECK : ADF TEST
install.packages("tseries")
library("tseries")
adf.test(kingstimeseries)
adf.test(kingstimeseriesdiff1)
##?adf.test
###Auto Regressive integrated Moving Average 
kingstimeseriesARIMA<- arima(kingstimeseries, order = c(1,1,2))
kingstimeseriesARIMA

library(forecast)
##Auto Regressive Integrated moving Average : Forecasts
fcastkings=forecast(kingstimeseriesARIMA,h=5)
plot(fcastkings)
tsdisplay(residuals(kingstimeseriesARIMA),lag.max = 20, main="(1,12) Model Residuals", col='purple',lwd=4)
plot(kingstimeseriesARIMA$residuals,lwd=3, col='green', main='Resisual Plot (1,1,2)')

##Histrogram of residuals
hist(kingstimeseriesARIMA$residuals, ccol = 'green', main='Histogram of residuals(1,1,2)')
library(psych)
describe(kingstimeseriesARIMA$residuals)

##let's confirm statistically 
Box.test(kingstimeseriesARIMA$residuals,lag = 20,type = "Ljung-Box")

Box.test(kingstimeseriesARIMA$residuals,lag=20,type = "Ljung-Box")
accuracy(kingstimeseriesARIMA)
