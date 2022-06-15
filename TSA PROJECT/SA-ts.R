library(astsa, quietly = TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(tseries)
library(gridExtra)
library(reshape2)
library(TTR)
SATIMESERIES<-read.csv('SATIMESERIES.csv',header = T,sep =',')
SATIMESERIES<-ts(SATIMESERIES, start = c(190,1), frequency = 1)
SATIMESERIES
acf(SATIMESERIES)
pacf(SATIMESERIES)
plot.ts(SATIMESERIES)
SATIMESERIESDECOMP<-decompose(SATIMESERIES)
SATIMESERIESDECOMP
plot(SATIMESERIESDECOMP)
SATIMESERIES_SSN_ADJ<- SATIMESERIES-SATIMESERIESDECOMP$seasonal
plot.ts(SATIMESERIES_SSN_ADJ)
SATIMESERIESSMA <- SMA(SATIMESERIES, m=3)
plot.ts(SATIMESERIESSMA)
print(SATIMESERIESDECOMP)

library(forecast)
deseason<-seasadj(SATIMESERIESDECOMP)
plot(deseason)
x<- (1:length(SATIMESERIES))
lines(predict(lm(SATIMESERIES~log(x))),col='green')
lines(predict(lm(SATIMESERIES~log(x))),col='red'[1][1])
adf.test(SATIMESERIES)
plot(SATIMESERIES)
drlog<- diff(log(SATIMESERIES))
adf.test(drlog)
drlog2<- diff(drlog)
adf.test(drlog2)
#ARIMA(1,2,7)

acf(drlog2)
pacf(drlog2)
fit1<- arima(SATIMESERIES, c(1,2,1))
fit1
names(fit1)
qqnorm(SATIMESERIES$residuals); qqline(SATIMESERIES$residuals, col =2)
install.packages('dygraphs')

#fitting risiduals
head(fit1$residuals)
fit1residuals=ts(fit1$residuals,start=c(1970,1),frequency=1)
ts.plot(fit1residuals)
fit1residuals

y=data.frame(fit1residuals)
ggplot(data=y,aes())+geom_histogram(fill='stillblue',color='black')+labs(title='Histogram of Residuals',x='residuals',y='Frequency)')

#Time series plot of differenced time series
plot.ts(drlog2)
autoplot(forecast(SATIMESERIES))
