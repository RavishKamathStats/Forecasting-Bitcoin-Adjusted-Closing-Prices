# Section 1 ---------------------------------------------------------------
ARTofR::xxx_title2('4130B Project: ARCH Model')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          4130B Project: ARCH Model                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries ---------------------------------------------------------------
library(astsa) 
library(zoo)
library(forecast) 
library(tseries)


# Data set -----------------------------------------------------------------
dat = read.csv(file.choose(), header = T)
View(dat)
close = as.numeric(dat[,5])

# Checking ACF and PACF and Time Series Plot -----------------------------------
par(mfrow=c(3,1),oma=c(0,0,0,0)) 
acf(close,lag.max=25,type="correlation",main="ACF for the BTC-CAD Adjusted Closing Price \n over 5 years") 
acf(close, lag.max=25,type="partial",main="PACF for the BTC-CAD AdjustedClosing Price \n over 5 years")

#Original Time Series Plot
plot(close,type = 'o', main = 'Time Series Plot for BTC-CAD Closing Price', ylab = 'Closing Price',
     xaxt = 'n', pch=16,cex=.5, xlab = '')

T = length(dat[,5])
lablist = as.vector(dat[seq(1,T,42),1])
axis(1, seq(1, T, 42), labels = F)
text(seq(1, T, 42), par('usr')[3]-.01, labels = lablist,
     srt = 45, pos = 1, offset = 2, xpd = T)



# Differentiating & Fitting a SARIMA Model -------------------------------------

###############Difference Applied to Time Series Plot
close_diffs = diff(diff(close,lag=1),lag=5)
plot(close_diffs,type = 'o', main = 'Time Series Plot for NASDAQ Composite', ylab = 'Closing Price',
     xaxt = 'n', pch=16,cex=.5, xlab = '')
T = length(dat[,5])
lablist = as.vector(dat[seq(1,T,42),1])
axis(1, seq(1, T, 42), labels = F)
text(seq(1, T, 42), par('usr')[3]-.01, labels = lablist,
     srt = 45, pos = 1, offset = 2, xpd = T)

par(mfrow=c(2,1),oma=c(0,0,0,0)) 
acf(close_diffs, lag.max=50, type="correlation", main = "ACF for NASDAQ Composite Differenced") 
acf(close_diffs, lag.max=50, type="partial", main = "PACF for NASDAQ Composite Differenced")


###############Fitting a SARIMA Model
close.fit.sar = Arima(close,order=c(0,1,1),seasonal=list(order = c(0,1,1),period=5)) 
res.close.sar<-as.vector(residuals(close.fit.sar))
fit.close.sar<-as.vector(fitted(close.fit.sar))

#ACF and PACF of the Residuals
par(mfrow=c(1,2),oma=c(0,0,0,0)) 
acf(res.close.sar,lag.max=50,type="correlation",main="ACF of the NASDAQ Composite Residuals")
acf(res.close.sar,lag.max=50,type="partial",main="PACF of the NASDAQ Composite Residuals")

#4-in-1 plot of the residuals
par(mfrow=c(2,2),oma=c(0,0,0,0)) 
qqnorm(res.close.sar,datax=TRUE,pch=16,xlab='Residual',main='') 
qqline(res.close.sar,datax=TRUE) 
plot(fit.close.sar,res.close.sar,pch=16, xlab='Fitted Value',ylab='Residual')
abline(h=0)
hist(res.close.sar,col="gray",xlab='Residual',main='') 
plot(res.close.sar,type="l",xlab='Observation Order', ylab='Residual') 
points(res.close.sar,pch=16,cex=.5)
abline(h=0)




# Fitting an ARCH Model ---------------------------------------------------
# Fit a ARCH(0,1) model
close.arch1<-garch(close_diffs, order = c(0,1), trace = FALSE) 
summary(close.arch1)

# Fit a ARCH(0,2) model
close.arch2<-garch(close_diffs, order = c(0,2), trace = FALSE) 
summary(close.arch2)

# Fit a ARCH(0,3) model
close.arch1<-garch(close_diffs, order = c(0,3), trace = FALSE) 
summary(close.arch1)

















# Section 2 ---------------------------------------------------------------

ARTofR::xxx_title2('4130B Project: Online Example')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##                        4130B Project: Online Example                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries ---------------------------------------------------------------
library(quantmod)
library(dplyr)
library(tidyverse)
library(forecast) 
library(tseries)
library(rugarch)
library(qrmtools)
library(xts)
library(PerformanceAnalytics)


# Data set --------------------------------------------------------------------
getSymbols("BTC-CAD", src = 'yahoo',from = "2017-11-12", 
                to = "2022-11-12" )
head(`BTC-CAD`)
df = data.frame(`BTC-CAD`)
head(df)



# ACF/PACF & Time Series Plots--------------------------------------------------
chartSeries(`BTC-CAD`)
par(mfrow = c(2,1))
acf(df$BTC.CAD.Adjusted,lag.max=1000,type="correlation",main="ACF") 
acf(df$BTC.CAD.Adjusted, lag.max=1000,type="partial",main="PACF")

# Differentiating ---------------------------------------------------------
#Doing difference

# Adjusted close is the closing price after adjustments for all applicable 
#splits and dividend distributions. Data is adjusted using appropriate split 
#and dividend multipliers, adhering to Center for Research in Security 
#Prices (CRSP) standards.

return = CalculateReturns(`BTC-CAD`, method = 'difference')
return = return[-c(1),]


# Time Series Plot and ACF/PACF After Differencing  -----------------------
#Time Series Plot
chart_Series(return$`BTC-CAD.Adjusted`, 
             name = 'Time Series Plot for BTC-CAD (differenced)')
#ACF & PACF
par(mfrow = c(2,1))
acf(return$`BTC-CAD.Adjusted`,lag.max=1000,type="correlation",main="ACF") 
acf(return$`BTC-CAD.Adjusted`, lag.max=1000,type="partial",main="PACF")

#Normality Plot 
chart.Histogram(return$`BTC-CAD.Adjusted`, colorset = c('blue'))
legend('topright', legend = c('return'), fill = c('blue'))

#Rolling Performance Chart For Volatility
chart.RollingPerformance(R = return$`BTC-CAD.Adjusted`['2020::2022'],width = 22, 
                         FUN = 'sd.annualized',
                         scale = 252, main = "BTC-CAD's Monthly Volatility")


# FITTING AN ARCH(1) Model ------------------------------------------------
armaORDER = c(0,0)
garchORDER = c(0,1)
model_spec = ugarchspec(mean.model = list(armaOrder = armaORDER), 
                        variance.model = list(model = "sGARCH", 
                                              garchOrder = garchORDER),
                        distribution.model = 'norm')
model_fitting = ugarchfit(data = return$`BTC-CAD.Adjusted`, 
                          spec = model_spec, out.sample = 20)
model_fitting


# Plots after Fitting -----------------------------------------------------
plot(model_fitting, which = 'all')





