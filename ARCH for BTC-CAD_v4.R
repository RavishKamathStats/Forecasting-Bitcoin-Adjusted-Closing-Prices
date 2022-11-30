ARTofR::xxx_title2('4130B Project: BTC (2017- Present) Using ARCH')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                4130B Project: BTC (2017- Present) Using ARCH             ----
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
library(FinTS)
library(dynlm)
library(vars)
library(nlWaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(ggplot2)
library(pdfetch)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(lubridate)


# Simulate ARCH(1) --------------------------------------------------------
archspec = garchSpec(model = list(alpha = c(0.1), beta = 0))
Arch1 = garchSim(spec = archspec, n = 500)
ts.plot(Arch1, main = 'Simulated ARCH(1) Process')
par(mfrow = c(2,1))
acf(Arch1^2, lag.max = 25, type="correlation", 
    main="ACF for Simulated ARCH(1)" )
acf(Arch1^2, lag.max = 25, type="partial", 
    main="PACF for Simulated ARCH(1)" )


# Data set --------------------------------------------------------------------
getSymbols("BTC-CAD", src = 'yahoo',from = "2017-01-01", to = "2022-11-23")
df = data.frame(`BTC-CAD`)
BTC_close = df[,6]

df = pdfetch_YAHOO(fields = 'adjclose', 'BTC-CAD', from = as.Date('2017-01-01'), 
                   to = as.Date('2022-11-25'))
df_ts = ts(df)

# Adjusted close is the closing price after adjustments for all applicable 
#splits and dividend distributions. Data is adjusted using appropriate split 
#and dividend multipliers, adhering to Center for Research in Security 
#Prices (CRSP) standards.

# Step 0: Exploratory Analysis -------------------------------------------------
#Overall Time Series Plot
chartSeries(`BTC-CAD`)

# Based of the overall time series chart, we can see high fluctuations in 
# multiple time periods. Hence why mast analyst say that crypto is an unstable 
# currency, when comparing to the US dollar for example.

#Chart indicating possible high level of volatility
chartSeries(`BTC-CAD`, type = 'candlesticks',subset = "2021-03::2021-07")

#Understanding Candlestick Charts

# The top outlier line of a single candlestick represents the highest price sold 
# at that specific time period. Likewise the lower outlier line represents the 
# lowest price.

# The bottom of the candle represents the closing price at a specific time period
# while the the top of the candle represents the opening price. If we see a 
# colour other than green, we know that the closing price is lower than the 
# opening price. Likewise, if we see a green candlestick, the opening price was 
# lower than the closing price at the specific time period. 

#Crypto Crashes:

#### December 2017-December 2018: -84%
# 2017 was a landmark year for Bitcoin, which broke all its own records and peaked
# near $20,000. Then, on Dec. 27, it all came crashing down as investors harvested 
# gains from what was an obvious bubble and sent the price cratering below $12,000. 
# The cryptocurrency would remain in the doldrums throughout 2018, as major hacks 
# in Korea and Japan — as well as rumors that those countries were planning to ban 
# Bitcoin — sent already skittish investors looking for the exits.


#### March 2020: -50%
# The pandemic did not spare Bitcoin, and when the markets crashed in March 2020, 
# the Bitcoin market crashed even harder. Bitcoin lost half its value in two days. 
# Over a month, it fell from above $10,000 in February to below $4,000 in March.

#### May 2021: -53%
# In April, Bitcoin was the talk of the investing world as it roared past an 
# astonishing $64,000 for a single coin. Then, in a flash, $1 trillion in value 
# was wiped off the global crypto market in a single week. First, Elon Musk went 
# back on a promise to accept Bitcoin as a payment for Tesla cars. Then, China 
# announced yet another crypto crackdown. Finally, the public learned about the 
# environmental impact of Bitcoin mining and crypto investors found themselves 
# in a familiar position — at the mercy of forces beyond their control.

#### Bear market No. 5: Bitcoin plummets from $68,000 to below $20,000 in 2022
# # Bitcoin failed to break $70,000 and started dropping in late 2021. The 
# # cryptocurrency has slipped into a bear market since November last year, 
# # recording one of its biggest historical crashes in 2022. In June, the 
# # cryptocurrency plunged below $20,000 for the first time since 2020, fueling 
# # extreme fear on the market. The ongoing bear market is largely attributed to 
# the crisis of algorithmic stablecoins — namely the TerraUSD Classic (USTC) 
# stablecoin — which are designed to support a stable 1:1 peg with the U.S. 
# dollar through blockchain algorithms rather than equivalent cash reserves.


#ACF & PACF
par(mfrow = c(2,1))
acf(df_ts,lag.max=2000,type="correlation",main="ACF")
acf(df_ts, lag.max=50,type="partial",main="PACF")



# Step 1: Checking for Normality ------------------------------------------
hist(df_ts, main = 'Histogram of BTC', freq = F, col = 'blue')

# Data clearly does not appear to be normal. Seems to be skewed to the right.
#However to confirm we will use the Shapiro Test to confirm our observation.

#Using the Shapiro Test for Normality
shapiro.test(df_ts)

# H_0 = Data is normally distributed

#Since our p-value is very small, we reject our null hypothesis, and infer that
#there is evidence of non-normality in our data set. 

#In order to overcome this, we will take the log of our data, to normalize it

logdf = log(df)
logdf_ts = ts(logdf)

plot.ts(logdf_ts)
title('Time Series Plot for the Log of BTC Prices')


# Step 2: Checking for Stationarity ---------------------------------------
Auto.VR(logdf)

#H_0 = Data has constant variance
#When running this function, the ratio test statistic is quite high, which 
#translates to non-constant variance. Hence we reject our null hypothesis.
# This means that we will need to add an ARCH model with an ARMA model. 

adf.test(logdf$`BTC-CAD`, k = 3)

#H_0: Non-stationary. Since our p-value is quite large, We cannot reject our
#null hypothesis, hence we can say that there is no evidence of stationarity.

#In order to make our data stationary, we will do a first difference of our data
#to see if this will aid with the stationarity aspect. 

logdiffdf = diff(logdf)
logdiffdf = na.remove(logdiffdf)  

# After completing our difference, we will now check for stationarity. 

adf.test(logdiffdf, k = 3)
Auto.VR(logdiffdf)
#H_0: Non-stationary. With our p-value being quite small, we can reject our
#null hypothesis and infer that there is evidence of stationarity in our 
#data-set. 

return = CalculateReturns(`BTC-CAD`, method = c('log', 'difference'))
return = return[-c(1),]
ret_adj = return$`BTC-CAD.Adjusted`
chart_Series(ret_adj, 
             name = 'Time Series Plot for BTC-CAD (log-differenced)')

#To give a visualization of stationarity, we can plot the time series.

# Step 3: Determining ARMA -----------------------------------------------------

#We will first check our ACF and PACF plots to determine what type of ARMA model
#we can use. 
par(mfrow = c(2,1))
acf(logdiffdf, lag.max = 1000, type="correlation", 
    main="ACF for BTC from 2017 - Present" )
acf(logdiffdf, lag.max = 1000, type="partial", 
    main="PACF for BTC from 2017 - Present" )

#ACF and PACF are not clear at all in indicating whether we should use an 
#ARMA process. We can further check it with the next function.

auto.arima(logdiffdf)

arima010 = arima(logdf, order = c(0,1,0))
summary(arima010)
tsdiag(arima010)


#Based off this output, we can now confirm that there is no need for adding an
#ARMA process to our model. Hence we can only use an ARCH process to best model
#this data set. One thing to note on the PACF, we do see quite a few spikes, that
#may refer to the volatility. This gives us further evidence of using an ARCH model.


checkresiduals(arima010)
ggtsdisplay(arima010$residuals, main = 'BTC ARIMA (0,1,0) Residuals')


#When looking at the residuals, we can see clear signs of volatility at multiple 
#points (the length of each vertical line). 

##FOR VI###
#You can use either one of the functions to check the residuals and explain.



# Step 4: Estimating the mean Equation -----------------------------------------

BTC_mean = dynlm(arima010$residuals ~ 1)
summary(BTC_mean)

#Based of this output, we can see that the mean value is insignificant. 
#It means that we are able to capture any mean effect by using the differencing.


# Step 5: ARCH EFFECT ----------------------------------------------------------
#Estimating the square of the residuals
esq = ts(resid(BTC_mean)^2)

#Creating the regression of the squared residuals with the lag of residual squares
BTC_ARCH_EF = dynlm(esq ~ L(esq))
summary(BTC_ARCH_EF)

#To understand this, the past squared residuals are explaining the present squared 
#residuals. Hence the residuals are a function of their past. To note, the squared
#residuals represents the volatility. 
#Checking the p-values we can see they are highly significant which shows 
#evidence of ARCH effect. 

#We can further confirm the ARCH effect using another command as shown below
ArchTest(arima010$residuals, lags = 1, demean = T)

#H_0: No Arch Effects. With our p-value being small, we can reject our null 
#hypothesis and say there is evidence of ARCH effect. 


# Step 6: Estimating the ARCH equation ------------------------------------

#Using/ARCH(1)
armaORDER = c(0,0)
garchORDER = c(0,1)
ARCH_spec = ugarchspec(mean.model = list(armaOrder = armaORDER), 
                        variance.model = list(model = "sGARCH", 
                                              garchOrder = garchORDER),
                        distribution.model = 'norm')
ARCH1 = ugarchfit(data = logdiffdf, 
                          spec = ARCH_spec, out.sample = 50)
ARCH1
#plot(ARCH1, which = 'all')

#AIC: -3.7679
#BIC: -3.7545


#Using ARCH(2)
armaORDER = c(0,0)
garchORDER = c(0,2)
ARCH_spec = ugarchspec(mean.model = list(armaOrder = armaORDER), 
                        variance.model = list(model = "sGARCH", 
                                              garchOrder = garchORDER),
                        distribution.model = 'norm')
ARCH2 = ugarchfit(data = logdiffdf, 
                          spec = ARCH_spec, out.sample = 50)
ARCH2

#AIC: -3.5467
#BIC: -3.5359

#Using ARCH(3)
armaORDER = c(0,0)
garchORDER = c(0,3)
ARCH_spec = ugarchspec(mean.model = list(armaOrder = armaORDER), 
                        variance.model = list(model = "sGARCH", 
                                              garchOrder = garchORDER),
                        distribution.model = 'norm')
ARCH3 = ugarchfit(data = logdiffdf, 
                          spec = ARCH_spec, out.sample = 50)
ARCH3

#AIC: -3.5462
#BIC: -3.5327


#We Have computed multiple ARCH models. Technically we should choose the one that
#has the smallest AIC & BIC. However all 3 models seem to relatively have similar
#values of AIC & BIC. For simplicity of the model, we would choose ARCH(1) as the 
#best model for BTC. 



# Step 7: Forecasting ---------------------------------------------------------
pred  = ugarchboot(ARCH3, n.ahead = 20, method = c('Partial', 'Full')[1])
plot(pred, which = 2)

# This shows the forecasting adjusted closing prices we could expect in the next
# 10 days. The dark blue dots represents the upper limit of our estimation of the
# ARCH model that we have built. Recall that we did a log and differencing, 
# to be able to have normality and stationarity, hence the interpretation of the 
# returns is not very clear. However we can expect few changes in the price in the
# next few days. 


# Further Notes -----------------------------------------------------------
#Let it be noted that this is still a very simplistic approach to forecasting.
# There is no gurantee that BTC will be at this level in the next 10 days, as stocks
# and crypto are very speculative. News and certain financial plays can be a major 
# shock that could either push the currency up or down in the future.

