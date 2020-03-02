# TS Final Project Data Exploration"
# Global Climate Change Data from 1750－2015
# data source: https://data.world/data-society/global-climate-change-data

# to prevent scientific notation, you can use a large value like 999
options(scipen=999)
# suppress warnings globally
options(warn=-1)

# load useful time series packages
library(fracdiff)
library(stats)
library(tseries)
library(forecast)
library(TSA)
library(zoo)
library(xts)
library(lmtest)
library(ggplot2)
library(imputeTS)
library(knitr)
library(MLmetrics)

setwd("/Users/michaelcolella/Google Drive/Graduate School/MScA/Courses/Time Series Analysis and Forecasting/Final Project/")
data <- read.csv('GlobalTemperatures.csv')
# LandAverageTemperature: global average land temperature in celsius
# LandAverageTemperatureUncertainty: the 95% confidence interval around the average

head(data)
summary(data)

ts.data <- ts(data$LandAverageTemperature, start = 1750, frequency = 12) # ts data is for land avg temp only
var.data <- ts(data$LandAverageTemperatureUncertainty, start =1750, frequency = 12) # this ts data is for land avg temp uncertainty for land data only
ts.ocean.data <- ts(data$LandAverageTemperatureUncertainty[1201:3192], start =1850, frequency = 12) # shouldn't this be on the column of LandAndOceanAverageTemperature?

# plot land temp ts data only
ts.plot(ts.data,type='p', ylab = 'Land Temperature',main= 'Monthly Average Land Temperture Measurements')
abline(v = 1900, col ='red')

# plot land temp uncertainty data only (as a ts)
ts.plot(var.data, type = 'p', ylab = "Measurement Variability", main ="Measurement Variability over Time")
abline(v = 1900, col ='red')

# Since we can see the accuracy of measurements prior to 1900 are much less reliable, we will only use temperature data 
# from 1/1/1900 onwards for our model
train.data <- ts(data$LandAverageTemperature[1801:3132], start = 1900, frequency = 12) # just the 1331 points of interest
str(train.data)
summary(train.data) # 
#  Min.  Median    Mean 3rd Qu.    Max. 
# 1.395   4.588   9.047   8.724  12.918  15.354

# stationarity testing
# test for Stationarity quantitatively - use ADF and KPSS from package tseries
# ADF = Augmented Dickey Fuller test -- a unit root test
# KPSS = -- Kwiatkowski–Phillips–Schmidt–Shin test -- non-parametric stationarity test

# ADF test 
# Null hypothesis: Series contains a unit root or series is non-stationary
# Alternative hypothesis: Process has root outside unit circle; so stationarity or trend stationarity
adf.test(data$LandAverageTemperature[1801:3132]) # can reject the null -- supports stationarity

# KPSS test for stationarity
# Null hypothesis: Level stationarity or Trend stationarity (depending on specification)
# Alternative hypothesis: There is a unit root

# kpss test for level stationarity
kpss.test(data$LandAverageTemperature[1801:3132], null = c("Level")) # reject null -- supports level non-stationarity

# kpss test for trend stationarity
kpss.test(data$LandAverageTemperature[1801:3132], null = c("Trend")) # fail to reject null -- supports trend stationarity


# acf and pacf plots
acf(train.data) # includes both direct and indirect correlations; clearly suggests seasonality
pacf(train.data) # partial autocorrelation functions seeks to remove indirect correlations

# decompose training data
decompose(train.data) # additive time series
train_decomp <- decompose(train.data)
plot(train_decomp) # breaks down training data into observed values, trend, seasonal and random components

test.data <- ts(data$LandAverageTemperature[3133:3192], start = 2011, frequency = 12) # 60 data points, or 5 years worth of data
summary(test.data)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.157   5.897   9.966   9.606  13.449  15.482 

# write train and test sets to CSVs -- only on initial run of code as a pre-processing step
# write.csv(train.data, 'train.data.csv')
# write.csv(test.data, 'test.data.csv')

# line graph for Land Temp data
ts.plot(train.data, ylab = 'Land Temperature', xlab = 'Date',main = 'Global Monthly Land Temperature')

# scatter plot for Land Temp data
ts.plot(train.data, type = 'p',ylab = 'Land Temperature', xlab = 'Date',main = 'Global Monthly Land Temperature')

# plot land temp data for different seasons
color = c('blue','yellow','red','orange')

plot(as.yearmon(time(train.data))[1+12*0:110],train.data[1+12*0:110], type='p', ylim = c(0,15),xlim = c(1900,2025),col = color[1],
     xlab = 'Date',ylab = 'Land Temperature', main = "Seasonal Land Temperature")
points(as.yearmon(time(train.data))[2+12*0:110],train.data[2+12*0:110],type='p',col = color[1])
points(as.yearmon(time(train.data))[12+12*0:110],train.data[12+12*0:110],type='p',col = color[1])

for (i in 1:3) {
  points(as.yearmon(time(train.data))[12*0:110+3*i],train.data[12*0:110+3*i], col = color[i+1],type='p')
  points(as.yearmon(time(train.data))[12*0:110+3*i*1],train.data[12*0:110+3*i+1], col = color[i+1],type='p')
  points(as.yearmon(time(train.data))[12*0:110+3*i+2],train.data[12*0:110+3*i+2], col = color[i+1],type='p')
}

legend("topright",legend = c("Dec - Feb","Mar - May","June - Aug","Sep - Nov"),fill = color, cex=0.8)

# plot land temp data for different months
color = rainbow(12)
plot(as.yearmon(time(train.data))[1+12*0:110],train.data[1+12*0:110],col = color[1],type ='p', ylim = c(0,15),xlim = c(1900,2025) ,xlab = 'Date',ylab = 'Land Temperature', main = "Monthly Land Temperature")

for (i in 2:12) {
  points(as.yearmon(time(train.data))[i+12*0:110],train.data[i+12*0:110],type ='p', col = color[i])
}
legend("topright",legend = c("Jan","Feb","Mar","Apr","May",'June','Jul','Aug','Sep','Oct','Nov'), fill = color, cex=0.8)


# base model: linear regression with month and year as predictor
y <- train.data[1:1332]
month <- factor(rep(1:12,111))
year <- rep(1900:2010,each =12)

lm.model <- lm(y ~ month + year)

plot(as.yearmon(time(train.data)),lm.model$fitted.values,ylim = c(0,15), type= 'l',pch=20,ylab = 'Temperature', main = "Linear Model Predicted Values", xlab = 'Date')
points(as.yearmon(time(train.data)),train.data[1:1332],col = 'red', type = 'p',pch=20)
legend('bottomright',legend = c('Predicted','Actual'),col =c('black','red'),lty=1, cex=0.8)

# There are four commonly used strategies for making multi-step forecasts:
# Direct Multi-step Forecast Strategy
# Recursive Multi-step Forecast Strategy
# Direct-Recursive Hybrid Multi-step Forecast Strategies
# Multiple Output Forecast Strategy

# --- model 1: Auto arima  NOT SURE IF THIS COUNTS W/O RESPECTIVE FORECASTING COMPONENT AS ASSIGNMENT CALLS FOR 3 FORECASTING MODELS
model.1 <- auto.arima(train.data)
model.1 # auto arima recommends (p,d,q) = (0,0,1) & (P,D,Q) = (0,1,1)
# [12] means # of periods in a season, or in this case, # of months in a year
# standard error of coefficient is sufficiently smaller than coefficient
# AIC=724.54   AICc=724.57   BIC=745.28

# plot residuals
plot(model.1$residuals, type='p', ylab = 'Residuals') # look like white noise

# get acf plot, residual distrbution and plot of training data
checkresiduals(model.1) # acf correlogram shows autocorrelation coefficients at different lags using height graph
# get pacf plot of residuals
pacf(model.1$residuals)

# Ljung-Box test: LBQ stat tests null that autocorrelations up to lag k equal 0 (data values are random and indep. up to certain # lags)
Box.test(adf.test(data$LandAverageTemperature[1801:3132]), lag=12, type=c("Ljung-Box")) # can reject null; autocorrelations are not 0; residuals not white noise
adf.test(model.1$residuals)

# --- model 2: Recursive Multi-step Forecast Strategy
# generates separate model for each forecasted time step
# b/c separate models are used, it means that there is no opportunity to model the dependencies between the predictions, 
# such as the prediction on day 2 being dependent on the prediction in day 1
recursive_fit <- Arima(train.data, order = c(0,0,1), list(order=c(0,1,1), period = 12)) # order as detemined by auto.arima
recursive_fit # AIC = 762.32   AICc = 762.34   BIC = 777.88

# good to look into residuals for fitted model on train data set
plot(recursive_fit$residuals) # residuals look good; no particular structure; relatively consistent variance
qqnorm(recursive_fit$residuals) # look fairly normal along line that cuts plot in half
acf(recursive_fit$residuals) # can verify residuals have no structure for most part but visible lag component

# mean of residuals
mean(recursive_fit$residuals) # 0.04

# actual recursive forecast -- as we have been doing most commonly on assignments
forecast(recursive_fit, h = 60) # forecast the next 60 months using the fitted model -- this corresponds to the forecast for the test 
# values
plot(forecast(recursive_fit, h = 60)) # visualize that forecast

# create data frame to measure accuracy
recursive_obj <- data.frame(forecast(recursive_fit, h = 60))[1]
names(recursive_obj) <- c("fcst")
recursive_obj # forecasted values

# merge forecasted values w/ actual values corresponding to test data for comparison
test_2 <- cbind(recursive_obj,data$LandAverageTemperature[3133:3192]) # bind forecasted values w/ original data points
colnames(test_2)[2] <- "test"
test_2

# df for recursive forecast + residuals
test_2_resid <- test_2
test_2_resid$residuals_1 <- test_2$test-test_2$fcst
test_2_resid

# Recursive forecast MAPE
MAPE(y_pred = test_2$fcst, y_true = test_2$test) # 0.03904545


# --- model 3. Direct-Recursive Hybrid Multi-step Forecast Strategy
# involves using a one-step model multiple times where the prediction for the prior time step is used as an input for making 
# a prediction on following
# model would then be used to predict month 1, then this prediction would be used as an observation input in order to predict month 2
# because predictions are used in place of observations, this recursive strategy allows prediction errors to accumulate such that 
# performance can quickly degrade as the prediction time horizon increases
DR_fit <- Arima(train.data, order = c(0,0,1), list(order=c(0,1,1), period = 12)) # using params derived from auto.arima
DR_fit # AIC = 762.32   AICc = 762.34   BIC = 777.88  (same as above)

# good to look into residuals
plot.ts(DR_fit$residuals) # residuals look good; no particular structure; relatively consistent variance
qqnorm(DR_fit$residuals) # look fairly normal along line that cuts plot in half
acf(DR_fit$residuals) # can verify residuals have no structure for most part but visible lag component

# mean of residuals
mean(DR_fit$residuals) # 0.04

# actual direct recursive forecast
train2 <- read.csv('train.data.csv')
train2 <- train2[2]

train2 <- ts(train2, start = 1900, frequency = 12) # prepare train2 as univariate ts for passage into Arima in for loop

for (month in 1:60) {
  arima_model <- Arima(train2, order = c(0,0,1), list(order=c(0,1,1), period = 12)) # Arima order used to build direct recursive fcst
  arima_estimate <- forecast(arima_model, h = 1) # forecast w/ model, 1 step ahead
  arima_estimate$mean
  train2 <- rbind(train2, arima_estimate$mean) # append newly derived data point to fuel next forecast
  train2 <- ts(train2, frequency =  12, start = c(1900,1))
} # generating next 60 months of forecast
head(train2)
tail(train2, 60) # just forecast corresponding to test data set

test_forecast <- data.frame(tail(train2, 60)) # putting forecast corresponding to test data set into a data frame
names(test_forecast) <- c("fcst")
test_forecast

test_3 <- cbind(test_forecast,data$LandAverageTemperature[3133:3192]) # binding in test actuals to data frame
test_3

colnames(test_3)[2] <- "test"
test_3

test_3_resid <- test_3
test_3_resid$residuals_2 <- test_3$test-test_3$fcst # calculating the residuals
test_3_resid

# Direct Recursive forecast MAPE
MAPE(y_pred = test_3$fcst, y_true = test_3$test) # 0.03904929



# comparison table across models
models <- c('1','2','3')
AIC <- c('XX','XX','XX')
AICc <- c('XX','XX','XX')
BIC <- c('XX','XX','XX')

