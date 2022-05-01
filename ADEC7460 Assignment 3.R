library(tidyr)
library(forecast)
library(ggplot2)
library(fpp3)
library(fpp2)
library(fma)
library(readxl)
library(tsibble)
library(feasts)
library(seasonal)
library(fpp)
library(psych)
library(MASS)
library(fitdistrplus)
library(pscl)
library(boot)
library(ggplot2)
library(fGarch)
library(pROC)
library(censReg)
library(tscount)
library(randomForest)
library(mice)
library(caret)
library(tensorflow)
library(keras)
library(Amelia)
library(e1071)
library(xgboost)
library(AnalyzeFMRI)
library(EBImage)
library(ResourceSelection)
library(car)
library(adabag)
library(neuralnet)
library(class)
library(simmer)
library(simmer.plot)
library(parallel)
library(rugarch)

#Modeling the next year's worth of stock performance of Goldman Sachs
#(ticker GS) based on the past five years' closing prices
gs <- read.csv("/BC/ADEC7460.02 Spring 2022 Predictive AnalyticsForecasting/GS.csv", stringsAsFactors=TRUE)

#Formatting data
gs$Open <- NULL
gs$High <- NULL
gs$Low <- NULL
gs$Close <- NULL
gs$Volume <- NULL


#Checking out the plot
autoplot(ts(gs$Adj.Close)) +
  labs(title = "GS Stock Price",
       y = "Close")

#Creating ETS model
model_ets <- ets(gs$Adj.Close)
model_ets
#Selects A, N, N model based on lowest AIC

#Making the forecast for next year's worth of trading days
forecast_ets <- forecast(model_ets, h = 252)

#Checking accuracy metrics
accuracy(forecast_ets)

#Check residual plot for heteroskedasticity
plot(resid(model_ets))
#No real concerns here

#Check the decomp plot
plot(model_ets)

#Plot out the forecasts
plot(forecast_ets)
#Model expects somewhat flat performance from the stock going forward
#Wide error bars

#Creating ARIMA model
model_arima <- auto.arima(gs$Adj.Close)
model_arima
#Selects 0,1,0 model based on lowest AIC

#Making the forecast for next year's worth of trading days
forecast_arima <- forecast(model_arima, h = 252)

#Checking accuracy metrics
accuracy(model_arima)

#Check residual plot for heteroskedasticity
plot(resid(model_arima))
#No real concerns here

#Check the AR or MA roots
plot(model_arima)
#No roots to plot

#Plot out the forecasts
plot(forecast_arima)
#Model expects somewhat flat performance from the stock going forward
#Wide error bars

#Creating GARCH model
spec_garch <- ugarchspec(mean.model = list(armaOrder = c(1, 0)))
model_garch <- ugarchfit(spec_garch, data = gs$Adj.Close)
model_garch
#Selects sGARCH 1,1 model

#Making the forecast for next year's worth of trading days
forecast_garch <- ugarchforecast(model_garch, n.ahead = 252)

#Check residual plots for heteroskedasticity
plot(model_garch, which = "all")
#Slight issues, but overall not super concerning

#Plot out the forecasts
plot(forecast_garch, which = 1)
#Model expects somewhat flat performance from the stock going forward
#Narrow error bars

#Creating Neural Net model
model_neuralnet <- nnetar(gs$Adj.Close)
model_neuralnet
#Selects NNAR 1,1 model

#Making the forecast for next year's worth of trading days
forecast_neuralnet <- forecast(model_neuralnet, h = 252)

#Checking accuracy metrics
accuracy(model_neuralnet)

#Check residual plot for heteroskedasticity
plot(resid(model_neuralnet))
#No real concerns here

#Plot out the forecasts
plot(forecast_neuralnet)
#Model expects some growth in the next few months, flattening out afterwards

#Check out accuracy metrics for relevant models
accuracy(model_ets)
accuracy(model_arima)
accuracy(model_neuralnet)
#All are really close, especially ETS and ARIMA
#Makes sense considering how similar the forecasts are
#Neural net does slightly better in ME and ACF1, giving it the edge

#Compare the plots for the forecasts
par(mfrow=c(2,2))
plot(forecast_ets)
plot(forecast_arima)
plot(forecast_garch, which = 1)
plot(forecast_neuralnet)
#Neural net seems to capture the overall trend slightly better
#than the other models
#Given its slight edge in the accuracy metrics, I think I would use
#the NNAR 1,1 model to make predictions going forward
#The similarity indicates its certainly worth continuing to monitor
#the other modeling methods going forward to ensure the neural net
#remains the better option.
