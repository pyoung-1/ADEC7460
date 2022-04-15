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


#####CHAPTER 8
###QUESTION 1

vic_pigs <- aus_livestock %>% 
  filter(Animal == "Pigs", State == "Victoria")

model_ets <- vic_pigs %>% 
  model(ETS(Count ~ error("A") + trend("N") + season("N")))
report(model_ets)
#The optimal alpha is 0.3221247 and l[0] is 100646.6

forecast_ets <- model_ets %>% 
  forecast(h = 4)
forecast_ets

y_hat <- mean(forecast_ets$.mean)
first_month <- augment(model_ets)
st_dev <- sd(first_month$.resid)
upper_lim <- y_hat + st_dev * 1.96
lower_lim <- y_hat - st_dev * 1.96
upper_lim
lower_lim
hilo(forecast_ets, 95)[7]
#The R calculated confidence interval is slightly wider than the one
#I calculated

###QUESTION 2
#REVISIT
my_ses <- function(y, alpha, l0){
  y_hat2 <- l0
  for(index in 1:length(y)){
    y_hat2 <- alpha * y[index] + (1 - alpha) * y_hat2 
  }
  print(as.character(y_hat2))
}
alpha <- 0.3221247
l0 <- 100646.6
my_ses(vic_pigs, alpha = alpha, l0 = l0)
#This should be the same amount as R calculated function

###QUESTION 3

my_ses2 <- function(pars = c(alpha, l0), y){
  error <- 0
  sse <- 0
  alpha <- pars[1]
  l0 <- pars[2]
  y_hat <- l0
  
  for (index in 1:length(y)) {
    error <- y[index] - y_hat
    sse <- sse + error^2
    
    y_hat <- alpha * y[index] + (1 - alpha) * y_hat
  }
  
  return(sse)
}

optim(par = c(0.5, vic_pigs[1]), y = vic_pigs, fn = my_ses2)

###QUESTION 4

my_ses3 <- function(init_pars, data){
  forecast_next <- 0
  sse <- function(pars, data){
    error <- 0
    sse <- 0
    alpha <- pars[1]
    l0 <- pars[2]
    y_hat <- l0
    
    for (index in 1:length(data)) {
      error <- data[index] - y_hat
      sse <- sse + error^2
      
      y_hat <- alpha * data[index] + (1 - alpha) * y_hat
    }
    forecast_next <<- y_hat
    return(sse)
  }
  optim_pars <- optim(par = init_pars, data = data, fn = sse)
  return(list(
    next_obs_forecast = forecast_next,
    alpha = optim_pars$par[1],
    l0 = optim_pars$par[2]
  ))
}

my_ses3(c(0.5, vic_pigs[1]), vic_pigs)

###QUESTION 5

print(unique(global_economy$Country))

ie_exports <- global_economy %>% 
  filter(Country == "Ireland")

ie_exports %>% 
  autoplot(Exports) +
  labs(title = "Irish Annual Exports",
       x = "Year")
#There is a steady positive trend with exceptions around the dotcom crash in the
#early 2000s and the GFC in the mid to late 200s

model_exports <- ie_exports %>% 
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
forecast_exports <- model_exports %>% 
  forecast(h = 20)
forecast_exports %>% 
  autoplot(ie_exports) +
  labs(title = "Irish Annual Exports",
       x = "Year")
accuracy(model_exports)

model_exports2 <- ie_exports %>% 
  model(ETS(Exports ~ error("A") + trend("A") + season("N")))
accuracy(model_exports2)
#The AAN model outperforms the ANN model in RMSE (and in most other categories)
#The AAN model takes trend into account, which likely resulted in the better RMSE

forecast_exports2 <- model_exports2 %>% 
  forecast(h = 20)
forecast_exports2 %>% 
  autoplot(ie_exports) +
  labs(title = "Irish Annual Exports",
       x = "Year")
#The AAN forecast seems much better since it takes into account the overall trend

y_hat_ann <- mean(forecast_exports$.mean)
first_month_ann <- augment(model_exports)
st_dev_ann <- sd(first_month_ann$.resid)
upper_lim_ann <- y_hat_ann + st_dev_ann * 1.96
lower_lim_ann <- y_hat_ann - st_dev_ann * 1.96
y_hat_aan <- mean(forecast_exports2$.mean)
first_month_aan <- augment(model_exports2)
st_dev_aan <- sd(first_month_aan$.resid)
upper_lim_aan <- y_hat_aan + st_dev_aan * 1.96
lower_lim_aan <- y_hat_aan - st_dev_aan * 1.96
print(c(lower_lim_ann, upper_lim_ann))
print(c(lower_lim_aan, upper_lim_aan))

hilo(forecast_exports, 95)[6]
hilo(forecast_exports2, 95)[6]

###QUESTION 6

cn_gdp <- global_economy %>% 
  filter(Country == "China")

model_gdp_ann <- cn_gdp %>% 
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
forecast_gdp_ann <- model_gdp_ann %>% 
  forecast(h = 20)
forecast_gdp_ann %>% 
  autoplot(cn_gdp) +
  labs(title = "Chinese GDP ANN",
       x = "Year")
accuracy(model_gdp_ann)

model_gdp_aan <- cn_gdp %>% 
  model(ETS(Exports ~ error("A") + trend("A") + season("N")))
forecast_gdp_aan <- model_gdp_aan %>% 
  forecast(h = 20)
forecast_gdp_aan %>% 
  autoplot(cn_gdp) +
  labs(title = "Chinese GDP AAN",
       x = "Year")
accuracy(model_gdp_aan)

model_gdp_mnn <- cn_gdp %>% 
  model(ETS(Exports ~ error("M") + trend("N") + season("N")))
forecast_gdp_mnn <- model_gdp_mnn %>% 
  forecast(h = 20)
forecast_gdp_mnn %>% 
  autoplot(cn_gdp) +
  labs(title = "Chinese GDP MNN",
       x = "Year")
accuracy(model_gdp_mnn)

model_gdp_mmn <- cn_gdp %>% 
  model(ETS(Exports ~ error("M") + trend("M") + season("N")))
forecast_gdp_mmn <- model_gdp_mmn %>% 
  forecast(h = 20)
forecast_gdp_mmn %>% 
  autoplot(cn_gdp) +
  labs(title = "Chinese GDP MMN",
       x = "Year")
accuracy(model_gdp_mmn)

lambda <- cn_gdp %>%
  features(GDP, features = guerrero) %>%
  pull(lambda_guerrero)

forecast_gdp <- cn_gdp %>% 
  model(
    ets = ETS(GDP),
    ets_box_cox = ETS(box_cox(GDP, lambda)),
    ets_damped = ETS(GDP ~ trend("Ad", phi = 0.9)),
    ets_log = ETS(log(GDP)))

forecast_gdp %>% 
  forecast(h = 20) %>% 
  autoplot(cn_gdp) +
  labs(title = "Chinese GDP",
       x = "Year")

###QUESTION 7

model_gas <- aus_production %>% 
  model(ETS(Gas))
report(model_gas)
model_gas %>% 
  forecast(h = 20) %>% 
  autoplot(aus_production) +
  labs(title = "Gas Production")
#You need the multiplicative seasonality because the seasonal variation is trending
#upward over time

model_gas_damp <- aus_production %>% 
  model(ETS(Gas ~ error("M") + trend("Ad") + season("M")))
report(model_gas_damp)
model_gas_damp %>% 
  forecast(h = 20) %>% 
  autoplot(aus_production) +
  labs(title = "Gas Production Damped")
accuracy(model_gas)
accuracy(model_gas_damp)
#The damped model seems to perform about as well as the non-damped

###QUESTION 8

retail_series <- aus_retail %>% 
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))
retail_series %>% 
  autoplot(Turnover) +
  labs(x = "Month")
#You need the multiplicative seasonality because the seasonal variation is trending
#upward over time

model_retail <- retail_series %>% 
  model(holt_winters = ETS(Turnover ~ error("M") + trend("A") + season("M")),
        holt_winter_damped = ETS(Turnover ~ error("M") + trend("Ad") + season("M")))
forecast_retail <- model_retail %>% 
  forecast(h = 120)
forecast_retail %>% 
  autoplot(retail_series) +
  labs(x = "Month")
accuracy(model_retail)
#The RMSE is better for the damped, but it's a mixed bag across other metrics
#No clear preference

model_hw <- retail_series %>% 
  model(holt_winters = ETS(Turnover ~ error("M") + trend("A") + season("M")))
model_hw %>% 
  gg_tsresiduals()

model_hwd <- retail_series %>% 
  model(holt_winters_damped = ETS(Turnover ~ error("M") + trend("Ad") + season("M")))
model_hwd %>% 
  gg_tsresiduals()
#No issues with these residuals, all looks to be white noise

retail_train <- retail_series %>% 
  filter(year(Month) < 2011)

model_retail2 <- retail_train %>% 
  model(holt_winters = ETS(Turnover ~ error("M") + trend("A") + season("M")),
        holt_winter_damped = ETS(Turnover ~ error("M") + trend("Ad") + season("M")),
        seasonal_naive = SNAIVE(Turnover))
retail_compare <- anti_join(retail_series, retail_train, 
                            by = c("State", "Industry", "Series ID", "Month", "Turnover"))
retail_forecast <- model_retail2 %>% 
  forecast(retail_compare)
autoplot(retail_compare, Turnover) +
  autolayer(retail_forecast) +
  labs(x = "Month")
accuracy(model_retail2)
#The two Holt Winters models significantly outperform the seasonal naive model

###QUESTION 9

model_stl <- retail_train %>% 
  model(stl = STL(Turnover, lambda))
components(model_stl) %>% 
  autoplot()

model_stl_boxcox <- retail_train %>% 
  model(stl = STL(box_cox(Turnover, lambda)))
components(model_stl_boxcox) %>% 
  autoplot()
model_ets_boxcox <- retail_train %>% 
  model(ETS(box_cox(Turnover, lambda)))
accuracy(model_stl_boxcox)
accuracy(model_ets_boxcox)
#The STL model seems to do better on most metrics.

###QUESTION 10

au_trips <- tourism %>% 
  summarise(Trips = sum(Trips))
au_trips %>% 
  autoplot(Trips) +
  labs(x = "Quarter")
#The data are seasonal with a recent uptrend, it had been relatively stable before 2010

model_stl <- au_trips %>% 
  model(STL(Trips))
stl_decomp <- components(model_stl)
stl_decomp %>% 
  as_tsibble() %>% 
  autoplot(season_adjust) +
  labs(x = "Quarter")

au_trips %>% 
  model(decomposition_model(STL(Trips), 
                            ETS(season_adjust ~ error("A") + trend("Ad") + season("N")))) %>% 
                              forecast(h = 8) %>% 
                              autoplot(au_trips)

au_trips %>% 
  model(decomposition_model(STL(Trips), 
                            ETS(season_adjust ~ error("A") + trend("A") + season("N")))) %>% 
  forecast(h = 8) %>% 
  autoplot(au_trips)

au_trips %>% 
  model(ETS(Trips)) %>% 
  forecast(h = 8) %>% 
  autoplot(au_trips)

model_trips <- au_trips %>% 
  model(model_decomp_damp = decomposition_model(STL(Trips), 
                            ETS(season_adjust ~ error("A") + trend("Ad") + season("N"))),
        model_decomp = decomposition_model(STL(Trips), 
                            ETS(season_adjust ~ error("A") + trend("A") + season("N"))),
        model_ets = ETS(Trips))
accuracy(model_trips)    
#The AAN and AAdN models have the same RMSE
#Overall, the AAN model seems to do the best by a very slight margin

model_trips %>% 
  forecast(h = 8) %>% 
  autoplot(au_trips)
#They're all very very similar, hard to distinguish which is most reasonable


au_trips %>% 
  model(decomposition_model(STL(Trips), 
                            ETS(season_adjust ~ error("A") + trend("A") + season("N")))) %>% 
  gg_tsresiduals()

###QUESTION 11

nz_arrivals <- aus_arrivals %>% 
  filter(Origin == "NZ")
autoplot(nz_arrivals) +
  labs(title = "Arrivals from NZ",
       x = "Quarter")
#Seasonal data with overall upward trend

train_nz <- nz_arrivals %>% 
  filter(Quarter <= max(Quarter) - 8)
model_nz <- train_nz %>% 
  model(ETS(Arrivals))
forecast_nz <- model_nz %>% 
  forecast(h = 8)
forecast_nz %>% 
  autoplot() +
  autolayer(nz_arrivals, Arrivals) +
  labs(title = "Arrivals from NZ",
       x = "Quarter")
#You need the multiplicative seasonality because the seasonal variation is trending
#upward over time

compare_nz <- anti_join(nz_arrivals, train_nz, by = c("Quarter", "Origin", "Arrivals"))
model_nz2 <- train_nz %>% 
  model(ets_nz = ETS(Arrivals),
        additive_log = ETS(log(Arrivals) ~ error("A") + trend("A") + season("A")),
        seasonal_naive = SNAIVE(Arrivals),
        stl_decomp_log = decomposition_model(STL(log(Arrivals)), ETS(season_adjust)))
forecast_nz2 <- model_nz2 %>% 
  forecast(h = 8)
forecast_nz2 %>% 
  accuracy(nz_arrivals)
#The ETS model is the best across all metrics

train_nz %>% 
  model(ETS(Arrivals)) %>% 
  gg_tsresiduals()
#The residuals seem fine, no red flags

cv_nz <- nz_arrivals %>% 
  slice( 1: (n()-2)) %>% 
  stretch_tsibble(.init = 20,.step = 2)
model_nz3 <- cv_nz %>% 
  model(ets_nz = ETS(Arrivals),
        additive_log = ETS(log(Arrivals) ~ error("A") + trend("A") + season("A")),
        seasonal_naive = SNAIVE(Arrivals),
        stl_decomp_log = decomposition_model(STL(log(Arrivals)), ETS(season_adjust)))
forecast_nz3 <- model_nz3 %>% 
  forecast(h = 8)
forecast_nz3 %>% 
  accuracy(nz_arrivals)
#Same results

###QUESTION 12

cv_cement <- aus_production %>%
  slice(1:(n()-4)) %>%
  stretch_tsibble(.init = 20, .step = 1)

model_cement <- cv_cement %>%
  model(ETS(Cement), 
        SNAIVE(Cement)
  ) %>%
  forecast(h = 4)
model_cement %>%
  group_by(.id, .model) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  accuracy(aus_production, by = c(".model", "h"))
#ETS more accurate, confirms what I would have expected

###QUESTION 13

model_beer <- aus_production %>%
  slice(1:(nrow(aus_production)-12)) %>%
  model(ets_beer = ETS(Beer), 
    seasonal_naive_beer = SNAIVE(Beer),
    stl_decomp_beer = decomposition_model(STL(log(Beer)),
                                              ETS(season_adjust))) %>%
  forecast(h = 12)
model_beer %>% 
  accuracy(aus_production)
#ETS does best across most metrics

bricks <- aus_production %>% 
  filter(!is.na(Bricks))
model_bricks <- bricks %>%
  slice(1:(nrow(bricks)-12)) %>%
  model(ets_bricks = ETS(Bricks), 
    seasonal_naive_bricks = SNAIVE(Bricks),
    stl_decomp_bricks = decomposition_model(STL(log(Bricks)),
                                              ETS(season_adjust))) %>%
  forecast(h = 12)
model_bricks %>% 
  accuracy(bricks)
#ETS does best across most metrics

subsidies <- PBS %>%
  filter(ATC2 %in% c("A10", "H02")) %>%
  group_by(ATC2) %>%
  summarise(Cost = sum(Cost))
diabetes <- subsidies %>%
  filter(ATC2 %in% "A10")
lambda1 <- diabetes %>% 
  features(Cost, features = guerrero) %>% 
  pull(lambda_guerrero)
model_diabetes <- diabetes %>% 
  filter(Month < max(Month) - 35) %>% 
  model(ets_diabetes = ETS(Cost),
        seasonal_naive_diabetes = SNAIVE(Cost),
        stl_decomp_diabetes = decomposition_model(STL(box_cox(log(Cost), lambda1)),
                                                   ETS(season_adjust))) %>% 
  forecast(h = 36)

corticosteroids <- subsidies %>%
  filter(ATC2 %in% "H02")
lambda2 <- corticosteroids %>% 
  features(Cost, features = guerrero) %>% 
  pull(lambda_guerrero)
model_corticosteroids <- corticosteroids %>% 
  filter(Month < max(Month) - 35) %>% 
  model(ets_corticosteroids = ETS(Cost),
        seasonal_naive_corticosteroids = SNAIVE(Cost),
        stl_decomp_corticosteroids = decomposition_model(STL(box_cox(log(Cost), lambda2)),
                                                  ETS(season_adjust))) %>% 
  forecast(h = 36)

forecast_subsidies <- bind_rows(model_diabetes, model_corticosteroids)
forecast_subsidies %>% 
  accuracy(subsidies) %>% 
  arrange(ATC2)
#The STL decomp does the best across most metrics

au_food <- aus_retail %>% 
  filter(Industry == "Food retailing") %>% 
  summarise(Turnover = sum(Turnover))
lambda_food <- au_food %>% 
  features(Turnover, features = guerrero) %>% 
  pull(lambda_guerrero)
forecast_food <- au_food %>% 
  filter(Month < max(Month) - 35) %>% 
  model(ets_food = ETS(Turnover),
        seasonal_naive_food = SNAIVE(Turnover),
        stl_decomp_food = decomposition_model(STL(box_cox(log(Turnover), lambda_food)),
                                              ETS(season_adjust))) %>% 
  forecast(h = 36)
forecast_food %>% 
  accuracy(au_food)
#STL decomp does the best across most metrics

###QUESTION 14

au_trips <- tourism %>% 
  summarise(Trips = sum(Trips))
model_trips <- au_trips %>% 
  model(ETS(Trips))
model_trips %>% 
  forecast() %>% 
  autoplot(au_trips) +
  labs(title = "Trips Across Australia")
#Seems like reasonable forecasts

gafa <- gafa_stock %>% 
  group_by(Symbol) %>% 
  mutate(trading_day = row_number()) %>% 
  ungroup() %>% 
  as_tsibble(index = trading_day, regular = TRUE)
model_gafa <- gafa %>% 
  model(ETS(Close))
model_gafa %>% 
  forecast(h = 20) %>% 
  autoplot(gafa %>% 
             group_by_key() %>% 
             slice((n() - 100):n())) +
  labs(title = "Prices")
#Not great forecasts here

model_pelt <- pelt %>% 
  model(ETS(Lynx)) 
model_pelt %>%
  forecast() %>% 
  autoplot(pelt) +
  labs(title = "Pelt")
#Not great here as well

###QUESTION 15

deaths_ets <- ets(usdeaths, model = "MAM")
forecast_deaths <- forecast(deaths_ets, h = 1)
deaths_hw <- hw(usdeaths, seasonal = 'multiplicative', h = 1)
forecast_deaths$mean
deaths_hw$mean

###QUESTION 16

deaths_ann <- ets(usdeaths, model = "ANN")
forecast_deaths_ann <- forecast(deaths_ann, 1)
summary(forecast_deaths_ann)
deaths_sigma <- 735.891
deaths_alpha <- 0.9999^2
deaths_h <- 2
deaths_conf_int <- deaths_sigma * (1 + deaths_alpha * (deaths_h - 1))
forecast_deaths_ann$mean[1] - deaths_conf_int
forecast_deaths_ann$lower[1, '95%']

###QUESTION 17

#sigma^2[1 + alpha^2(h ??? 1)]



#####CHAPTER 9
###QUESTION 1

#The acf bands are becoming tighter as the amount of random numbers increases
#Since all the data are within the bands, we are confident it's white noise
#The critical values are different distances because they are based on the 
#length of the time series.  As the count increases, the bands tighten.

###QUESTION 2

amzn <- gafa_stock %>%
  filter(Symbol == "AMZN")

amzn %>%
  gg_tsdisplay(Close) +
  labs(title = "Amazon Price")

acf(amzn$Close, plot = FALSE)
pacf(amzn$Close, plot = FALSE)

#The autocorrelation values seem to decrease over time
#The data also show a positive trend up until late 2018, then a sharper decline

###QUESTION 3

tr_gdp <- global_economy %>% 
  filter(Country == "Turkey")
lambda_tr_gdp <- tr_gdp %>% 
  features(GDP, features = guerrero) %>% 
  pull(lambda_guerrero)
tr_gdp %>% 
  features(box_cox(GDP, lambda_tr_gdp), unitroot_ndiffs)

tasmania_acc <- aus_accommodation %>% 
  filter(State == "Tasmania")
lambda_tasmania_acc <- tasmania_acc %>% 
  features(Takings, features = guerrero) %>% 
  pull(lambda_guerrero)
tasmania_acc %>% 
  features(box_cox(Takings, lambda_tasmania_acc), unitroot_ndiffs)

lambda_souvenirs <- souvenirs %>% 
  features(Sales, features = guerrero) %>% 
  pull(lambda_guerrero)
souvenirs %>% 
  features(box_cox(Sales, lambda_souvenirs), unitroot_ndiffs)

###QUESTION 4

#y(t) = (1 - B^12) * y(t)

###QUESTION 5

lambda_retail <- retail_series %>% 
  features(Turnover, features = guerrero) %>% 
  pull(lambda_guerrero)
retail_series %>% 
  features(box_cox(Turnover, lambda_retail), unitroot_ndiffs)

###QUESTION 6

y <- numeric(100)
e <- rnorm(100)
for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]
sim <- tsibble(idx = seq_len(100), y = y, index = idx)

sim %>% 
  autoplot(y) +
  labs(title = "Phi1 = 0.6")

for(i in 2:100)
  y[i] <- 1*y[i-1] + e[i]
tsibble(idx = seq_len(100), y = y, index = idx) %>%
  autoplot(y) +
  labs(title = "Phi1 = 1")

for(i in 2:100)
  y[i] <- 0*y[i-1] + e[i]
tsibble(idx = seq_len(100), y = y, index = idx) %>%
  autoplot(y) +
  labs(title = "Phi1 = 0")

for(i in 2:100)
  y[i] <- -0.6*y[i-1] + e[i]
tsibble(idx = seq_len(100), y = y, index = idx) %>%
  autoplot(y) +
  labs(title = "Phi1 = -0.6")

for(i in 2:100)
  y[i] <- e[i] + 0.6 * e[i-1]
sim2 <- tsibble(idx = seq_len(100), y = y, index = idx)

sim2 %>%
  autoplot(y) +
  labs(title = "Theta1 = 0.6")

for(i in 2:100)
  y[i] <- e[i] + 1 * e[i-1]
tsibble(idx = seq_len(100), y = y, index = idx) %>%
  autoplot(y) +
  labs(title = "Theta1 = 1")

for(i in 2:100)
  y[i] <- e[i] + 0 * e[i-1]
tsibble(idx = seq_len(100), y = y, index = idx) %>%
  autoplot(y) +
  labs(title = "Theta1 = 0")

for(i in 2:100)
  y[i] <- e[i] + -0.6 * e[i-1]
tsibble(idx = seq_len(100), y = y, index = idx) %>%
  autoplot(y) +
  labs(title = "Theta1 = -0.6")

y <- numeric(100)
for(i in 2:100)
  y[i] <- e[i] + 0.6 * e[i-1] + 0.6 * y[i-1]
arma_1_1 <- tsibble(idx = seq_len(100), y = y, index = idx)

y <- numeric(100)
for(i in 3:100)
  y[i] <- -0.8 * y[i-1] + 0.3 * y[i-2] + e[i]
ar_2 <- tsibble(idx = seq_len(100), y = y, index = idx)

arma_1_1 %>%
  gg_tsdisplay(y, plot_type='partial') +
  labs(title = "ARMA(1,1)")

ar_2 %>%
  gg_tsdisplay(y, plot_type='partial') +
  labs(title ="AR(2)")

###QUESTION 7

model_passengers <- aus_airpassengers %>% 
  filter(Year <= 2011) %>% 
  model(ARIMA(Passengers))
report(model_passengers)

model_passengers %>% 
  forecast(h = 10) %>% 
  autoplot(aus_airpassengers) +
  labs(title = "AU Air Passengers")

model_passengers %>% 
  gg_tsresiduals()

#yt = -0.87et-1 + et
#(1- B)^2 * yt = (1 - 0.87B) * et

model_passengers_010 <- aus_airpassengers %>% 
  filter(Year <= 2011) %>%
  model(ARIMA(Passengers ~ pdq(0, 1, 0)))

model_passengers_010 %>% 
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "AU Air Passengers")
#This model forecast lower values than the 0, 2, 1 model

model_passengers_212 <- aus_airpassengers %>% 
  filter(Year <= 2011) %>%
  model(ARIMA(Passengers ~ pdq(2, 1, 2)))

model_passengers_212 %>% 
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "AU Air Passengers")
#This model forecast higher values than the 0, 1, 0, pretty close to the 0, 2, 1

model_passengers_212_noconstant <- aus_airpassengers %>% 
  filter(Year <= 2011) %>%
  model(ARIMA(Passengers ~ 0 + pdq(2, 1, 2)))

model_passengers_212_noconstant %>% 
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "AU Air Passengers")
#Won't plot

report(model_passengers_212_noconstant)

model_passengers_021_constant <- aus_airpassengers %>% 
  filter(Year <= 2011) %>%
  model(ARIMA(Passengers ~ 1 + pdq(0, 2, 1)))

model_passengers_021_constant %>% 
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "AU Air Passengers")
#Seems to project the highest forecasts of all

###QUESTION 8

us_gdp <- global_economy %>% 
  filter(Code == "USA")
lambda_us_gdp <- us_gdp %>% 
  features(GDP, features = guerrero) %>% 
  pull(lambda_guerrero)

model_gdp <- us_gdp %>% 
  model(ARIMA(box_cox(GDP, lambda_us_gdp)))
report(model_gdp)

model_gdp_011 <- us_gdp %>% 
  model(ARIMA(box_cox(GDP, lambda_us_gdp) ~ pdq(0, 1, 1)))
report(model_gdp_011)

model_gdp_111 <- us_gdp %>% 
  model(ARIMA(box_cox(GDP, lambda_us_gdp) ~ pdq(1, 1, 1)))
report(model_gdp_111)

model_gdp_121 <- us_gdp %>% 
  model(ARIMA(box_cox(GDP, lambda_us_gdp) ~ pdq(1, 2, 1)))
report(model_gdp_121)

accuracy(model_gdp)
accuracy(model_gdp_011)
accuracy(model_gdp_101)
accuracy(model_gdp_121)
#The original 1, 1, 0 seems to do the best

model_gdp %>% 
  gg_tsresiduals()
#residual plot not great, not evenly distributed

model_gdp %>% 
  forecast(h = 20) %>% 
  autoplot(us_gdp)
#The forecasts do look pretty good, on trend

model_gdp_ets <- us_gdp %>% 
  model(ETS(GDP))
model_gdp_ets %>% 
  forecast(h = 20) %>% 
  autoplot(us_gdp)
#The confidence intervals are far wider, doesn't seem to capture the trend as well

###QUESTION 9

jp_arrivals <- aus_arrivals %>% 
  filter(Origin == "Japan")

jp_arrivals %>% 
  autoplot() +
  labs(title = "JP Arrivals",
       x = "Quarter")

ggAcf(jp_arrivals)
ggPacf(jp_arrivals)
#Both are pretty far outside the bounds

model_jp_arrivals <- jp_arrivals %>% 
  model(ARIMA(Arrivals))
report(model_jp_arrivals)

###QUESTION 10

employment_private <- us_employment %>% 
  filter(Title == "Total Private")

employment_stl <- employment_private %>% 
  model(STL(Employed))

employment_stl %>% 
  components() %>% 
  autoplot()
#Definite uptrend, seasonal variation, big exogenous drop in WW2, mid 70s, and GFC
#Data do not seem to need transformation

employment_private %>% 
  features(Employed, unitroot_kpss)
#kpss at 12.2, not stationary

employment_private %>% 
  mutate(dif_employed = difference(Employed)) %>%
  features(dif_employed, unitroot_kpss)

employment_private %>% 
  features(Employed, unitroot_ndiffs)

employment_private %>% 
  mutate(log_employed = log(Employed)) %>%
  features(log_employed, unitroot_nsdiffs)

employment_private %>% 
  mutate(log_employed = difference(log(Employed), 12)) %>%
  features(log_employed, unitroot_ndiffs)

employment_model <- employment_private %>% 
model(arima_011 = ARIMA(Employed ~ pdq(0, 1, 1)),
      arima_111 = ARIMA(Employed ~ pdq(1, 1, 1)),
      arima_110 = ARIMA(Employed ~ pdq(1, 1, 0)),
      arima_212 = ARIMA(Employed ~ pdq(2, 1, 2)))
report(employment_model)

employment_arima <- employment_private %>% 
  model(ARIMA(Employed))
report(employment_arima)
employment_arima %>% 
  gg_tsresiduals()
#Everything looks ok to me, seems like resids are white noise

employment_arima %>% 
  forecast(h = 36) %>% 
  autoplot(employment_private)
#Forecast slope is maybe a little low, also hard to predict the big drops we see
#Very few I think, hard to predict the big drops that recessions bring about
#If we could predict them, we would prevent them

###QUESTION 11

aus_cement <- tsibble(aus_production[1], aus_production[5])
aus_cement %>% 
  features(Cement, unitroot_kpss)

model_cement <- aus_cement %>% 
  model(arima_011 = ARIMA(Cement ~ pdq(0, 1, 1)),
        arima_111 = ARIMA(Cement ~ pdq(1, 1, 1)),
        arima_110 = ARIMA(Cement ~ pdq(1, 1, 0)),
        arima_212 = ARIMA(Cement ~ pdq(2, 1, 2)))
report(model_cement)
#The 0, 1, 1 and 1, 1, 0 have the lowest AIC

cement_arima <- aus_cement %>% 
  model(ARIMA(Cement))
report(cement_arima)
cement_arima %>% 
  gg_tsresiduals()
#Seem ok to me

cement_arima %>% 
  forecast(h = 8) %>% 
  autoplot(aus_cement)

cement_ets <- aus_cement %>% 
  model(ETS(Cement))
cement_ets %>% 
  forecast(h = 8) %>% 
  autoplot(aus_cement)
#ARIMA seems better to me

###QUESTION 12

cement_stl <- aus_cement %>% 
  model(stl = STL(Cement))
cement_stl %>% 
  forecast(h = 8) %>% 
  autoplot(aus_cement)

###QUESTION 13

trips_arima <- au_trips %>% 
  model(ARIMA(Trips))
trips_arima %>% 
  forecast(h = 20) %>% 
  autoplot(au_trips)
#The forecasts do look reasonable the trend slope could be higher I think

trips_snowy <- tourism %>%
  filter(Region == "Snowy Mountains") %>% 
  summarise(Trips = sum(Trips))
snowy_arima <- trips_snowy %>% 
  model(ARIMA(Trips))
snowy_arima %>% 
  forecast(h = 20) %>% 
  autoplot(trips_snowy)

trips_melbourne <- tourism %>%
  filter(Region == "Melbourne") %>% 
  summarise(Trips = sum(Trips))
melbourne_arima <- trips_melbourne %>% 
  model(ARIMA(Trips))
melbourne_arima %>% 
  forecast(h = 20) %>% 
  autoplot(trips_melbourne)
#The Snowy Mountains forecast looks way more reasonable than the Melbourne forecast
#Melbourne doesn't seem to be following trend

###QUESTION 14

lambda_retail <- retail_series %>% 
  features(Turnover, features = guerrero) %>% 
  pull(lambda_guerrero)
retail_series %>% 
  features(box_cox(Turnover, lambda_retail), unitroot_ndiffs)

retail_arima <- retail_series %>% 
  model(ARIMA(Turnover))
retail_arima %>% 
  forecast(h = 48) %>% 
  autoplot(retail_series)
#The forecast doesn't look great compared to other models, but none actually seemed
#to correctly forecast the retail sales

###QUESTION 15

pelt %>% 
  autoplot(Hare) +
  labs(title = "Snowshoe Hare Furs",
       x = "Year",
       y = "Furs")

acf(pelt$Hare, plot = FALSE)
pacf(pelt$Hare, plot = FALSE)
#The autocorrelation seems to be decreasing over time

30993 + 0.82 * 15760 - 0.29 * 81660 - 0.01 * 89760 ??? 0.22 * 82110
30993 + 0.82 * 1273 - 0.29 * 15760 - 0.01 * 81660 ??? 0.22 * 89760
30993 + 0.82 * 6902.66 - 0.29 * 1273 - 0.01 * 15760 ??? 0.22 * 81660

pelt_arima <- pelt %>% 
  model(ARIMA(Hare))
pelt_arima %>% 
  forecast(h = 3)
#I think the difference is the error term being missed when manually calculated

###QUESTION 16

swiss_data <- global_economy %>% 
  filter(Country == "Switzerland")
swiss_data %>% 
  autoplot(Population) +
  labs(title = "Swiss Population",
       x = "Year")

swiss_arima <- swiss_data %>% 
  model(ARIMA(Population))

acf(swiss_data$Population, plot = FALSE)
pacf(swiss_data$Population, plot = FALSE)
#The autocorrelation seems to be decreasing over time

0.0053 + 8.47 + 1.64 * (8.47 - 8.37) - 1.17 * (8.37 - 8.28) + 0.45 * (8.28 - 8.19)
0.0053 + 8.47 + 1.64 * (8.5745 - 8.47) - 1.17 * (8.47 - 8.37) + 0.45 * (8.37 - 8.28)
0.0053 + 8.47 + 1.64 * (8.57018 - 8.5745) - 1.17 * (8.5745 - 8.47) + 0.45 * (8.47 - 8.37)

swiss_arima %>% 
  forecast(h = 3)
#I think the difference is the error term being missed when manually calculated

###QUESTION 17

install.packages("Quandl")
library(Quandl)

y <- as_tsibble(Quandl::Quandl("QUOTEMEDIA/PRICES"), index = Date)
#Couldn't get this method to work, downloaded and imported instead


QUOTEMEDIA_PRICES <- read_csv("C:/Users/PYoung/Downloads/QUOTEMEDIA-PRICES.csv")
xom_prices <- QUOTEMEDIA_PRICES %>% 
  filter(ticker == "XOM")
xom_ts <- as_tsibble(xom_prices, index = date)

xom_ts %>% 
  autoplot(close) + 
  labs(title = "XOM Closing Prices",
       x = "Date",
       y = "Price")

xom_arima <- xom_ts %>% 
  model(ARIMA(close))

xom_arima %>% 
  gg_tsresiduals()

plot(resid(xom_arima))

xom_arima_forecast <- xom_arima %>% 
  forecast(h = 1000)

xom_arima_forecast %>% 
  autoplot(xom_ts) +
  labs(title = "XOM Closing Prices",
       x = "Date",
       y = "Price")

xom_ets <- xom_ts %>% 
  model(ETS(close))

xom_ets %>% 
  gg_tsresiduals()

plot(resid(xom_ets))

xom_ets_forecast <- xom_ets %>% 
  forecast(h = 1000)

xom_ets_forecast %>% 
  autoplot(xom_ts) +
  labs(title = "XOM Closing Prices",
       x = "Date",
       y = "Price")
#These are both predicting 1000 days of data based on a training set of 42 days
#They are both likely to be bad predictors