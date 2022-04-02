library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
library(tidyverse)
library(seasonal)
library(feasts)
library(seasonal)
library(tsibble)
library(fable)
library(fpp3)
library(fpp2)

#CHAPTER 3
#Question 1
global_economy %>%
  tsibble(key = Code, index = Year) %>%
  autoplot(GDP / Population, show.legend =  FALSE) +
  labs(title= "GDP Per Capita",
       y = "GDP", 
       x = "Year")

global_economy %>%
  mutate(gdp_per_capita = GDP / Population) %>%
  filter(gdp_per_capita == max(gdp_per_capita, na.rm = TRUE)) %>%
  select(Country, gdp_per_capita)

global_economy %>%
  filter(Country == "Monaco") %>%
  autoplot(GDP / Population) +
  labs(title= "Monaco GDP Per Capita",
       y = "GDP",
       x = "Year")


#Question 2
global_economy %>%  #NO TRANSFORMATION, TREND VISIBLE AS IS
  filter(Country == "United States") %>%
  autoplot(GDP) +
  labs(title= "United States GDP", 
       y = "GDP",
       x = "Year") 

aus_livestock %>% #NO TRANSFORMATION, TREND VISIBLE AS IS
  filter(Animal == "Bulls, bullocks and steers",
         State == "Victoria") %>%
  autoplot(Count) +
  labs(title= "Slaughter of Victorian “Bulls, bullocks and steers”",
       x = "Month") 

vic_elec %>%  #TRANSFORMED TO MONTHLY DATA, ORIGINALLY TOO CONFUSING
  group_by(Date) %>%
  mutate(Demand = sum(Demand)) %>%
  distinct(Date, Demand) %>% 
  mutate(Date = yearmonth(Date)) %>%
  group_by(Date) %>%
  summarise(Demand = sum(Demand)) %>%
  as_tsibble(index = Date) %>%
  autoplot(Demand) +
  labs(title= "Monthly Victorian Electricity Demand", 
       y = "Demand",
       x = "Month")

aus_production %>% #NO TRANSFORMATION, TREND VISIBLE AS IS
  autoplot(Gas) +
  labs(title = "Gas production",
       y = "Production",
       x = "Quarter")


#Question 3
canadian_gas %>%
  autoplot(Volume) +
  labs(title = "Non-Transformed",
       y = "Volume",
       x = "Month")

lambda_gas <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

canadian_gas %>%
  autoplot(box_cox(Volume, lambda_gas)) +
  labs(title = "Transformed with Lambda 0.39",
       y = "Volume",
       x = "Month")
#UNHELPFUL BECAUSE IT DOES NOT MAKE SEASONALITY UNIFORM
#GREATER VARIATION IN LATE 70S, THROUGH EARLY 90S


#Question 4
series <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1)) 

series %>% 
  autoplot(Turnover)+
  labs(title = "Retail Turnover", 
       y = "Turnover",
       x = "Month")

lambda_turnover <- series %>% 
  features(Turnover, features = guerrero) %>% 
  pull(lambda_guerrero)

series %>% 
  autoplot(box_cox(Turnover, lambda_turnover)) +
  labs(title = "Transformed with Lambda -0.29",
       y = "Turnover",
       x = "Month")
#I WOULD USE A BOX COX TRANSFORMATION OF LAMBDA -0.287633917752507


#Question 5
lambda_tobacco <- aus_production %>%
  features(Tobacco, features = guerrero) %>%
  pull(lambda_guerrero)

aus_production %>%
  autoplot(box_cox(Tobacco, lambda_tobacco)) +
  labs(title = "Transformed with Lambda 0.93",
       y = "Production",
       x = "Quarter")

lambda_passengers <- ansett %>%
  filter(Class == "Economy",
         Airports == "MEL-SYD") %>%
  features(Passengers, features = guerrero) %>%
  pull(lambda_guerrero)

ansett %>% 
  filter(Class == "Economy",
         Airports == "MEL-SYD")%>%
  autoplot(box_cox(Passengers, lambda_passengers)) +
  labs(title = "Transformed with Lambda 2.00",
       y = "Passengers",
       x = "Week")

lambda_pedestrian <- pedestrian %>%
  filter(Sensor == "Southern Cross Station") %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)

pedestrian %>%
  filter(Sensor == "Southern Cross Station") %>%
  autoplot(box_cox(Count, lambda_pedestrian)) +
  labs(title = "Transformed with Lambda 0.93",
       y = "Pedestrians",
       x = "Hour")


#Question 6
#1/15(Y1) 2/15(Y2) 3/15(Y3) 3/15(Y4) 3/15(Y5) 2/15(Y6) 1/15(Y7) == 0.067 0.133 0.2 0.2 0.2 0.133 0.067


#Question 7
gas <- tail(aus_production, 5*4) %>% select(Gas)

gas %>%
  autoplot(Gas) +
  labs(title = "Gas Production",
       y = "Gas",
       x = "Quarter")
#THERE SEEMS TO BE A YEARLY CYCLE, SLIGHT UP TREND YEAR OVER YEAR

gas %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  autoplot(Gas) + 
  labs(title = "Gas Production",
       y = "Gas",
       x = "Quarter")
#YES, THE TREND GRAPH IS INCREASING AND THE SEASONAL GRAPH IS CYCLICAL

gas_decomp <- gas %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components()

gas_decomp %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Production")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonal")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Gas",
       title = "Gas Production") +
  scale_colour_manual(
    values = c("red", "blue", "green"),
    breaks = c("Production", "Seasonal", "Trend"))

gas_outlier <- gas

gas_outlier$Gas[15] <- gas_outlier$Gas[15] + 300

gas_outlier %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  autoplot(Gas) +
  labs(title = "Gas Production",
       y = "Gas",
       x = "Quarter")
#SEASONALITY NO LONGER CLEAR CYCLICAL, HAS A LITTLE DOWN TREND MID SEASON
#TREND APPEARS TO NOW BE ON THE DOWN TREND WHERE IT WAS UP PREVIOUSLY

gas_outlier_beg <- gas

gas_outlier_beg$Gas[2] <- gas_outlier_beg$Gas[2] + 300

gas_outlier_beg %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  autoplot(Gas) +
  labs(title = "Gas Production",
       y = "Gas",
       x = "Quarter")

gas_outlier_end <- gas

gas_outlier_end$Gas[18] <- gas_outlier_beg$Gas[18] + 300

gas_outlier_end %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  autoplot(Gas) +
  labs(title = "Gas Production",
       y = "Gas",
       x = "Quarter")
#IT CHANGES THE TREND AROUND A LOT, BUT DOESN'T HAVE AS MUCH IMPACT ON THE SEASONAL DATA


#Question 8
series %>%
  model(x11 = X_13ARIMA_SEATS(Turnover ~ x11())) %>%
  components() %>%
  autoplot() + 
  labs(title = "Retail Turnover",
       y = "Turnover",
       x = "Month")
#THERE APPEARS TO BE A LARGE DROP IN TREND IN THE LATE 2000S
#THERE IS ALSO A LOT OF IRREGULAR ACTIVITY MID 1990S


#Question 9
#THE TREND WAS ON A RELATIVELY STEADY INCREASE; THE ONLY SLIGHTLY FLAT PERIOD WAS IN THE
#EARLY 1990S BUT THEN PICKED UP TOWARDS THE MID 1990S.
#THERE ARE LARGE DOWNWARD SPIKES IN THE REMAINDER PERIOD IN THE EARLY 1990S AS WELL.
#THE RECESSION IS VERY VISIBLE IN THE REMAINDER CHART AS A DOWNWARD SPIKE


#Question 10
canadian_gas %>%
  autoplot(Volume) +
  labs(title = "Canadian Gas Production",
       y = "Volume",
       x = "Month")

canadian_gas %>%
  gg_subseries(Volume) +
  labs(title = "Canadian Gas Production",
       y = "Volume",
       x = "Month")

canadian_gas %>%
  gg_season(Volume) +
  labs(title = "Canadian Gas Production",
       y = "Volume",
       x = "Month")

canadian_gas %>%
  model(STL(Volume ~ trend(window = 20) +
          season(window = 20),
        robust = TRUE)) %>%
  components() %>%
  autoplot(Volume) +
  labs(title = "Canadian Gas Production",
       y = "Volume",
       x = "Month")
#THE SEASONAL VARIATION GROWS LARGER FROM THE MID 1970S TO THE MID 1980S, THEN DECREASES AGAIN

canadian_gas %>%
  model(STL(Volume ~ trend(window = 20) +
          season(window = 20),
        robust = TRUE)) %>%
  components() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Volume, colour = "Volume")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonal")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(title = "Canadian Gas Production") +
  scale_colour_manual(
    values = c("red", "blue", "green"),
    breaks = c("Volume", "Seasonal", "Trend"))

canadian_gas %>%
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>%
  components() %>%
  autoplot(Volume) +
  labs(title = "Canadian Gas Production")

canadian_gas %>%
  model(seats = X_13ARIMA_SEATS(Volume ~ seats())) %>%
  components() %>%
  autoplot(Volume) +
  labs(title ="Canadian Gas Production")
#THE SEASONAL AND TREND ARE VERY SIMILAR IN BOTH METHODS, NOT MUCH DIFFERENCE
#THE IRREGULAR IS MUCH LARGER IN THE SEATS THAN IN THE X11



#CHAPTER 7
#Question 1
jan14_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  index_by(Date = as_date(Time)) %>%
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

autoplot(jan14_vic_elec) + 
  labs(title = "Electricity Demand",
       y = "Volume",
       x = "Day")

model <- lm(Demand ~ Temperature, data = jan14_vic_elec)
summary(model)
#THERE IS A POSITIVE RELATIONSIHP BECAUSE AS TEMPERATURE GOES UP, THE DEMAND FOR AIR
#CONDITIONING (AND THEREFORE ELECTRICITY) GOES UP

plot(resid(model))
#THERE DOES NOT SEEM TO BE ANY PATTERN OR OUTLIERS TO THE RESIDUALS, INDICATING THAT THE MODEL
#IS ADEQUATE

jan14_vic_elec %>%
  model(TSLM(Demand ~ Temperature)) %>%
  forecast(
    new_data(jan14_vic_elec, 1) %>%
      mutate(Temperature = 15)
  ) %>%
  autoplot(jan14_vic_elec)

prediction <- forecast(model, data.frame(Temperature = c(15, 35)))
prediction
#THESE PREDICTIONS SEEM TO BE BELIEVABLE; THEY ARE IN RANGE OF THE DATA

prediction$upper[,1]
prediction$lower[,1]

vic_elec %>%
  as.data.frame() %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  ylab("Electricity Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#THE SHAPE OF THE DATA IS NOT LINEAR; THE LINEAR MODEL DOESN'T SEEM TO ADEQUATELY CAPTURE
#THE RELATIONSHIP


#Question 2
olympic_running %>% 
autoplot(Time) +
  labs(title = "Winning Times",
       y = "Time",
       x = "Year")
#THE TIMES GO DOWN EACH YEAR, THE BIGGEST DECREASE IS AT THE BEGINNING OF THE EVENTS

olympic_100 <- olympic_running %>% 
  filter(Length == "100")
model_100 <- lm(Time ~ Year, data = olympic_100)
summary(model_100)

olympic_200 <- olympic_running %>% 
  filter(Length == "200")
model_200 <- lm(Time ~ Year, data = olympic_200)
summary(model_200)

olympic_400 <- olympic_running %>% 
  filter(Length == "400")
model_400 <- lm(Time ~ Year, data = olympic_400)
summary(model_400)

olympic_800 <- olympic_running %>% 
  filter(Length == "800")
model_800 <- lm(Time ~ Year, data = olympic_800)
summary(model_800)

olympic_1500 <- olympic_running %>% 
  filter(Length == "1500")
model_1500 <- lm(Time ~ Year, data = olympic_1500)
summary(model_1500)

olympic_5000 <- olympic_running %>% 
  filter(Length == "5000")
model_5000 <- lm(Time ~ Year, data = olympic_5000)
summary(model_5000)

olympic_10000 <- olympic_running %>% 
  filter(Length == "10000")
model_10000 <- lm(Time ~ Year, data = olympic_10000)
summary(model_10000)

plot(resid(model_100))
plot(resid(model_200))
plot(resid(model_400))
plot(resid(model_800))
plot(resid(model_1500))
plot(resid(model_5000))
plot(resid(model_10000))
#RESIDUALS U-SHAPED, NOT EVENLY DISTRIBUTED MEANING THE MODELS ARE NOT ADEQUATE

predict_100 <- forecast(model_100, data.frame(Year = 2020))
predict_100
predict_200 <- forecast(model_200, data.frame(Year = 2020))
predict_200
predict_400 <- forecast(model_400, data.frame(Year = 2020))
predict_400
predict_800 <- forecast(model_800, data.frame(Year = 2020))
predict_800
predict_1500 <- forecast(model_1500, data.frame(Year = 2020))
predict_1500
predict_5000 <- forecast(model_5000, data.frame(Year = 2020))
predict_5000
predict_10000 <- forecast(model_10000, data.frame(Year = 2020))
predict_10000

#Question 3
#dy/y = Beta1(dx/x)
#Beta1 = (dy/y) * (x/dx)
#Beta1 = (dy/dx) * (x/y)


#Question 4
souvenirs %>% 
  autoplot(Sales) +
  labs(title = "Monthly Sales",
       y = "Sales",
       x = "Year")
#SEASONALLY, SALES SEEM TO INCREASE EVERY DECEMBER; THE OVERALL TREND SEEMS TO BE GROWING 
#EVERY YEAR

#THE DATA SEEM TO BE NON-LINEAR, SO THEY ARE NOT GROWING AT THE SAME PACE EVERY YEAR
#THIS WOULD LEAD TO INACCURATE FORECASTS IF NOT LOG TRANSFORMED

fit_souvenirs <- souvenirs %>% 
  model(TSLM(Sales ~ trend() + season()))
report(fit_souvenirs)

resid(fit_souvenirs) %>% 
  autoplot() +
  labs(title = "Residuals",
       y = "Sales",
       x = "Year")
#THERE DOES SEEM TO BE SOME HETEROSKEDACITY IN THE RESIDUALS, INDICATING AN ISSUE WITH THE MODEL

res_souvenirs <- resid(fit_souvenirs)

fit_souvenirs

boxplot(res_souvenirs$Month)

#THE BOXPLOTS SHOW THAT THE VARIATION DIFFERS FROM MONTH TO MONTH, IT IS NOT CONSISTENT

#THE COEFFICIENTS ARE THE EXPECTED SALES NUMBERS FOR EACH MONTH

#LARGE LJUNG-BOX VALUES SUGGEST THAT AUTOCORRELATIONS DO NOT COME FROM A WHITE NOISE SERIES

pred_souvenirs <-forecast(fit_souvenirs, newdata = data.frame(dummy_fest = rep(0, 36)))
pred_souvenirs

#THIS COULD BE IMPROVED WITH THE INCLUSION OF OTHER VARIABLES, OR USING A NON-LINEAR MODEL
#SINCE THE DATA SEEMS TO BE NON-LINEAR (LIKE EXPONENTIAL)


#Question 5
ts_gasoline <- us_gasoline %>% 
  mutate(Week = yearweek(Week)) %>% 
  as_tsibble(index = Week)

gas_2004 <- window(ts_gasoline, end = 2005)

harmonic_gas <- tslm(gas_2004 ~ trend + fourier(gas_2004, K = 1))
autoplot(gas_2004, series = "Data") +
  autolayer(fitted(harmonic_gas), series = "Fitted") +
  xlab("Year") + ylab("Gas Supply") +
  ggtitle("Gas Supply")

harmonic_gas2 <- tslm(gas_2004 ~ trend + fourier(gas_2004, K = 2))
autoplot(gas_2004, series = "Data") +
  autolayer(fitted(harmonic_gas), series = "Fitted") +
  xlab("Year") + ylab("Gas Supply") +
  ggtitle("Gas Supply")

harmonic_gas3 <- tslm(gas_2004 ~ trend + fourier(gas_2004, K = 3))
autoplot(gas_2004, series = "Data") +
  autolayer(fitted(harmonic_gas), series = "Fitted") +
  xlab("Year") + ylab("Gas Supply") +
  ggtitle("Gas Supply")

for(j in 1:20){
  fit_harmonic <- tslm(gas_2004 ~ trend + fourier(gas_2004, K = j))
  if (j == 1){
    min_cv <- CV(fit)["CV"]
    min_k <- j
  }else{
    if (min_cv > CV(fit)["CV"]){
      min_cv <- CV(fit)["CV"]
      min_k <- j
    }
  }
}
min_cv
min_k

resid(fit_harmonic) %>% 
  autoplot() +
  labs(title = "Residuals",
       y = "Gas",
       x = "Year")

ljung_box(fit_harmonic)

fit_harmonic <- tslm(gas_2004 ~ trend + fourier(gas_2004, K = min.k))
predict_harmonic <- forecast(fit_harmonic,
                             newdata = data.frame(fourier(gas_2004, min_k, 52)))
predict_harmonic

autoplot(gas_2004, series = "Data") +
  autolayer(predict_harmonic, series = "Fitted") +
  xlab("Year") + ylab("Gas") +
  ggtitle("Gas Supply")



#Question 6
global_economy %>% 
  filter(Country == "Afghanistan") %>% 
  autoplot(Population) +
  labs(title = "Annual Population of Afghanistan",
       y = "Population",
       x = "Year")
#THERE IS AN  UPWARD TREND IN POPULATION, WITH THE EXCEPTION OF THE CLEARLY VISIBLE
#DIP IN THE 1980S FROM THE SOVIET-AFGHAN WAR

afghan_pop <- global_economy %>% 
  filter(Country == "Afghanistan")
afghan_pop <- as_tsibble(afghan_pop, index = Year)

fit_afghan <- afghan_pop %>% 
  model(
    linear = TSLM(Population ~ trend()),
    exponential = TSLM(log(Population) ~ trend()),
    piecewise = TSLM(Population ~ trend(knots = c(1980, 1989)))
  )
autoplot(resid(fit_afghan))

fit_lm_afghan <- lm(Population ~ Year, data = afghan_pop)
plot(resid(fit_lm_afghan))

predict_afghan <- fit_afghan %>% forecast(h = 10)

afghan_pop %>% 
  autoplot(Population) +
  geom_line(data = fitted(fit_afghan),
            aes(y = .fitted, colour = .model)) +
  autolayer(predict_afghan, alpha = 0.5, level = 95) +
  labs(y = "Population",
      x = "Year",
      title = "Annual Population of Afghanistan")
#THE PIECEWISE SEEMS PRETTY CLEARLY TO BE THE MOST ON TRACK WITH THE ACTUAL DATA
#IT HANDLED THE DIP FROM THE WAR THE BEST


