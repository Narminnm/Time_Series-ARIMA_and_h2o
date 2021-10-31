library(tidyverse)
library(lubridate)
library(timetk)
library(h2o)
install.packages("caTools")
library(caTools)
library(highcharter)
library(tidymodels)
library(modeltime)



temp <- read.csv("daily-minimum-temperatures-in-me (1).csv")
temp %>% view()
colnames(temp)[2] <- "Temperature"

temp$Date <- as.Date(temp$Date, format="%m/%d/%Y")
temp$Temperature %>% class()
temp$Temperature <- as.numeric(temp$Temperature)

temp %>% plot_time_series(Date, Temperature)

temp_timetk <- temp %>% tk_augment_timeseries_signature()
temp_timetk <- temp_timetk %>% 
  select(-contains("hour"),-minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character, as_factor)

#______________________________________________________H2O_________________________________________________

h2o.init()

split <- sample.split(temp_timetk, SplitRatio = 0.8)
train_h2o <- subset(temp_timetk, split==TRUE) %>% as.h2o()
test_h2o <- subset(temp_timetk, split==FALSE) %>% as.h2o()

y <- "Temperature"
x <- temp_timetk %>% select(-Temperature) %>% names()

model_h2o <- h2o.automl(x=x, y=y,
                        training_frame = train_h2o,
                        validation_frame = test_h2o,
                        leaderboard_frame = test_h2o,
                        stopping_metric = "RMSE",
                        seed=123,nfolds=10,
                        exclude_algos = c("GBM","GLM","XGBoost","DRF"),
                        max_runtime_secs = 15)



model_h2o@leaderboard %>% as.data.frame()
h2o_leader <-  model_h2o@leader

pred_h2o <- h2o_leader %>% h2o.predict(test_h2o)
pred_h2o

comparison <- subset(temp_timetk, split==FALSE) %>% 
  add_column(pred=pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(actual=Temperature) %>% select(Date, actual, pred)

highchart() %>% 
  hc_xAxis(categories=comparison$Date) %>% 
  hc_add_series(data=comparison$actual, type="line", color="red", name="Actual") %>% 
  hc_add_series(data=comparison$pred, type="line", color="green", name="Prediction") %>% 
  hc_title(text="Predict")

#_________________________________________ARIMA______________________________________________________

split <- sample.split(temp, SplitRatio = 0.8)
train <- subset(temp, split==TRUE)
test <- subset(temp, split==FALSE)

arima <- arima_reg() %>% set_engine("auto_arima") %>% fit(Temperature ~ Date, train)

modeltime_table(arima) %>% 
  modeltime_calibrate(test) %>% 
  modeltime_forecast(actual_data = temp) %>% 
  plot_modeltime_forecast(.interactive = T)

modeltime_table(arima) %>% modeltime_calibrate(test) %>% modeltime_accuracy()

#________________________________________FORECAST____________________________________________________

next_year <- seq(as.Date("1991-01-01"), as.Date("1991-12-31"), "days") %>% 
  as_tibble() %>% 
  add_column(Temperature=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>% 
  select(-contains("hour"),
         -minute, second, -am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character, as.factor)

next_year_h2o <- next_year %>% as.h2o()

next_year_predictions <- h2o_leader %>% 
  h2o.predict(next_year_h2o) %>% 
  as_tibble() %>% 
  add_column(Date=next_year$Date) %>% 
  select(Date,predict) %>% 
  rename(Temperature=predict)

temp %>% 
  bind_rows(next_year_predictions) %>% 
  mutate(categories=c(rep("Actual", nrow(temp)),rep("Predicted", nrow(next_year_predictions)))) %>% 
  hchart("line", hcaes(Date, Temperature, group=categories)) %>% 
  hc_title(text="Forecast") %>% 
  hc_colors(colors=c("red","green"))











