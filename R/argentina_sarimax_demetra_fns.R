source('./R/utils.R')

library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

final_forecast_horizon <- c(2019, 12)
h_max = 3 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16
data_path <- "./data/excel_data/Argentina.xlsx"



tic()
myres <- bsarimax_as_function(data_path = data_path, number_of_cv = number_of_cv,
                              train_span = train_span, h_max = h_max,
                              final_forecast_horizon = final_forecast_horizon,
                              outer_cv_round = 0)
toc()



outer_cv_bsarimax <- function(number_of_outer_cv, data_path, train_span = 16,
                              h_max = 6, number_of_cv = 8, 
                              final_forecast_horizon = c(2019, 12)) {
  
  for (i in seq(1, number_of_outer_cv)  ) {
    myres <- bsarimax_as_function(data_path = data_path, 
                                  number_of_cv = number_of_cv,
                                  train_span = train_span, 
                                  final_forecast_horizon = final_forecast_horizon, 
                                  outer_cv_round = i)
  }
  
} 

number_of_outer_cv <-  2
# foo <- outer_cv_bsarimax(number_of_outer_cv = number_of_outer_cv, 
#                          data_path = data_path , train_span = 16,
#                               h_max = h_max, number_of_cv = number_of_cv, 
#                               final_forecast_horizon = c(2019, 12))

