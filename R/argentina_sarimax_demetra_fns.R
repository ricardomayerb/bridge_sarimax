source('./R/utils.R')

library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16
data_path <- "./data/excel_data/Argentina.xlsx"

tic()
myres <- bsarimax_as_function(data_path = data_path, number_of_cv = number_of_cv,
                              train_span = train_span, h_max = h_max,
                              final_forecast_horizon = final_forecast_horizon,
                              outer_cv_round = 0, s4xreg = TRUE)
toc()

level_fc_using_accu_level_weights <- myres$expo_final_rgdp_and_w_fc
level_fc_using_accu_yoy_weights <- myres$expo_final_rgdp_and_yoyw_fc
yoy_fc_using_accu_level_weights <- myres$yoy_growth_expo_final_rgdp_and_w_fc
yoy_fc_using_accu_yoy_weights <- myres$yoy_growth_expo_final_rgdp_and_yoyw_fc

cv_rmse_yoy_rgdp_conditional_on_x <- myres$cv_all_x_rmse_each_h_yoy
cv_rmse_yoy_rgdp <- myres$cv_rmse_each_h_rgdp_yoy

cv_rmse_level_rgdp_conditional_on_x <- myres$cv_all_x_rmse_each_h
cv_rmse_level_rgdp <- myres$cv_rmse_each_h_rgdp



m_arg <- read_excel("data/Argentina_m_analysis_rgdp.xlsx")
m_all_rmse <- m_arg[, c("rmse1", "rmse2", "rmse3", "rmse4", "rmse5", "rmse6", "rmse7", "rmse8")]
m_all_rmse$cond_exo <- m_arg[ , "cond_exo"]
