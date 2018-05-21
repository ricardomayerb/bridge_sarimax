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

country_name <- "Paraguay"
data_path <- paste0("./data/excel_data/", country_name,".xlsx")
m_analysis_path <- paste0("data/", country_name,"_m_analysis_rgdp.xlsx")
rds_file_name = paste0("sarimax_objects_", country_name,".rds")
  
tic()
myres <- bsarimax_as_function(data_path = data_path, number_of_cv = number_of_cv,
                              train_span = train_span, h_max = h_max,
                              final_forecast_horizon = final_forecast_horizon,
                              outer_cv_round = 0, s4xreg = FALSE)
toc()


var_lag_order_season <- myres$var_lag_order_season

level_fc_using_accu_level_weights <- myres$expo_final_rgdp_and_w_fc

level_fc_using_accu_yoy_weights <- myres$expo_final_rgdp_and_yoyw_fc

yoy_fc_using_accu_level_weights <- myres$yoy_growth_expo_final_rgdp_and_w_fc
yoy_fc_using_accu_yoy_weights <- myres$yoy_growth_expo_final_rgdp_and_yoyw_fc
cv_rmse_yoy_rgdp_conditional_on_x <- myres$cv_all_x_rmse_each_h_yoy
cv_rmse_yoy_rgdp <- myres$cv_rmse_each_h_rgdp_yoy
cv_rmse_level_rgdp_conditional_on_x <- myres$cv_all_x_rmse_each_h
cv_rmse_level_rgdp <- myres$cv_rmse_each_h_rgdp

names(cv_rmse_level_rgdp_conditional_on_x)[1:8] <- paste0("level_rmse_", 1:8)
names(cv_rmse_yoy_rgdp_conditional_on_x)[1:8] <- paste0("yoy_rmse_", 1:8)
names(cv_rmse_yoy_rgdp)[1:8] <- paste0("yoy_rmse_", 1:8)
names(cv_rmse_level_rgdp)[1:8] <- paste0("level_rmse_", 1:8)


m_arg <- read_excel(m_analysis_path)

m_all_rmse <- m_arg[, c("cond_exo", "rmse1", "rmse2", "rmse3", "rmse4", "rmse5", "rmse6", "rmse7", "rmse8")]
m_to_compare_rmse <- m_all_rmse %>% 
  mutate_if(is.double, function(x) 0.01 * x) %>% 
  mutate(pre_variable = str_remove(cond_exo, "S4.l"),
         variable = str_extract(pre_variable, "([^\\s]+)"),
         lag = ifelse(str_detect(pre_variable, "LS"), 
                      ifelse(str_detect(pre_variable, "L2S"), 2, 1) , 0)
                          ) %>% 
  select(-pre_variable) %>% 
  mutate(variable = ifelse(variable == "_NONE", "rgdp", variable))

compare_rmse <- rbind(cv_rmse_level_rgdp, cv_rmse_level_rgdp_conditional_on_x) %>%
  left_join(m_to_compare_rmse, by = c("variable", "lag")) %>% 
  dplyr::select( -c(cond_exo))

compare_rmse_yoy <- rbind(cv_rmse_yoy_rgdp, 
                          cv_rmse_yoy_rgdp_conditional_on_x)

country_objects <- list(
  var_lag_order_season = var_lag_order_season,
  compare_rmse = compare_rmse, 
  compare_rmse_yoy = compare_rmse_yoy, 
  yoy_fc_using_accu_level_weights = yoy_fc_using_accu_level_weights,
  yoy_fc_using_accu_yoy_weights = yoy_fc_using_accu_yoy_weights,
  level_fc_using_accu_level_weights = level_fc_using_accu_level_weights,
  level_fc_using_accu_yoy_weights = level_fc_using_accu_yoy_weights)


saveRDS(country_objects, file = rds_file_name)
