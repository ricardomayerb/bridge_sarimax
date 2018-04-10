source('./R/utils.R')

library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

data_path <- "./data/excel_data/Argentina.xlsx"

gdp_and_dates <- get_rgdp_and_dates(data_path)

monthly_data <- get_monthly_variables(data_path = data_path)

monthly_ts <- make_monthly_ts(monthly_data)

monthly_names <- colnames(monthly_ts)

rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
              start = gdp_and_dates[["gdp_start"]], frequency = 4)

demetra_output <- get_demetra_params(data_path)

fit_arima_rgdp_list_dem <- fit_arimas(
  y_ts = rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")

fit_arima_monthly_list_dem <- fit_arimas(
  y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
  this_arima_names = monthly_names)

tic()
fit_arima_rgdp_list_r <- fit_arimas(
  y_ts = rgdp_ts, auto = TRUE, this_arima_names = "rgdp")
toc()

fit_arima_monthly_list_r <- fit_arimas(
  y_ts = monthly_ts, auto = TRUE)

gdp_order_r <- get_order_from_arima(fit_arima_rgdp_list_r)[[1]]
gdp_order_dm <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]
gdp_order_r_and_dem <- rbind(gdp_order_r, gdp_order_dm)
gdp_order_r_and_dem <- rbind(gdp_order_r_and_dem, gdp_order_r - gdp_order_dm)

monthly_order_r <- get_order_from_arima(fit_arima_monthly_list_r, 
                                        suffix = "r",
                                        this_arima_names = monthly_names)

monthly_order_dm <- get_order_from_arima(fit_arima_monthly_list_dem, 
                                         suffix = "dm",
                                         this_arima_names = monthly_names)


monthly_order_r_df <- t(as.data.frame(monthly_order_r))
monthly_order_dm_df <- t(as.data.frame(monthly_order_dm))

monthly_order_both_df <- cbind(monthly_order_r_df, monthly_order_dm_df)

monthly_order_both_diff_df <- as.data.frame(monthly_order_both_df) %>%
  mutate(p_diff = p_r - p_dm, q_diff = q_r - q_dm, d_diff = d_r - d_dm,
         P_diff = P_r - P_dm, Q_diff = Q_r - Q_dm, D_diff = D_r - D_dm) %>%
  select(c(p_diff, q_diff, d_diff, P_diff, Q_diff, D_diff)) %>% 
  mutate(id = monthly_names)
  
  
