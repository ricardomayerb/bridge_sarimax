source('./R/utils.R')

library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)


# preliminaries -----------------------------------------------------------

final_forecast_horizon <- c(2019, 12)

data_path <- "./data/excel_data/Argentina.xlsx"

gdp_and_dates <- get_rgdp_and_dates(data_path)

monthly_data <- get_monthly_variables(data_path = data_path)

monthly_ts <- make_monthly_ts(monthly_data)
monthly_ts  <- log(monthly_ts)

monthly_names <- colnames(monthly_ts)

rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
              start = gdp_and_dates[["gdp_start"]], frequency = 4)
rgdp_ts <- log(rgdp_ts)

demetra_output <- get_demetra_params(data_path)

tic()
fit_arima_rgdp_list_dem <- fit_arimas(
  y_ts = rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")
toc()

tic()
fit_arima_monthly_list_dem <- fit_arimas(
  y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
  this_arima_names = monthly_names)
toc()

tic()
fit_arima_rgdp_list_r <- fit_arimas(
  y_ts = rgdp_ts, auto = TRUE, this_arima_names = "rgdp")
toc()

tic()
fit_arima_monthly_list_r <- fit_arimas(
  y_ts = monthly_ts, auto = TRUE)
toc()


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


comparison_of_orders <- compare_two_orders(monthly_order_r, monthly_order_dm, 
                                           monthly_names)  

mdata_ext_dm <- extend_and_qtr(data_mts = monthly_ts, 
                                 final_horizon_date = final_forecast_horizon , 
                                 vec_of_names = monthly_names, 
                                 fitted_arima_list = fit_arima_monthly_list_dem,
                                 start_date_gdp = gdp_and_dates[["gdp_start"]])

mdata_ext_r <- extend_and_qtr(data_mts = monthly_ts, 
                                    final_horizon_date = final_forecast_horizon , 
                                    vec_of_names = monthly_names, 
                                    fitted_arima_list = fit_arima_monthly_list_r,
                                    start_date_gdp = gdp_and_dates[["gdp_start"]])

roox <- mdata_ext_r[["series_xts"]]
roos <- mdata_ext_r[["series_ts"]]

doox <- mdata_ext_dm[["series_xts"]]
doos <- mdata_ext_dm[["series_ts"]]

my_emae <- roos[, "emae"]

rgdp_order_r <-  gdp_order_r[c("p", "d", "q")]
rgdp_seasonal_r <-  gdp_order_r[c("P", "D", "Q")]

marimax_r <- my_arimax(y_ts = rgdp_ts, xreg_ts = roos,  y_order = rgdp_order_r, 
                 y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

marimax_dm <- my_arimax(y_ts = rgdp_ts, xreg_ts = doos,  y_order = rgdp_order_r, 
                       y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

my_emaeip_r <- roos[, c("emae", "ip")]
my_emaeip_dm <- doos[, c("emae", "ip")]

mycv <- cv_arimax(y_ts = rgdp_ts, xreg_ts = roos,  h_max = 3, n_cv = 2,
                  training_length = 12,  y_order = rgdp_order_r, 
                  y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

