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

rgdp_order_dm <-  gdp_order_dm[c("p", "d", "q")]
rgdp_seasonal_dm <-  gdp_order_dm[c("P", "D", "Q")]



my_emaeip_r <- roos[, c("emae", "ip")]
my_emaeip_dm <- doos[, c("emae", "ip")]

tic()
# using contemporary xregs (k = 0)
cv0_e_r <- cv_arimax(y_ts = rgdp_ts, xreg_ts = roos,  h_max = 6, n_cv = 8,
                  training_length = 16,  y_order = rgdp_order_r, 
                  y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

cv0_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = doos,  h_max = 6, n_cv = 8,
                     training_length = 16,  y_order = rgdp_order_dm, 
                     y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)

# using contemporary xregs (k = 1)
cv1_e_r <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(roos, k = 1),  h_max = 6, n_cv = 8,
                   training_length = 16,  y_order = rgdp_order_r, 
                   y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

cv1_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(doos, k = 1),  h_max = 6, n_cv = 8,
                     training_length = 16,  y_order = rgdp_order_dm, 
                     y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)

# using contemporary xregs (k = 2)
cv2_e_r <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(roos, k = 2),  h_max = 6, n_cv = 8,
                   training_length = 16,  y_order = rgdp_order_r, 
                   y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

cv2_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(doos, k = 2),  h_max = 6, n_cv = 8,
                     training_length = 16,  y_order = rgdp_order_dm, 
                     y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)
toc()

# make a function called rmse
# example with weights_vec set to default
cv0_rmse_list_r <- map(cv0_e_r, compute_rmse, h_max = 6, n_cv = 8)
cv1_rmse_list_r <- map(cv1_e_r, compute_rmse, h_max = 6, n_cv = 8)
cv2_rmse_list_r <- map(cv2_e_r, compute_rmse, h_max = 6, n_cv = 8)

# notice the difference between these two
cv0_rmse_wm_list_r <- map(cv0_rmse_list_r, "weighted_rmse_all_horizons")
cv0_rmse_wm_vector_r <- map_dbl(cv0_rmse_list_r, "weighted_rmse_all_horizons")
cv1_rmse_wm_vector_r <- map_dbl(cv1_rmse_list_r, "weighted_rmse_all_horizons")
cv2_rmse_wm_vector_r <- map_dbl(cv2_rmse_list_r, "weighted_rmse_all_horizons")

all_arimax_r <- my_arimax(y_ts = rgdp_ts, xreg_ts = roos,  y_order = rgdp_order_r, 
                       y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

all_arimax_dm <- my_arimax(y_ts = rgdp_ts, xreg_ts = doos,  y_order = rgdp_order_dm, 
                        y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)

all_fc_arimax_r <- map2(all_arimax_r, my_forecast, h = 6, xreg =  roos[ , "emae"])

forecast(arimax_r_emae, h = 6, xreg =  roos[57:62, "emae"])

arimax_r_emae <- all_arimax_r[[1]]

# # example with weights_vec set to 0.2, 0.2, 0.2, 0.2, 0.1, 0.1
# moo <- map(cv0_e_r, compute_rmse, h_max = 6, weights_vec = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1))
# moo