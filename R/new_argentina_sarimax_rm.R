source('./R/utils.R')

library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

tic()
# preliminaries -----------------------------------------------------------

final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4

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
cv0_e_r <- cv_arimax(y_ts = rgdp_ts, xreg_ts = roos,  h_max = h_max, n_cv = 8,
                  training_length = 16,  y_order = rgdp_order_r, 
                  y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names,
                  method = "ML")

cv0_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = doos,  h_max =  h_max, n_cv = 8,
                     training_length = 16,  y_order = rgdp_order_dm, 
                     y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names,
                     method = "ML")

# using contemporary xregs (k = 1)
cv1_e_r <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(roos, k = 1),  h_max = h_max,
                     n_cv = 8, training_length = 16,  y_order = rgdp_order_r, 
                     y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names,
                     method = "ML")

cv1_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(doos, k = 1),  h_max = h_max,
                      n_cv = 8, training_length = 16,  y_order = rgdp_order_dm, 
                      y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names,
                      method = "ML")

# using contemporary xregs (k = 2)
cv2_e_r <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(roos, k = 2),  h_max = h_max,
                     n_cv = 8, training_length = 16,  y_order = rgdp_order_r, 
                     y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names,
                     method = "ML")

cv2_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(doos, k = 2),  h_max = h_max,
                      n_cv = 8, training_length = 16,  y_order = rgdp_order_dm, 
                      y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names,
                      method = "ML")

cv_rgdp_e_r <- cv_arima(y_ts = rgdp_ts, h_max = h_max, n_cv = 8,
                     training_length = 16,  y_order = rgdp_order_r, 
                     y_seasonal = rgdp_seasonal_r,
                     method = "ML")

cv_rgdp_e_dm <- cv_arima(y_ts = rgdp_ts, h_max = h_max, n_cv = 8,
                        training_length = 16,  y_order = rgdp_order_dm, 
                        y_seasonal = rgdp_seasonal_dm,
                        method = "ML")

toc()

# make a function called rmse
# example with weights_vec set to default
cv0_rmse_list_r <- map(cv0_e_r, compute_rmse, h_max = h_max, n_cv = 8)
cv1_rmse_list_r <- map(cv1_e_r, compute_rmse, h_max = h_max, n_cv = 8)
cv2_rmse_list_r <- map(cv2_e_r, compute_rmse, h_max = h_max, n_cv = 8)

cv_rdgp_rmse_r <- compute_rmse(cv_rgdp_e_r, h_max = h_max, n_cv = 8)
cv_rdgp_rmse_dm <- compute_rmse(cv_rgdp_e_dm, h_max = h_max, n_cv = 8)

# notice the difference between these two
cv0_rmse_wm_list_r <- map(cv0_rmse_list_r, "weighted_same_h")
cv0_rmse_wm_r <- map_dbl(cv0_rmse_list_r, "weighted_same_h")
cv1_rmse_wm_r <- map_dbl(cv1_rmse_list_r, "weighted_same_h")
cv2_rmse_wm_r <- map_dbl(cv2_rmse_list_r, "weighted_same_h")

cv_rmse_rgdp_r <- cv_rdgp_rmse_r[["weighted_same_h"]]
cv_rmse_rgdp_dm <- cv_rdgp_rmse_dm[["weighted_same_h"]]

all_arimax_r <- my_arimax(y_ts = rgdp_ts, xreg_ts = roos,  y_order = rgdp_order_r, 
                       y_seasonal = rgdp_seasonal_r, vec_of_names = monthly_names)

all_arimax_dm <- my_arimax(y_ts = rgdp_ts, xreg_ts = doos,  y_order = rgdp_order_dm, 
                        y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)

all_fcs_r <- forecast_xreg(all_arimax_r, roos, h = h_max, vec_of_names = monthly_names)
all_fcs_dm <- forecast_xreg(all_arimax_dm, doos, h = h_max, vec_of_names = monthly_names)

toc()
# # example with weights_vec set to 0.2, 0.2, 0.2, 0.2, 0.1, 0.1
# moo <- map(cv0_e_r, compute_rmse, h_max = 6, weights_vec = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1))
# moo

ave_rmse_012_r <- cbind(cv0_rmse_wm_r, cv1_rmse_wm_r, cv2_rmse_wm_r)

ave_rmse_r_df <- as.data.frame(ave_rmse_012_r) 
ave_rmse_r_df[, "arima_gdp"] <- cv_rmse_rgdp_r
ave_rmse_r_df <- rownames_to_column(ave_rmse_r_df, var = "id")
ave_rmse_r_df


ave_rmse_r_tbl <- as_tibble(ave_rmse_012_r) 
ave_rmse_r_tbl[, "arima_gdp"] <- cv_rmse_rgdp_r
ave_rmse_r_tbl[, "id"] <- monthly_names 
ave_rmse_r_tbl
