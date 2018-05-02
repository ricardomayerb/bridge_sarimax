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


gdp_order <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]

monthly_order <- get_order_from_arima(fit_arima_monthly_list_dem, 
                                         suffix = "dm",
                                         this_arima_names = monthly_names)

mdata_ext <- extend_and_qtr(data_mts = monthly_ts, 
                                 final_horizon_date = final_forecast_horizon , 
                                 vec_of_names = monthly_names, 
                                 fitted_arima_list = fit_arima_monthly_list_dem,
                                 start_date_gdp = gdp_and_dates[["gdp_start"]])

doox <- mdata_ext[["series_xts"]]
extended_ts <- mdata_ext[["series_ts"]]

rgdp_order <-  gdp_order[c("p", "d", "q")]
rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]


tic()
# using contemporary xregs (k = 0)
cv0_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = extended_ts,  h_max =  h_max, n_cv = 8,
                     training_length = 16,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                     method = "ML")

# using contemporary xregs (k = 1)

cv1_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(extended_ts, k = 1),  h_max = h_max,
                      n_cv = 8, training_length = 16,  y_order = rgdp_order, 
                      y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                      method = "ML")

# using contemporary xregs (k = 2)
cv2_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(extended_ts, k = 2),  h_max = h_max,
                      n_cv = 8, training_length = 16,  y_order = rgdp_order, 
                      y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                      method = "ML")

cv_rgdp_e <- cv_arima(y_ts = rgdp_ts, h_max = h_max, n_cv = 8,
                        training_length = 16,  y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal,
                        method = "ML")

toc()

# make a function called rmse
# example with weights_vec set to default
cv0_rmse_list <- map(cv0_e, compute_rmse, h_max = h_max, n_cv = 8)
cv1_rmse_list <- map(cv1_e, compute_rmse, h_max = h_max, n_cv = 8)
cv2_rmse_list <- map(cv2_e, compute_rmse, h_max = h_max, n_cv = 8)

cv_rdgp_rmse <- compute_rmse(cv_rgdp_e, h_max = h_max, n_cv = 8)

# notice the difference between these two
cv0_rmse_wm <- map_dbl(cv0_rmse_list, "weighted_same_h")
cv1_rmse_wm <- map_dbl(cv1_rmse_list, "weighted_same_h")
cv2_rmse_wm <- map_dbl(cv2_rmse_list, "weighted_same_h")

cv_rmse_rgdp <- cv_rdgp_rmse[["weighted_same_h"]]

all_arimax_0 <- my_arimax(y_ts = rgdp_ts, xreg_ts = extended_ts,  y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal, vec_of_names = monthly_names)
all_arimax_1 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(extended_ts, k = 1),  y_order = rgdp_order, 
                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names)
all_arimax_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(extended_ts, k = 2),  y_order = rgdp_order, 
                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names)

all_fcs_0 <- forecast_xreg(all_arimax_0, extended_ts, h = h_max, vec_of_names = monthly_names)
all_fcs_1 <- forecast_xreg(all_arimax_1, extended_ts, h = h_max, vec_of_names = monthly_names)
all_fcs_2 <- forecast_xreg(all_arimax_2, extended_ts, h = h_max, vec_of_names = monthly_names)

toc()

all_fcs <- tibble(fc_0 = all_fcs_0, fc_1 = all_fcs_1, fc_2 = all_fcs_2, 
                    id_fc = monthly_names) %>%
  gather(key = "type_fc", value = "fc", -id_fc)
  
ave_rmse_012 <- cbind(cv0_rmse_wm, cv1_rmse_wm, cv2_rmse_wm)


ave_rmse_r_tbl <- as_tibble(ave_rmse_012) %>% 
  mutate(id = monthly_names) %>% 
  gather(key = "type", value = "rmse", -id) %>% 
  cbind(all_fcs) %>% 
  select(-c(id, type)) %>% 
  arrange(id_fc) %>% 
  group_by(id_fc) %>% 
  mutate(min = min(rmse)) %>% 
  filter(rmse == min) %>% 
  mutate(rmse_rgdp = cv_rmse_rgdp) %>% 
  filter(min <= rmse_rgdp) %>% 
  mutate(fc_mean = map(fc, "mean")) %>% 
  mutate(inv_mse = 1/(rmse^2)) %>% 
  ungroup() %>% 
  mutate(sum_inv_mse = sum(inv_mse),
         fc_weight = inv_mse/sum_inv_mse)

fcs_and_weights <- ave_rmse_r_tbl %>% 
  select(c(id_fc, type_fc, fc_weight, fc_mean)) %>% 
  mutate(weighted_fc_means = map2(fc_weight, fc_mean, ~ .x * .y))

w_fcs <- fcs_and_weights %>% select(weighted_fc_means) 

final_fc_mean <- colSums(reduce((reduce(w_fcs, ts.union)), rbind))

