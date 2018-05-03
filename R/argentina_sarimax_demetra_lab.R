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
h_max = 6 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16

data_path <- "./data/excel_data/Argentina.xlsx"

gdp_and_dates <- get_rgdp_and_dates(data_path)

monthly_data <- get_monthly_variables(data_path = data_path)

monthly_ts <- make_monthly_ts(monthly_data)
monthly_ts  <- log(monthly_ts)
full_monthly_ts  <- monthly_ts

monthly_names <- colnames(monthly_ts)

rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
              start = gdp_and_dates[["gdp_start"]], frequency = 4)
rgdp_ts <- log(rgdp_ts)


foo <- monthly_ts[, 1]

cutback_ts <- function(single_ts, nrows_to_cut) {
  
  if (nrows_to_cut > 0) {
    
    if (  is.na(last(single_ts))  ) {
      ddc_ts <- diff(diff(cumsum(is.na(single_ts))))
      ind_last_obs <- which(ddc_ts == 1) + 1
      new_ts <- single_ts
      new_ts[(ind_last_obs - nrows_to_cut + 1):ind_last_obs] <- NA
    } else {
      new_ts <- single_ts
      new_ts[(length(single_ts) - nrows_to_cut + 1):length(single_ts)] <- NA
    }
    return(new_ts)
    } else{
    return(single_ts)
  }
  
}

moo <- cutback_ts(single_ts = foo, nrows_to_cut = 2)
ts.union(foo, moo)

rgdp_ts_cv <- cutback_ts(single_ts = rgdp_ts, nrows_to_cut = 3)
ts.union(rgdp_ts, rgdp_ts_cv)

rgdp_ts_onena <- cutback_ts(single_ts = rgdp_ts, nrows_to_cut = 1)
ts.union(rgdp_ts, rgdp_ts_onena)

rgdp_ts_twona <- cutback_ts(single_ts = rgdp_ts_onena, nrows_to_cut = 1)
ts.union(rgdp_ts_onena, rgdp_ts_twona)

rgdp_ts_fourna <- cutback_ts(single_ts = rgdp_ts_twona, nrows_to_cut = 4)
ts.union(rgdp_ts_twona, rgdp_ts_fourna)

# funciona!!! pero debe volver a ser ts
monthly_data_cv <- apply(monthly_ts, MARGIN = 2, FUN = cutback_ts, nrows_to_cut = 2)
start_time_vec_monthly <- min(time(monthly_ts))
start_year_vec_monthly <- floor(start_time_vec_monthly)
start_quarter_vec_monthly <- 4*(start_time_vec_monthly - start_year_vec_monthly + 0.25)

monthly_ts_cv <- ts(monthly_data_cv, 
                    start = c(start_year_vec_monthly, 
                              start_quarter_vec_monthly),
                    frequency = 12)

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


gdp_order_dm <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]

monthly_order_dm <- get_order_from_arima(fit_arima_monthly_list_dem, 
                                         suffix = "dm",
                                         this_arima_names = monthly_names)

mdata_ext_dm <- extend_and_qtr(data_mts = monthly_ts, 
                                 final_horizon_date = final_forecast_horizon , 
                                 vec_of_names = monthly_names, 
                                 fitted_arima_list = fit_arima_monthly_list_dem,
                                 start_date_gdp = gdp_and_dates[["gdp_start"]])

doox <- mdata_ext_dm[["series_xts"]]
mdata_ext_dm_ts <- mdata_ext_dm[["series_ts"]]

my_emae <- mdata_ext_dm_ts[, "emae"]

rgdp_order_dm <-  gdp_order_dm[c("p", "d", "q")]
rgdp_seasonal_dm <-  gdp_order_dm[c("P", "D", "Q")]


my_emaeip_dm <- mdata_ext_dm_ts[, c("emae", "ip")]


tic()
# using contemporary xregs (k = 0)
cv0_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_dm_ts,  h_max =  h_max, n_cv = number_of_cv,
                     training_length = train_span,  y_order = rgdp_order_dm, 
                     y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names,
                     method = "ML")

# using one-lag xregs (k = 1)
cv1_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_dm_ts, k = 1),  h_max = h_max,
                      n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order_dm, 
                      y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names,
                      method = "ML")

# using two-lags xregs (k = 2)
cv2_e_dm <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_dm_ts, k = 2),  h_max = h_max,
                      n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order_dm, 
                      y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names,
                      method = "ML")

cv_rgdp_e_dm <- cv_arima(y_ts = rgdp_ts, h_max = h_max, n_cv = number_of_cv,
                        training_length = train_span,  y_order = rgdp_order_dm, 
                        y_seasonal = rgdp_seasonal_dm,
                        method = "ML")

cv0_e_dm_yoy <- cv0_e_dm[["cv_yoy_errors_all_pairs_yx"]]
cv1_e_dm_yoy <- cv1_e_dm[["cv_yoy_errors_all_pairs_yx"]]
cv2_e_dm_yoy <- cv2_e_dm[["cv_yoy_errors_all_pairs_yx"]]

cv0_e_dm <- cv0_e_dm[["cv_errors_all_pairs_yx"]]
cv1_e_dm <- cv1_e_dm[["cv_errors_all_pairs_yx"]]
cv2_e_dm <- cv2_e_dm[["cv_errors_all_pairs_yx"]]

cv_rgdp_e_dm_yoy <- cv_rgdp_e_dm[["cv_yoy_errors"]]
cv_rgdp_e_dm <- cv_rgdp_e_dm[["cv_errors"]]

toc()

# example with weights_vec set to default
cv0_rmse_list_dm <- map(cv0_e_dm, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv1_rmse_list_dm <- map(cv1_e_dm, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv2_rmse_list_dm <- map(cv2_e_dm, compute_rmse, h_max = h_max, n_cv = number_of_cv)

cv0_rmse_list_dm_yoy <- map(cv0_e_dm_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv1_rmse_list_dm_yoy <- map(cv1_e_dm_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv2_rmse_list_dm_yoy <- map(cv2_e_dm_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)

cv_rdgp_rmse_dm <- compute_rmse(cv_rgdp_e_dm, h_max = h_max, n_cv = number_of_cv)
cv_rdgp_rmse_dm_yoy <- compute_rmse(cv_rgdp_e_dm_yoy, h_max = h_max, n_cv = number_of_cv)

cv0_rmse_wm_list_dm <- map(cv0_rmse_list_dm, "weighted_same_h")
cv0_rmse_wm_dm <- map_dbl(cv0_rmse_list_dm, "weighted_same_h")
cv1_rmse_wm_dm <- map_dbl(cv1_rmse_list_dm, "weighted_same_h")
cv2_rmse_wm_dm <- map_dbl(cv2_rmse_list_dm, "weighted_same_h")

cv0_rmse_wm_dm_yoy <- map_dbl(cv0_rmse_list_dm_yoy, "weighted_same_h")
cv1_rmse_wm_dm_yoy <- map_dbl(cv1_rmse_list_dm_yoy, "weighted_same_h")
cv2_rmse_wm_dm_yoy <- map_dbl(cv2_rmse_list_dm_yoy, "weighted_same_h")

cv_rmse_rgdp_dm <- cv_rdgp_rmse_dm[["weighted_same_h"]]
cv_rmse_rgdp_dm_yoy <- cv_rdgp_rmse_dm_yoy[["weighted_same_h"]]


all_arimax_dm_0 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_dm_ts,  y_order = rgdp_order_dm, 
                        y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)
all_arimax_dm_1 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_dm_ts, k = 1),  y_order = rgdp_order_dm, 
                           y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)
all_arimax_dm_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_dm_ts, k = 2),  y_order = rgdp_order_dm, 
                           y_seasonal = rgdp_seasonal_dm, vec_of_names = monthly_names)

all_fcs_dm_0 <- forecast_xreg(all_arimax_dm_0, mdata_ext_dm_ts, h = h_max, vec_of_names = monthly_names)
all_fcs_dm_1 <- forecast_xreg(all_arimax_dm_1, mdata_ext_dm_ts, h = h_max, vec_of_names = monthly_names)
all_fcs_dm_2 <- forecast_xreg(all_arimax_dm_2, mdata_ext_dm_ts, h = h_max, vec_of_names = monthly_names)
toc()

all_fcs_dm <- tibble(fc_0 = all_fcs_dm_0, fc_1 = all_fcs_dm_1, fc_2 = all_fcs_dm_2, 
                    id_fc = monthly_names) %>%
  gather(key = "type_fc", value = "fc", -id_fc)
  

# # example with weights_vec set to 0.2, 0.2, 0.2, 0.2, 0.1, 0.1
# moo <- map(cv0_e_r, compute_rmse, h_max = 6, weights_vec = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1))
# moo

ave_rmse_012_dm <- cbind(cv0_rmse_wm_dm, cv1_rmse_wm_dm, cv2_rmse_wm_dm)


ave_rmse_r_tbl_dm <- as_tibble(ave_rmse_012_dm) %>% 
  mutate(id = monthly_names) %>% 
  gather(key = "type", value = "rmse", -id) %>% 
  cbind(all_fcs_dm) %>% 
  select(-c(id, type)) %>% 
  arrange(id_fc) %>% 
  group_by(id_fc) %>% 
  mutate(min = min(rmse)) %>% 
  filter(rmse == min) %>% 
  mutate(rmse_rgdp = cv_rmse_rgdp_dm) %>% 
  filter(min <= rmse_rgdp) %>% 
  mutate(fc_mean = map(fc, "mean")) %>% 
  mutate(inv_mse = 1/(rmse^2)) %>% 
  ungroup() %>% 
  mutate(sum_inv_mse = sum(inv_mse),
         fc_weight = inv_mse/sum_inv_mse)

fcs_and_weights_dm <- ave_rmse_r_tbl_dm %>% 
  select(c(id_fc, type_fc, fc_weight, fc_mean)) %>% 
  mutate(weighted_fc_means = map2(fc_weight, fc_mean, ~ .x * .y))

w_fcs_dm <- fcs_and_weights_dm %>% select(weighted_fc_means) 

final_fc_mean_dm <- colSums(reduce((reduce(w_fcs_dm, ts.union)), rbind))

# ave_rmse_dm_tbl <- as_tibble(ave_rmse_012_dm) %>% 
#   mutate(id = monthly_names) %>% 
#   gather(key = "type", value = "rmse", -id) %>% 
#   arrange(id) %>% 
#   group_by(id) %>% 
#   mutate(min = min(rmse)) %>% 
#   filter(rmse == min) %>% 
#   mutate(rmse_rgdp = cv_rmse_rgdp_dm) %>% 
#   filter(min <= rmse_rgdp)
#   
# ave_rmse_dm_tbl
# 
# ave_rmse_012_tbl_dm <- tibble( lag_0 = cv0_rmse_list_dm, lag_1 = cv1_rmse_wm_dm,
#                                lag_2 = cv2_rmse_wm_dm)

