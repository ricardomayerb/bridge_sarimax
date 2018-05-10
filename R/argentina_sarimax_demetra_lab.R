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


# rgdp_ts_cv <- cutback_ts(single_ts = rgdp_ts, nrows_to_cut = 3)
# ts.union(rgdp_ts, rgdp_ts_cv)
# 
# # funciona!!! pero debe volver a ser ts
# monthly_data_cv <- apply(monthly_ts, MARGIN = 2, FUN = cutback_ts, nrows_to_cut = 2)
# 
# monthly_ts_cv <- ts(monthly_data_cv, 
#                     start = stats::start(monthly_ts),
#                     frequency = 12)



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
mdata_ext_ts <- mdata_ext[["series_ts"]]

my_emae <- mdata_ext_ts[, "emae"]

rgdp_order <-  gdp_order[c("p", "d", "q")]
rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]


my_emaeip <- mdata_ext_ts[, c("emae", "ip")]


tic()
# using contemporary xregs (k = 0)
cv0_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  h_max =  h_max, n_cv = number_of_cv,
                     training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                     method = "ML", s4xreg = TRUE)

# using one-lag xregs (k = 1)
cv1_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_ts, k = 1),  h_max = h_max,
                      n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                      y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                      method = "ML", s4xreg = TRUE)

# using two-lags xregs (k = 2)
cv2_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_ts, k = 2),  h_max = h_max,
                      n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                      y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                      method = "ML", s4xreg = TRUE)

cv_rgdp_e <- cv_arima(y_ts = rgdp_ts, h_max = h_max, n_cv = number_of_cv,
                        training_length = train_span,  y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal,
                        method = "ML")

cv0_e_yoy <- cv0_e[["cv_yoy_errors_all_pairs_yx"]]
cv1_e_yoy <- cv1_e[["cv_yoy_errors_all_pairs_yx"]]
cv2_e_yoy <- cv2_e[["cv_yoy_errors_all_pairs_yx"]]

cv0_e <- cv0_e[["cv_errors_all_pairs_yx"]]
cv1_e <- cv1_e[["cv_errors_all_pairs_yx"]]
cv2_e <- cv2_e[["cv_errors_all_pairs_yx"]]

cv_rgdp_e_yoy <- cv_rgdp_e[["cv_yoy_errors"]]
cv_rgdp_e <- cv_rgdp_e[["cv_errors"]]

toc()

# example with weights_vec set to default
cv0_rmse_list <- map(cv0_e, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv1_rmse_list <- map(cv1_e, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv2_rmse_list <- map(cv2_e, compute_rmse, h_max = h_max, n_cv = number_of_cv)

cv0_rmse_list_yoy <- map(cv0_e_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv1_rmse_list_yoy <- map(cv1_e_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
cv2_rmse_list_yoy <- map(cv2_e_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)

cv_rdgp_rmse <- compute_rmse(cv_rgdp_e, h_max = h_max, n_cv = number_of_cv)
cv_rdgp_rmse_yoy <- compute_rmse(cv_rgdp_e_yoy, h_max = h_max, n_cv = number_of_cv)

cv0_rmse_wm_list <- map(cv0_rmse_list, "weighted_same_h")
cv0_rmse_wm <- map_dbl(cv0_rmse_list, "weighted_same_h")
cv1_rmse_wm <- map_dbl(cv1_rmse_list, "weighted_same_h")
cv2_rmse_wm <- map_dbl(cv2_rmse_list, "weighted_same_h")

cv0_rmse_wm_yoy <- map_dbl(cv0_rmse_list_yoy, "weighted_same_h")
cv1_rmse_wm_yoy <- map_dbl(cv1_rmse_list_yoy, "weighted_same_h")
cv2_rmse_wm_yoy <- map_dbl(cv2_rmse_list_yoy, "weighted_same_h")

cv_rmse_rgdp <- cv_rdgp_rmse[["weighted_same_h"]]
cv_rmse_rgdp_yoy <- cv_rdgp_rmse_yoy[["weighted_same_h"]]


all_arimax_0 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal, vec_of_names = monthly_names, s4xreg = TRUE)
all_arimax_1 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_ts, k = 1),  y_order = rgdp_order, 
                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names, s4xreg = TRUE)
all_arimax_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_ts, k = 2),  y_order = rgdp_order, 
                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names, s4xreg = TRUE)

all_fcs_0 <- forecast_xreg(all_arimax_0, mdata_ext_ts, h = h_max, vec_of_names = monthly_names)
all_fcs_1 <- forecast_xreg(all_arimax_1, mdata_ext_ts, h = h_max, vec_of_names = monthly_names)
all_fcs_2 <- forecast_xreg(all_arimax_2, mdata_ext_ts, h = h_max, vec_of_names = monthly_names)
toc()

all_fcs <- tibble(fc_0 = all_fcs_0, fc_1 = all_fcs_1, fc_2 = all_fcs_2, 
                    id_fc = monthly_names) %>%
  gather(key = "type_fc", value = "fc", -id_fc)

all_fcs_yoy <- tibble(fc_0 = all_fcs_0, fc_1 = all_fcs_1, fc_2 = all_fcs_2, 
                     id_fc = monthly_names) %>%
  gather(key = "type_fc", value = "fc", -id_fc)  %>% 
  mutate(fc_rgdp_mean = map(fc, "mean"),
         rgdp_data = map(fc, "x"),
         rgdp_and_fc_level = map2(rgdp_data, fc_rgdp_mean,   ~ ts(c(.x,.y), frequency = 4, 
                                       start = stats::start(.x))
                                  ),
         yoy_rgdp_and_fc_level = map(rgdp_and_fc_level, diff, 4),
         yoy_fc_rgdp_mean = map2(yoy_rgdp_and_fc_level, fc_rgdp_mean,
                                 ~ window(.x, start = stats::start(.y))
                                 )
  )

# # example with weights_vec set to 0.2, 0.2, 0.2, 0.2, 0.1, 0.1
# moo <- map(cv0_e_r, compute_rmse, h_max = 6, weights_vec = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1))
# moo

ave_rmse_012 <- cbind(cv0_rmse_wm, cv1_rmse_wm, cv2_rmse_wm)
ave_rmse_012_yoy <- cbind(cv0_rmse_wm_yoy, cv1_rmse_wm_yoy,
                             cv2_rmse_wm_yoy)

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

ave_rmse_r_tbl_yoy <- as_tibble(ave_rmse_012_yoy) %>% 
  mutate(id = monthly_names) %>% 
  gather(key = "type", value = "rmse", -id) %>% 
  cbind(all_fcs_yoy) %>% 
  select(-c(id, type)) %>% 
  arrange(id_fc) %>% 
  group_by(id_fc) %>% 
  mutate(min = min(rmse)) %>% 
  filter(rmse == min) %>% 
  mutate(rmse_rgdp_yoy = cv_rmse_rgdp_yoy) %>% 
  filter(min <= rmse_rgdp_yoy) %>%
  mutate(inv_mse = 1/(rmse^2)) %>% 
  ungroup() %>% 
  mutate(sum_inv_mse = sum(inv_mse),
         fc_weight = inv_mse/sum_inv_mse)

fcs_and_weights <- ave_rmse_r_tbl %>% 
  select(c(id_fc, type_fc, fc_weight, fc_mean)) %>% 
  mutate(weighted_fc_means = map2(fc_weight, fc_mean, ~ .x * .y))

fcs_and_weights_yoy <- ave_rmse_r_tbl_yoy %>% 
  select(c(id_fc, type_fc, fc_weight, yoy_fc_rgdp_mean)) %>% 
  mutate(weighted_fc_means = map2(fc_weight, yoy_fc_rgdp_mean, ~ .x * .y))

w_fcs <- fcs_and_weights %>% select(weighted_fc_means) 
w_fcs_yoy <- fcs_and_weights_yoy %>% select(weighted_fc_means) 

final_fc_mean <- colSums(reduce((reduce(w_fcs, ts.union)), rbind))
final_fc_mean_yoy <- colSums(reduce((reduce(w_fcs_yoy, ts.union)), rbind))

final_rgdp_and_fc <- ts(c(rgdp_ts, final_fc_mean), frequency = 4,
                              start = stats::start(rgdp_ts))

direct_final_rgdp_and_fc_yoy <- diff(final_rgdp_and_fc, lag = 4)
direct_final_fc_yoy <- subset(direct_final_rgdp_and_fc_yoy,
                              start = length(direct_final_rgdp_and_fc_yoy) - h_max + 1)

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

