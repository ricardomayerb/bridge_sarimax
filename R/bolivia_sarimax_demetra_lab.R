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
number_of_cv = 8
train_span = 16

data_path <- "./data/excel_data/Bolivia.xlsx"

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

rgdp_uncond_fc <- forecast(fit_arima_rgdp_list_dem[["rgdp"]], h = h_max)
rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean


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

# doox <- mdata_ext[["series_xts"]]
mdata_ext_ts <- mdata_ext[["series_ts"]]
yoy_mdata_ext_ts <- diff(mdata_ext_ts, lag = 4)


rgdp_order <-  gdp_order[c("p", "d", "q")]
rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]


# emae_ts <- ts(mdata_ext_ts[, "emae"], , start = stats::start(rgdp_ts),
#               end = stats::end(rgdp_ts), frequency = 4)

# one_arimax_fp <- auto.arima(y = rgdp_ts, xreg = emae_ts)
# two_arimax_fp <- auto.arima(y = rgdp_ts)
# 
# mod_arimax_fp <- Arima(y = rgdp_ts, xreg = emae_ts, order = rgdp_order,
#                        seasonal = rgdp_seasonal)
# 
# foo <- cbind(emae_ts, xts::lag.xts(emae_ts, k = 1), lag.xts(emae_ts, k = 2))
# colnames(foo) <- paste0("xlag_", 0:2)
# foo
# 
# xlagmat <- c()
# 
# for (i in 0:2) {
#   xlagmat <- cbind(xlagmat, lag.xts(emae_ts, k = i))
# }
# 
# colnames(xlagmat) <- paste0("xlag_", 0:2)

# emae_ts_yoy <- diff(emae_ts, lag = 4)
# length(emae_ts)
# train_emae <- subset(emae_ts, end = 49)
# length(train_emae)
# test_emae <- subset(emae_ts, start = 50)
# length(test_emae)
# emae_train_arima <- auto.arima(train_emae)
# fc_train_emae <- forecast(emae_train_arima, h = 8)
# fc_train_emae
# emae_train_and_fc <- ts(c(train_emae, fc_train_emae$mean), start = stats::start(train_emae), frequency = 4)
# emae_train_and_fc_yoy <- diff(emae_train_and_fc, lag = 4)
# emae_fc_yoy <- subset(emae_train_and_fc_yoy, start = 50)
# emae_error_yoy <- emae_test_yoy - emae_fc_yoy
# emae_level_error <- test_emae - fc_train_emae$mean




# my_emaeip <- mdata_ext_ts[, c("emae", "ip")]


tic()
# using contemporary xregs (k = 0)
cv0_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  h_max =  h_max, n_cv = number_of_cv,
                     training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                     method = "ML", s4xreg = FALSE)

# using one-lag xregs (k = 1)
# xreg01 <- cbind()

# cv1_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_ts, k = 1),  h_max = h_max,
#                       n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
#                       y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
#                       method = "ML", s4xreg = TRUE)

cv1_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  h_max = h_max,
                   n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                   y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                   method = "ML", s4xreg = FALSE, xreg_lags = 0:1)

# using two-lags xregs (k = 2)
cv2_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  h_max = h_max,
                      n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                      y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                      method = "ML", s4xreg = FALSE, xreg_lags = 0:2)

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

cv0_rmse_each_h <- map(cv0_rmse_list, "same_h_rmse") %>% reduce(., rbind) %>% 
  mutate(variable = monthly_names, lag = 0)
cv1_rmse_each_h <- map(cv1_rmse_list, "same_h_rmse") %>% reduce(., rbind) %>% 
  mutate(variable = monthly_names, lag = 1)
cv2_rmse_each_h <- map(cv2_rmse_list, "same_h_rmse") %>% reduce(., rbind) %>% 
  mutate(variable = monthly_names, lag = 2)
cv_rmse_each_h_rgdp <- cv_rdgp_rmse[["same_h_rmse"]] %>% 
  mutate(variable = "rgdp", lag = 0)

cv_all_x_rmse_each_h <- rbind(cv0_rmse_each_h,
                            cv1_rmse_each_h, cv2_rmse_each_h)

cv0_rmse_each_h_yoy <- map(cv0_rmse_list_yoy, "same_h_rmse") %>% reduce(., rbind) %>% 
  mutate(variable = monthly_names, lag = 0)
cv1_rmse_each_h_yoy <- map(cv1_rmse_list_yoy, "same_h_rmse") %>% reduce(., rbind) %>% 
  mutate(variable = monthly_names, lag = 1)
cv2_rmse_each_h_yoy <- map(cv2_rmse_list_yoy, "same_h_rmse") %>% reduce(., rbind) %>% 
  mutate(variable = monthly_names, lag = 2)
cv_rmse_each_h_rgdp_yoy <- cv_rdgp_rmse_yoy[["same_h_rmse"]] %>% 
  mutate(variable = "rgdp", lag = 0)


cv_all_x_rmse_each_h_yoy <- rbind(cv0_rmse_each_h_yoy,
                            cv1_rmse_each_h_yoy, cv2_rmse_each_h_yoy)




# foo <- get_fc_weights_one_h(mat_cv_rmses_from_x = cv_all_x_rmse_each_h
#                       , vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp, pos = 6)
# 
# foo_yoy <- get_fc_weights_one_h(mat_cv_rmses_from_x = cv_all_x_rmse_each_h_yoy
#                             , vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy,
#                             pos = 6)


all_arimax_0 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                        s4xreg = FALSE)

all_arimax_1 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                          s4xreg = FALSE, xreg_lags = 0:1)

all_arimax_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                          s4xreg = FALSE, xreg_lags = 0:2)

# all_arimax_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = lag.xts(mdata_ext_ts, k = 2),  y_order = rgdp_order, 
#                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names, s4xreg = TRUE)


all_fcs_0 <- forecast_xreg(all_arimax_0, mdata_ext_ts, h = h_max, 
                           vec_of_names = monthly_names)
all_fcs_1 <- forecast_xreg(all_arimax_1, mdata_ext_ts, h = h_max, 
                           vec_of_names = monthly_names, xreg_lags = 0:1)
all_fcs_2 <- forecast_xreg(all_arimax_2, mdata_ext_ts, h = h_max,
                           vec_of_names = monthly_names, xreg_lags = 0:2)

toc()

all_fcs <- tibble(fc_0 = all_fcs_0, fc_1 = all_fcs_1, fc_2 = all_fcs_2, 
                    id_fc = monthly_names) %>%
  gather(key = "type_fc", value = "fc", -id_fc) %>% 
  mutate(lag = as.integer(str_remove(type_fc, "fc_")),
         raw_rgdp_fc = map(fc, "mean"))

mat_of_raw_fcs <- reduce(all_fcs$raw_rgdp_fc, rbind)


weigthed_fcs <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                        mat_cv_rmses_from_x = cv_all_x_rmse_each_h,
                        vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp)

weigthed_fcs[ is.nan(weigthed_fcs)] <- rgdp_uncond_fc_mean[ is.nan(weigthed_fcs)]


fcs_using_yoy_weights <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                 mat_cv_rmses_from_x = cv_all_x_rmse_each_h_yoy,
                                 vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy)

fcs_using_yoy_weights[ is.nan(fcs_using_yoy_weights)] <- rgdp_uncond_fc_mean[ is.nan(fcs_using_yoy_weights)]

weigthed_fcs <- ts(weigthed_fcs, 
                   start = stats::start(rgdp_uncond_fc_mean), 
                   frequency = 4)
fcs_using_yoy_weights <- ts(fcs_using_yoy_weights, 
                            start = stats::start(rgdp_uncond_fc_mean), 
                            frequency = 4)

final_rgdp_and_w_fc <- ts(c(rgdp_ts, weigthed_fcs), frequency = 4,
                              start = stats::start(rgdp_ts))

final_rgdp_and_yoyw_fc <- ts(c(rgdp_ts, fcs_using_yoy_weights), frequency = 4,
                          start = stats::start(rgdp_ts))

expo_final_rgdp_and_w_fc <- exp(final_rgdp_and_w_fc)
expo_final_rgdp_and_yoyw_fc <- exp(final_rgdp_and_yoyw_fc)

yoy_growth_expo_final_rgdp_and_w_fc <- diff(expo_final_rgdp_and_w_fc, lag = 4)/lag.xts(expo_final_rgdp_and_w_fc, k = 4)
yoy_growth_expo_final_rgdp_and_yoyw_fc <- diff(expo_final_rgdp_and_yoyw_fc, lag = 4)/lag.xts(expo_final_rgdp_and_yoyw_fc, k = 4)

# return(list(cv_all_x_rmse_each_h,
#             cv_all_x_rmse_each_h_yoy,
#             cv_rmse_each_h_rgdp,
#             cv_rmse_each_h_rgdp_yoy,
#             expo_final_rgdp_and_w_fc,
#             expo_final_rgdp_and_yoyw_fc,
#             yoy_growth_expo_final_rgdp_and_w_fc,
#             yoy_growth_expo_final_rgdp_and_yoyw_fc))

# # example with weights_vec set to 0.2, 0.2, 0.2, 0.2, 0.1, 0.1
# moo <- map(cv0_e_r, compute_rmse, h_max = 6, weights_vec = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1))
# moo


# each_h_rmse_012 <- cbind(reduce(cv0_rmse_each_h, rbind),
#                          reduce(cv1_rmse_each_h, rbind),
#                          reduce(cv2_rmse_each_h, rbind)
# )
# 


# ave_rmse_r_tbl <- as_tibble(ave_rmse_012) %>% 
#   mutate(id = monthly_names) %>% 
#   gather(key = "type", value = "rmse", -id) %>% 
#   cbind(all_fcs) %>% 
#   select(-c(id, type)) %>% 
#   arrange(id_fc) %>% 
#   group_by(id_fc) %>% 
#   mutate(min = min(rmse)) %>% 
#   filter(rmse == min) %>% 
#   mutate(rmse_rgdp = cv_rmse_rgdp) %>% 
#   filter(min <= rmse_rgdp) %>% 
#   mutate(fc_mean = map(fc, "mean")) %>% 
#   mutate(inv_mse = 1/(rmse^2)) %>% 
#   ungroup() %>% 
#   mutate(sum_inv_mse = sum(inv_mse),
#          fc_weight = inv_mse/sum_inv_mse)
# 
# ave_rmse_r_tbl_yoy <- as_tibble(ave_rmse_012_yoy) %>% 
#   mutate(id = monthly_names) %>% 
#   gather(key = "type", value = "rmse", -id) %>% 
#   cbind(all_fcs_yoy) %>% 
#   select(-c(id, type)) %>% 
#   arrange(id_fc) %>% 
#   group_by(id_fc) %>% 
#   mutate(min = min(rmse)) %>% 
#   filter(rmse == min) %>% 
#   mutate(rmse_rgdp_yoy = cv_rmse_rgdp_yoy) %>% 
#   filter(min <= rmse_rgdp_yoy) %>%
#   mutate(inv_mse = 1/(rmse^2)) %>% 
#   ungroup() %>% 
#   mutate(sum_inv_mse = sum(inv_mse),
#          fc_weight = inv_mse/sum_inv_mse)
# 
# fcs_and_weights <- ave_rmse_r_tbl %>% 
#   select(c(id_fc, type_fc, fc_weight, fc_mean)) %>% 
#   mutate(weighted_fc_means = map2(fc_weight, fc_mean, ~ .x * .y))
# 
# fcs_and_weights_yoy <- ave_rmse_r_tbl_yoy %>% 
#   select(c(id_fc, type_fc, fc_weight, yoy_fc_rgdp_mean)) %>% 
#   mutate(weighted_fc_means = map2(fc_weight, yoy_fc_rgdp_mean, ~ .x * .y))
# 
# w_fcs <- fcs_and_weights %>% select(weighted_fc_means) 
# w_fcs_yoy <- fcs_and_weights_yoy %>% select(weighted_fc_means) 
# 
# final_fc_mean <- colSums(reduce((reduce(w_fcs, ts.union)), rbind))
# final_fc_mean_yoy <- colSums(reduce((reduce(w_fcs_yoy, ts.union)), rbind))
# 
# final_rgdp_and_fc <- ts(c(rgdp_ts, final_fc_mean), frequency = 4,
#                               start = stats::start(rgdp_ts))
# 
# expo_final_rgdp_and_fc <- exp(final_rgdp_and_fc)
# 
# yoy_expo_final_rgdp_and_fc <- diff(expo_final_rgdp_and_fc, lag = 4)/lag.xts(expo_final_rgdp_and_fc, k = 4)
# 
# 
# direct_final_rgdp_and_fc_yoy <- diff(final_rgdp_and_fc, lag = 4)
# direct_final_fc_yoy <- subset(direct_final_rgdp_and_fc_yoy,
#                               start = length(direct_final_rgdp_and_fc_yoy) - h_max + 1)
# 
# 
# 
# m_arg <- read_excel("data/Argentina_m_analysis_rgdp.xlsx")
# 
# m_all_rmse <- m_arg[, c("rmse1", "rmse2", "rmse3", "rmse4", "rmse5", "rmse6", "rmse7", "rmse8")]
# 
# m_all_rmse$mean_rmse <- rowMeans(m_all_rmse)
# 
# m_all_rmse$cond_exo <- m_arg[ , "cond_exo"]
# 
# m_all_rmse <- arrange(m_all_rmse, rmse1)
# 
# 
