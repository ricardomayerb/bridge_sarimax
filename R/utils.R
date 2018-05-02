library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)
library(stringr)

bsarimax_as_function <- function(data_path, train_span = 16, h_max = 6,
                                 number_of_cv = 8, 
                                 final_forecast_horizon = c(2019, 12),
                                 outer_cv_round = 0) {
  
  
  gdp_and_dates <- get_rgdp_and_dates(data_path)
  
  monthly_data <- get_monthly_variables(data_path = data_path)
  monthly_ts <- make_monthly_ts(monthly_data)
  monthly_ts  <- log(monthly_ts)
  monthly_names <- colnames(monthly_ts)
  
  rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
                start = gdp_and_dates[["gdp_start"]], frequency = 4)
  rgdp_ts <- log(rgdp_ts)
  
  if(outer_cv_round > 0) {
    total_obs <- length(rgdp_ts)
    print(total_obs)
    
    print("rgdp_ts")
    print(rgdp_ts)
    
    outer_obs <- total_obs - outer_cv_round  + 1
    print(outer_obs)
    
    cv_rgdp_ts <- ts(data = rgdp_ts[1:outer_obs], 
                     start = gdp_and_dates[["gdp_start"]], frequency = 4)
    
    print("cv_rgdp_ts")
    print(cv_rgdp_ts)
    
    traininig_set_rgdp <- subset(cv_rgdp_ts, 
                                 start = outer_obs - h_max - train_span + 1,
                                 end = outer_obs - h_max)
    
    print("traininig_set_rgdp")
    print(traininig_set_rgdp)
    
    test_set_rgdp <- subset(cv_rgdp_ts, 
                                 start = outer_obs - h_max + 1,
                                 end = outer_obs)
    
    print("test_set_rgdp")
    print(test_set_rgdp)
    
    rgdp_ts <- traininig_set_rgdp
    
  }
  
  demetra_output <- get_demetra_params(data_path)
  
  fit_arima_rgdp_list_dem <- fit_arimas(
    y_ts = rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
    this_arima_names = "rgdp")

  fit_arima_monthly_list_dem <- fit_arimas(
    y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
    this_arima_names = monthly_names)
  
  gdp_order_dm <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]
  
  monthly_order_dm <- get_order_from_arima(fit_arima_monthly_list_dem, 
                                           suffix = "dm",
                                           this_arima_names = monthly_names)
  
  mdata_ext_dm <- extend_and_qtr(data_mts = monthly_ts, 
                                 final_horizon_date = final_forecast_horizon , 
                                 vec_of_names = monthly_names, 
                                 fitted_arima_list = fit_arima_monthly_list_dem,
                                 start_date_gdp = gdp_and_dates[["gdp_start"]])
  
  mdata_ext_dm_ts <- mdata_ext_dm[["series_ts"]]
  
  rgdp_order_dm <-  gdp_order_dm[c("p", "d", "q")]
  rgdp_seasonal_dm <-  gdp_order_dm[c("P", "D", "Q")]
  
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
  
  # example with weights_vec set to default
  cv0_rmse_list_dm <- map(cv0_e_dm, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  cv1_rmse_list_dm <- map(cv1_e_dm, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  cv2_rmse_list_dm <- map(cv2_e_dm, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  
  cv_rdgp_rmse_dm <- compute_rmse(cv_rgdp_e_dm, h_max = h_max, n_cv = number_of_cv)
  
  cv0_rmse_wm_list_dm <- map(cv0_rmse_list_dm, "weighted_same_h")
  cv0_rmse_wm_dm <- map_dbl(cv0_rmse_list_dm, "weighted_same_h")
  cv1_rmse_wm_dm <- map_dbl(cv1_rmse_list_dm, "weighted_same_h")
  cv2_rmse_wm_dm <- map_dbl(cv2_rmse_list_dm, "weighted_same_h")
  
  cv_rmse_rgdp_dm <- cv_rdgp_rmse_dm[["weighted_same_h"]]
  
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
  
  return(final_fc_mean_dm)
}

get_rgdp_and_dates <- function(data_path, gdp_name = "rgdp", 
                               date_name = "date") {
  
  data <- read_excel(data_path, sheet = "quarterly") 

  data <- data[ !is.na(data[, gdp_name]) , ]

  start_year_gdp <- min(data$year)
  start_quarter_gdp <- min(data$quarter)
  
  end_year_gdp <- max(data$year)
  end_quarter_gdp <- max(data$quarter)
  
  gdp_data <- data[, gdp_name]
  
  return(list(gdp_data = gdp_data,
              gdp_start = c(start_year_gdp, start_quarter_gdp),
              gdp_end = c(end_year_gdp, end_quarter_gdp))
  )
  
}

get_monthly_variables <- function(data_path, vars_to_eliminate = 
                                    c("hlookup")) {
  data <- read_excel(data_path, sheet = "monthly")
  
  for (variable in vars_to_eliminate) {
    data[, variable] <- NULL 
  }
  
  return(data)
  
}

make_monthly_ts <- function(data, 
                           names_to_exclude = c("date", "year", "month")) {
  
  start_year <- min(data$year)
  start_month <- min(data$month)
  
  all_names <- names(data)
  names_to_keep <- all_names[!str_detect(all_names, names_to_exclude)]
  
  data_ts <- tk_ts(data, frequency = 12, 
                     start = c(start_year, start_month))
  
  data_ts <- data_ts[ , names_to_keep]
  
  return(data_ts)
  
  
}

get_demetra_params <- function(data_path) {
  
  optimal_demetra <- read_excel(data_path, sheet = "optimal")
  optimal_demetra <- optimal_demetra %>% 
    separate(data = ., col = 1, into = c("freq_type", "id"), sep = " - ")

  optimal_demetra <- optimal_demetra %>% 
    rename(p_dm = P, q_dm = Q, d_dm = D, P_dm = BP, Q_dm = BQ, D_dm = BD)

  rgdp_demetra <- optimal_demetra %>% 
    filter(id == "rgdp") %>% select(- freq_type)
  
  rgdp_demetra_pdqPDQ <- rgdp_demetra %>% 
    select(p_dm, d_dm, q_dm, P_dm, D_dm, Q_dm)
  
  monthly_demetra <- optimal_demetra %>% 
    filter(id != "rgdp") %>% select(- freq_type)
  
  monthly_demetra$id <- as.factor(monthly_demetra$id) 
  
  monthly_demetra_pdqPDQ <- monthly_demetra %>% 
    select(id, p_dm, d_dm, q_dm, P_dm, D_dm, Q_dm)
  
  monthly_names <- monthly_demetra$id
  
  rgdp_order <- c(rgdp_demetra$p_dm, rgdp_demetra$d_dm, rgdp_demetra$q_dm)
  rgdp_seasonal <- c(rgdp_demetra$P_dm, rgdp_demetra$D_dm, rgdp_demetra$Q_dm)
  rgdp_inc_mean <- ifelse(rgdp_demetra$Mean == 1, TRUE, FALSE)
  
  this_instruction <- list(order = rgdp_order, seasonal = rgdp_seasonal,
                           mean_logical = rgdp_inc_mean)
  
  rgdp_order_list <- list()
  
  rgdp_order_list[[1]] <- this_instruction 
  
  monthly_demetra_order_list <- list_along(monthly_names)
 
  for (i in seq_along(monthly_names)) {

    this_order <- c(monthly_demetra$p_dm[i], monthly_demetra$d_dm[i],
                    monthly_demetra$q_dm[i])
    
    this_seasonal <- c(monthly_demetra$P_dm[i], monthly_demetra$D_dm[i],
                       monthly_demetra$Q_dm[i])
    
    inc_mean <- ifelse(monthly_demetra$Mean[i] == 1, TRUE, FALSE)
    
    this_instruction <- list(order = this_order, seasonal = this_seasonal,
                              mean_logical = inc_mean)

    monthly_demetra_order_list[[i]] <- this_instruction
  }
  
  names(monthly_demetra_order_list) <- monthly_names
  
  return(list(monthly_order_list = monthly_demetra_order_list,
              monthly_info_tbl = monthly_demetra,
              monthly_pdqPDQ = monthly_demetra_pdqPDQ,
              rgdp_order_list = rgdp_order_list,
              rgdp_info_tbl = rgdp_demetra,
              rgdp_pqdPDQ = rgdp_demetra_pdqPDQ) 
         )
}

fit_arimas <- function(y_ts, auto = FALSE, order_list = NULL, my_lambda = NULL,
                       my_biasadj = FALSE, this_arima_names = NULL) {
  
  n_of_series <- ncol(y_ts)
  
  if (is.null(n_of_series)) {
    n_of_series <- 1
  }
  
  
  fit_arimas_list <- list_along(seq.int(1, n_of_series))
  
  for (i in seq.int(1, n_of_series)) {
    
    this_y <- y_ts[, i]
    
    if (!auto) {
      this_instruction <- order_list[[i]]
      this_order <- this_instruction[["order"]]
      this_seasonal <- this_instruction[["seasonal"]]
      this_mean <- this_instruction[["mean_logical"]]
      
      fit <- Arima(y = this_y, order = this_order, seasonal = this_seasonal,
                  include.mean = this_mean, lambda = my_lambda, 
                  biasadj = my_biasadj)
      
    } else {
      
      fit <- auto.arima(y = this_y, lambda = my_lambda, biasadj = my_biasadj)
      
    }
    
    fit_arimas_list[[i]] <- fit
    
  }
  
  names(fit_arimas_list) <- this_arima_names
  
  return(fit_arimas_list)
  
}


get_order_from_arima <- function(arima_obj, suffix = NULL, 
                                 this_arima_names = NULL) {
  
  len_obj <- length(arima_obj)
  
  order_names <- c("p", "q", "P", "Q", "freq", "d", "D")
  
  if (!is.null(suffix)) {
    order_names <- paste(order_names, suffix, sep = "_")
  }
  
  order_par_list <- list_along(seq.int(1, len_obj))
  
  for (i in seq.int(1, len_obj)) {
    
    this_arima <- arima_obj[[i]]
    arma_par <- this_arima[["arma"]]
    names(arma_par) <- order_names
    
    order_par_list[[i]] <- arma_par
    
  }
  
  
  names(order_par_list) <- this_arima_names
  
  return(order_par_list)
  
}

compare_two_orders <- function(ord1, ord2, vec_of_names) {
  
  ord1_df <- t(as.data.frame(ord1))
  ord2_df <- t(as.data.frame(ord2))
  
  order_both_df <- cbind(ord1_df, ord2_df)
  
  order_both_diff_df <- as.data.frame(order_both_df) %>%
    mutate(p_diff = p_r - p_dm, q_diff = q_r - q_dm, d_diff = d_r - d_dm,
           P_diff = P_r - P_dm, Q_diff = Q_r - Q_dm, D_diff = D_r - D_dm) %>%
    select(c(p_diff, q_diff, d_diff, P_diff, Q_diff, D_diff)) %>% 
    mutate(id = vec_of_names)
  
  return(
    list(diffs = order_both_diff_df, both = order_both_df)
    )
}


glue_x_mean <- function(ts1, ts2, freq = 12) {
  
  ts1_new_length <- length(ts1) - length(ts2)
  ts1_new_data <- ts1[1:ts1_new_length]
  
  ts12 <- ts( data = c(ts1_new_data, ts2), 
              frequency = freq,
              start = stats::start(ts1)
  )
  
  return(ts12)
}

extend_and_qtr <- function(data_mts, final_horizon_date, vec_of_names, 
                           fitted_arima_list, start_date_gdp) {
  
  fc_list_m <- list() 
  extended_m_ts_list <- list()
  
  final_year <- final_horizon_date[1]
  final_month <- final_horizon_date[2] - 1
  
  final_horizon_decimal <- final_year + final_month/12
  
  for (i in seq_along(vec_of_names)) {

    this_arima <- fitted_arima_list[[i]]
    monthly_series <- data_mts[, vec_of_names[i]]
    
    series_date_max <- monthly_series %>% na.omit() %>% time %>% max
    diff_decimal <- final_horizon_decimal - series_date_max 
    diff_in_month <- as.integer(12 * diff_decimal)
    
    this_fc <- forecast(this_arima, h = diff_in_month)
    
    fc_mean <- this_fc$mean
    
    extended_monthly_series <- glue_x_mean(monthly_series, fc_mean)
    
    fc_list_m[[i]] <- this_fc
    extended_m_ts_list[[i]] <- extended_monthly_series
    
  }
  
  names(fc_list_m) <- vec_of_names
  names(extended_m_ts_list) <- vec_of_names
  
  ext_monthly_series_mts <- reduce(extended_m_ts_list, ts.union)
  colnames(ext_monthly_series_mts) <- vec_of_names
  
  index_date <- as.Date(time(ext_monthly_series_mts))
  
  rgdp_start_year <- start_date_gdp[1]
  rgdp_start_quarter <- start_date_gdp[2]
  start_gdp_str <- paste0(rgdp_start_year, "-", rgdp_start_quarter, "/")

  ext_monthly_series_tbl <- as.tibble(ext_monthly_series_mts) 
  ext_monthly_series_tbl <- cbind(tibble(date = index_date), ext_monthly_series_tbl)
  
  ext_series_xts_monthly <- tk_xts(ext_monthly_series_tbl, date_var = date)
  
  
  
  ext_series_xts_quarterly <- apply.quarterly(ext_series_xts_monthly , 
                                              mean, na.rm = TRUE)
  ext_series_xts_quarterly <- ext_series_xts_quarterly[start_gdp_str]
  
  ext_series_ts_quarterly <- tk_ts(ext_series_xts_quarterly, 
                                   start = c(rgdp_start_year, rgdp_start_quarter), 
                                   frequency = 4)
  
  
  # chop off any obs previous to the start of gdp
  
  return(list(series_ts = ext_series_ts_quarterly,
              series_xts = ext_series_xts_quarterly))
  
  # return(ext_series_ts_quarterly)
  
}


my_arimax <- function(y_ts, xreg_ts, y_order, y_seasonal,
                      y_include_mean = FALSE, 
                      vec_of_names = NULL) {
 
  i = 1
  
  y_ts <- na.omit(y_ts)
  
  y_time <- time(y_ts)
  y_yqrt <- as.yearqtr(y_time)

  y_end_year <- year(max( y_yqrt ))
  y_end_quarter <- quarter(max( y_yqrt ))
  
  y_start_year <- year(min( y_yqrt ))
  y_start_quarter <- quarter(min( y_yqrt ))

  n <- length(y_ts)
  
  x_names <- colnames(xreg_ts)
  
  number_of_xregs <- ncol(as.matrix(xreg_ts))
  
  arimax_list <-  list()

  for (x_regressor in 1:number_of_xregs) {

    if (is.null(ncol(xreg_ts))) {
      x_series <-  xreg_ts
    } else {
      x_series <-  xreg_ts[ , x_regressor]
    }
    
    
    x_as_y <- window(x_series, start = c(y_start_year, y_start_quarter),
                              end = c(y_end_year, y_end_quarter),
                     frequency = 4)

    n_x <- length(x_as_y)
    
    this_arimax <- Arima(y = y_ts, xreg = x_as_y, 
                         order = y_order, seasonal = y_seasonal,
                         include.mean = y_include_mean )
    
    arimax_list[[x_regressor]] <- this_arimax

    
  }
  
  names(arimax_list) <- vec_of_names
  
  return(arimax_list)
  

}


cv_arimax <- function(y_ts, xreg_ts, h_max, n_cv, training_length,
                      y_order, y_seasonal,
                      y_include_mean = FALSE, 
                      vec_of_names = NULL, method = "CSS") {
  
  i = 1
  y_ts <- na.omit(y_ts)
  
  y_time <- time(y_ts)
  y_yqrt <- as.yearqtr(y_time)
  
  y_end_year <- year(max( y_yqrt ))
  y_end_quarter <- quarter(max( y_yqrt ))
  
  y_start_year <- year(min( y_yqrt ))
  y_start_quarter <- quarter(min( y_yqrt ))
  
  n <- length(y_ts)
  
  xreg_as_y <- window(xreg_ts, start = c(y_start_year, y_start_quarter),
                   end = c(y_end_year, y_end_quarter), frequency = 4)
  
  number_of_xregs <- ncol(as.matrix(xreg_ts))
  cv_errors_all_pairs_yx <- list_along(seq.int(1, number_of_xregs)) 
  
  
  for (x in 1:number_of_xregs) {
    
    if (is.null(ncol(xreg_as_y))) {
      x_series <-  xreg_as_y
    } else {
      x_series <-  xreg_as_y[ , x]
    }
    
    n_x <- length(x_series)
    
    cv_errors_this_x <- list_along(1:n_cv)
    
    for (i in seq_along(1:n_cv)) {
      
      train_plus_test_plus_im1 <- training_length + h_max + (i - 1)
      start_training_index_y <-  n - train_plus_test_plus_im1 + 1
      end_training_index_y <-  start_training_index_y + training_length - 1
      start_test_index_y <- end_training_index_y + 1
      end_test_index_y <- start_test_index_y + h_max - 1

      start_training_index_x <-  n_x - train_plus_test_plus_im1 + 1
      end_training_index_x <-  start_training_index_x + training_length - 1
      start_test_index_x <- end_training_index_x + 1
      end_test_index_x <- start_test_index_x + h_max - 1

      
      training_y <- subset(y_ts, 
                           start = start_training_index_y,
                           end = end_training_index_y)

      
      training_x <- subset(x_series,
                           start = start_training_index_x,
                           end = end_training_index_x)

      test_y <- subset(y_ts, 
                       start = start_test_index_y,
                       end = end_test_index_y)

      test_x <- subset(x_series,
                       start = start_test_index_x,
                       end = end_test_index_x)

      this_arimax <- Arima(training_y, order = y_order,
                           seasonal = y_seasonal,
                           xreg = training_x,
                           method = method)
      
      this_fc <- forecast(this_arimax, h = h_max, xreg = test_x)

      fc_error <- test_y - this_fc$mean

      cv_errors_this_x[[i]] <- fc_error
      
    }
    
    cv_errors_all_pairs_yx[[x]] <- cv_errors_this_x
  }
  
  cv_errors_all_pairs_yx <- map(cv_errors_all_pairs_yx, reduce, rbind)
  names(cv_errors_all_pairs_yx) <- vec_of_names
  return(cv_errors_all_pairs_yx)
}


cv_arima <- function(y_ts,  h_max, n_cv, training_length,
                      y_order, y_seasonal,
                      y_include_mean = FALSE, 
                      method = "CSS") {
  
  i = 1
  y_ts <- na.omit(y_ts)
  
  y_time <- time(y_ts)
  y_yqrt <- as.yearqtr(y_time)
  
  y_end_year <- year(max( y_yqrt ))
  y_end_quarter <- quarter(max( y_yqrt ))
  
  y_start_year <- year(min( y_yqrt ))
  y_start_quarter <- quarter(min( y_yqrt ))
  
  n <- length(y_ts)
  
  cv_errors <- list_along(1:n_cv)
  
  for (i in seq_along(1:n_cv)) {
      
    train_plus_test_plus_im1 <- training_length + h_max + (i - 1)
    start_training_index_y <-  n - train_plus_test_plus_im1 + 1
    end_training_index_y <-  start_training_index_y + training_length - 1
    start_test_index_y <- end_training_index_y + 1
    end_test_index_y <- start_test_index_y + h_max - 1
      
    training_y <- subset(y_ts, 
                         start = start_training_index_y,
                         end = end_training_index_y)
      
    test_y <- subset(y_ts, 
                     start = start_test_index_y,
                     end = end_test_index_y)
      
    this_arima <- Arima(training_y, order = y_order,
                        seasonal = y_seasonal,
                        method = method)
      
    this_fc <- forecast(this_arima, h = h_max)
      
    fc_error <- test_y - this_fc$mean
      
    cv_errors[[i]] <- fc_error
      
  }
  
  cv_errors <- reduce(cv_errors, rbind)
  
  return(cv_errors)
  
}
  



compute_rmse <- function(mycv, h_max, n_cv, col_weights_vec = NULL,
                         row_weights_vec = NULL) {
  
  vec_h <- 1:h_max
  vec_cv <- 1:n_cv
  
  horizon_names <- paste("h = ", vec_h)
  cv_names <- paste("cv = ", vec_cv)
  
  same_h_mse <- colMeans(mycv^2, na.rm = T)
  same_h_rmse <- same_h_mse %>% sqrt()
  same_h_rmse <- as.data.frame(t(same_h_rmse))
  colnames(same_h_rmse) <- horizon_names 
  
  across_hs_mse <- rowMeans(mycv^2, na.rm = T)
  across_hs_rmse <- across_hs_mse %>% sqrt()
  across_hs_rmse <- as.data.frame(t(across_hs_rmse))
  colnames(across_hs_rmse) <- cv_names 
  
  if(is.null(col_weights_vec))
    col_weights_vec <- seq(1/length(vec_h), by = 0, length.out = length(vec_h))
  else col_weights_vec <- col_weights_vec
  
  weighted_same_h_rmse_all_hs <- weighted.mean(same_h_rmse, col_weights_vec)

  if(is.null(row_weights_vec))
    row_weights_vec <- seq(1/length(vec_cv), by = 0, length.out = length(vec_cv))
  else row_weights_vec <- row_weights_vec
  
  weighted_across_h_rmse_all_cvs <- weighted.mean(across_hs_rmse, row_weights_vec)
  
  return(list(same_h_rmse = same_h_rmse, 
              across_hs_rmse = across_hs_rmse,
              weighted_same_h = weighted_same_h_rmse_all_hs,
              weighted_across_hs = weighted_across_h_rmse_all_cvs))
}


forecast_xreg <- function(arimax_list, xreg_mts, h, vec_of_names = NULL) {
  
  n_vars <- length(arimax_list)
  
  fc_list <- list()
  
  for(i in seq.int(1, n_vars)) {

    this_arimax <- arimax_list[[i]]
    this_series <- xreg_mts[,i]
    
    n_obs <- length(this_series)
    
    series_for_fc <- this_series[(n_obs-h+1) : n_obs]
    
    this_fc <- forecast(this_arimax, h = h, xreg = series_for_fc)
    
    fc_list[[i]] <- this_fc
    
  }
  
  names(fc_list) <- vec_of_names
  return(fc_list)
  
}
