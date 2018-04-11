library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)
library(stringr)

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
  
  
  
  ext_series_xts_quarterly <- apply.quarterly(ext_series_xts_monthly , mean)
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
  
  xreg_as_y <- window(xreg_ts, start = c(y_start_year, y_start_quarter),
                   end = c(y_end_year, y_end_quarter), frequency = 4)
  
  n_x <- length(xreg_ts)
  
  number_of_xregs <- ncol(as.matrix(xreg_ts))
  
  for (x in 1:number_of_xregs) {
    
    if (is.null(ncol(xreg_as_y))) {
      x_series <-  xreg_as_y
    } else {
      x_series <-  xreg_as_y[ , x]
    }
    
    for (i in seq_along(1:n_cv)) {
      
      train_plus_test_plus_im1 <- training_length + h_max + (i - 1)
      start_training_index_y <-  n - train_plus_test_plus_im1 + 1
      end_training_index_y <-  start_training_index_y + training_length - 1
      start_test_index_y <- end_training_index_y + 1
      end_test_index_y <- start_test_index_y + h_max - 1
      
      print(paste("start training _y:", start_training_index_y))
      print(paste("training length", training_length))
      print(paste("end_training _y:", end_training_index_y))
      print(paste("start test _y:", start_test_index_y))
      print(paste("end test_y :", end_test_index_y))
      
      start_training_index_x <-  n_x - train_plus_test_plus_im1 + 1
      end_training_index_x <-  start_training_index_x + training_length - 1
      start_test_index_x <- end_training_index_x + 1
      end_test_index_x <- start_test_index_x + h_max - 1
      
      print(paste("start training x:", start_training_index_x))
      print(paste("end_training x :", end_training_index_x))
      print(paste("start test x:", start_test_index_x))
      print(paste("end test x:", end_test_index_x))
      
      print("y_ts")
      print(y_ts)
      
      print("x_as_y")
      print(x_series)
      
      training_y <- subset(y_ts, 
                           start = start_training_index_y,
                           end = end_training_index_y)
      
      print("training_y")
      print(training_y)
      
      training_x <- subset(x_series,
                           start = start_training_index_x,
                           end = end_training_index_x)
      
      print("training_x")
      print(training_x)
      
      test_y <- subset(y_ts, 
                       start = start_test_index_y,
                       end = end_test_index_y)
      
      print("test_y")
      print(test_y)
      
      test_x <- subset(x_series,
                       start = start_test_index_x,
                       end = end_test_index_x)
      
      print("test_x")
      print(test_x)
      
      this_arimax <- Arima(training_y, order = y_order,
                           seasonal = y_seasonal,
                           xreg = training_x)
      
      fc <- forecast(this_arimax, h = h_max, xreg = test_x)
      
      print("fc$mean")
      print(fc$mean)
      
      fc_error <- test_y - fc$mean
      print("fc_error")
      print(fc_error)
      
      
    }
    
    
  
  
  
  
}




i = 1
ny_without_na <- length(na.omit(y_ts)) 
n <- length(y_ts)

print(paste("n :", n))
print(paste("nnonas :", ny_without_na))

cv_all_errors_list <- list()

for (x_regressor in 1:ncol(as.matrix(xreg_ts))) {
  
  print(arima_names[x_regressor])
  
  if (is.null(ncol(xreg_ts))) {
    x_series <-  xreg_ts
  } else {
    x_series <-  xreg_ts[ , x_regressor]
  }
  
  x_same_end_as_y <-  window(x_series, end = c(2017, 4))
  
  n_x <- length(x_same_end_as_y)
  
  x_var_cv_errors <- matrix(data = NA, nrow = n_cv, ncol = h_max)
  
  for (i in seq_along(1:n_cv)) {
    
    train_plus_test_plus_im1 <- win_len + h_max + (i - 1)
    start_training_index_y <-  n - train_plus_test_plus_im1 + 1
    end_training_index_y <-  start_training_index_y + win_len - 1
    start_test_index_y <- end_training_index_y + 1
    end_test_index_y <- start_test_index_y + h_max - 1
    
    print(paste("start training _y:", start_training_index_y))
    print(paste("training length", win_len))
    print(paste("end_training _y:", end_training_index_y))
    print(paste("start test _y:", start_test_index_y))
    print(paste("end test_y :", end_test_index_y))
    
    start_training_index_x <-  n_x - train_plus_test_plus_im1 + 1
    end_training_index_x <-  start_training_index_x + win_len - 1
    start_test_index_x <- end_training_index_x + 1
    end_test_index_x <- start_test_index_x + h_max - 1
    
    print(paste("start training x:", start_training_index_x))
    print(paste("end_training x :", end_training_index_x))
    print(paste("start test x:", start_test_index_x))
    print(paste("end test x:", end_test_index_x))
    
    print("y_ts")
    print(y_ts)
    
    print("x_same_end_as_y")
    print(x_same_end_as_y)
    
    
    
    training_y <- subset(y_ts, 
                         start = start_training_index_y,
                         end = end_training_index_y)
    
    print("training_y")
    print(training_y)
    
    training_x <- subset(x_same_end_as_y,
                         start = start_training_index_x,
                         end = end_training_index_x)
    
    print("training_x")
    print(training_x)
    
    test_y <- subset(y_ts, 
                     start = start_test_index_y,
                     end = end_test_index_y)
    
    print("test_y")
    print(test_y)
    
    test_x <- subset(x_same_end_as_y,
                     start = start_test_index_x,
                     end = end_test_index_x)
    
    print("test_x")
    print(test_x)
    
    arimax_lagged <- Arima(training_y, order = rgdp_order,
                           seasonal = rgdp_seasonal,
                           include.mean = rgdp_inc_mean,
                           lambda = 0,
                           biasadj = TRUE,
                           xreg = training_x)
    
    fc <- forecast(arimax_lagged, h = h_max, xreg = test_x)
    
    print("fc$mean")
    print(fc$mean)
    
    
    
    fc_error <- test_y - fc$mean
    print("fc_error")
    print(fc_error)
    
    x_var_cv_errors[i, ] <- fc_error
    
  }
  
  print("x_var_cv_errors")
  print(x_var_cv_errors)
  
  cv_all_errors_list[[x_regressor]] <- x_var_cv_errors
  
}

print("cv_all_errors_list")
print(cv_all_errors_list)

return(cv_all_errors_list)

}
