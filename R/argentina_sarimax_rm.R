library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

data_path <- "./data/excel_data/Argentina.xlsx"

arg_m <- read_excel(data_path, sheet = "monthly")
arg_gdp <- read_excel(data_path, sheet = "quarterly", range = "A1:E113")
country_m <- arg_m
country_gdp <- arg_gdp
start_year <- min(country_m$year)
start_month <- 1
start_year_gdp <- min(country_gdp$year)
start_quarter <- 1
this_start  <- c(start_year, start_month)
this_start_gdp <- c(start_year_gdp, start_quarter)
arima_names <- names(country_m)[5:ncol(country_m)]
gdp_name <- "rgdp"
data_m_ts <- tk_ts(country_m, frequency = 12, 
                   start = this_start)
data_m_ts <- data_m_ts[ , arima_names]
rgdp_ts <- tk_ts(country_gdp, frequency = 4, 
                   start = this_start_gdp)
rgdp_ts <- rgdp_ts[ , gdp_name]

country_dmetra <- read_excel(data_path, sheet = "optimal")
country_dmetra <- country_dmetra %>% 
  separate(data = ., col = 1, into = c("freq_type", "id"))

country_rgdp_dmetra <- country_dmetra %>% 
  filter(id == "rgdp") %>% select(- freq_type)
  
country_monthly_dmetra <- country_dmetra %>% 
  filter(id != "rgdp") %>% select(- freq_type)

country_monthly_dmetra$id <- as.factor(country_monthly_dmetra$id) 

rgdp_order <- c(country_rgdp_dmetra$P, country_rgdp_dmetra$D,
                country_rgdp_dmetra$Q)
rgdp_seasonal <- c(country_rgdp_dmetra$BP, country_rgdp_dmetra$BD, 
                   country_rgdp_dmetra$BQ)
rgdp_inc_mean <- ifelse(country_rgdp_dmetra$Mean == 1, TRUE, FALSE)



arima_list <- list_along(arima_names)

tic()
for (i in seq_along(country_monthly_dmetra$id)) {
  
  print(i)
  
  this_order <- c(country_monthly_dmetra$P[i], country_monthly_dmetra$D[i],
                  country_monthly_dmetra$Q[i])
  
  this_seasonal <- c(country_monthly_dmetra$BP[i], country_monthly_dmetra$BD[i], 
                     country_monthly_dmetra$BQ[i])
  
  inc_mean <- ifelse(country_monthly_dmetra$Mean[i] == 1, TRUE, FALSE)
  
  # this_arima_lev <- Arima(data_m_ts[,i], order = this_order,
  #                         seasonal = this_seasonal, include.mean = inc_mean)
  # 
  # this_arima_log <- Arima(log(data_m_ts[,i]), order = this_order,
  #                         seasonal = this_seasonal, include.mean = inc_mean)

  this_arima_lam <- Arima(data_m_ts[,i], order = this_order,
                          seasonal = this_seasonal, include.mean = inc_mean,
                          lambda = 0, biasadj = TRUE)
  arima_list[[i]] <- this_arima_lam
}
toc()

arima_gdp <- Arima(rgdp_ts, order = rgdp_order,
                   seasonal = rgdp_seasonal, 
                   include.mean = rgdp_inc_mean,
                   lambda = 0, 
                   biasadj = TRUE)

# p q P Q freq d D  
arima_order_names <- c("p_r", "q_r", "P_r", "Q_r", "freq_r", "d_r", "D_r")
arima_list_m_order <- list()

# rgdp
gdp_r_par <- arima_gdp[["arma"]]
names(gdp_r_par) <- arima_order_names

gdp_r_par_tbl <- tibble(id_r = "rgdp", p_r = gdp_r_par[1], q_r = gdp_r_par[2], 
       P_r = gdp_r_par[3], Q_r = gdp_r_par[4], freq_r = gdp_r_par[5], 
       d_r = gdp_r_par[6], D_r = gdp_r_par[7])

dmetra_and_r_order_rgdp <- cbind(gdp_r_par_tbl, country_rgdp_dmetra)
dmetra_and_r_order_rgdp[,  c("BIC", "N", "Seasonal", "Q-val", "SE(res)", "Log", "Mean", "id_r")] <- NULL

dmetra_and_r_order_rgdp$pdiff  <-  dmetra_and_r_order_rgdp$p_r - dmetra_and_r_order_rgdp$P
dmetra_and_r_order_rgdp$qdiff  <-  dmetra_and_r_order_rgdp$q_r - dmetra_and_r_order_rgdp$Q
dmetra_and_r_order_rgdp$Pdiff  <-  dmetra_and_r_order_rgdp$P_r - dmetra_and_r_order_rgdp$BP
dmetra_and_r_order_rgdp$Qdiff  <-  dmetra_and_r_order_rgdp$Q_r - dmetra_and_r_order_rgdp$BQ
dmetra_and_r_order_rgdp$ddiff  <-  dmetra_and_r_order_rgdp$d_r - dmetra_and_r_order_rgdp$D
dmetra_and_r_order_rgdp$Ddiff  <-  dmetra_and_r_order_rgdp$D_r - dmetra_and_r_order_rgdp$BD

### monthly-variables

# id <- arima_names
id <- country_monthly_dmetra$id
id_r <- country_monthly_dmetra$id


for(i in 1:length(arima_names)) {
  arima_list_m_order[[i]] <- arima_list[[i]][["arma"]]
  names(arima_list_m_order[[i]]) <- arima_order_names
}

arima_order_tbl <- as.tibble(reduce(arima_list_m_order, rbind))
arima_order_tbl <- cbind(id, arima_order_tbl)

names(arima_order_tbl)[1] <- "id_r"
# joint_tbl <- left_join(arima_order_tbl, country_monthly_dmetra, by = "id")

dmetra_and_r_order <- cbind(arima_order_tbl, country_monthly_dmetra)
dmetra_and_r_order[,  c("BIC", "N", "Seasonal", "Q-val", "SE(res)", "Log", "Mean", "id_r")] <- NULL

dmetra_and_r_order$pdiff  <-  dmetra_and_r_order$p_r - dmetra_and_r_order$P
dmetra_and_r_order$qdiff  <-  dmetra_and_r_order$q_r - dmetra_and_r_order$Q
dmetra_and_r_order$Pdiff  <-  dmetra_and_r_order$P_r - dmetra_and_r_order$BP
dmetra_and_r_order$Qdiff  <-  dmetra_and_r_order$Q_r - dmetra_and_r_order$BQ
dmetra_and_r_order$ddiff  <-  dmetra_and_r_order$d_r - dmetra_and_r_order$D
dmetra_and_r_order$Ddiff  <-  dmetra_and_r_order$D_r - dmetra_and_r_order$BD

# playing with dates, to educate ourselves
final_date_dec <- 2019 + (11/12) 

arima_list_m <- list_along(arima_names)

tic()
for (i in seq_along(arima_names)) {
  print(paste("i =", i, ",", arima_names[i]))
  monthly_series <- data_m_ts[, arima_names[i]]
  fit <- auto.arima(monthly_series)
  
  arima_list_m[[i]] <- fit
  
}
toc()


glue_x_mean <- function(ts1, ts2, freq = 12) {
  
  ts1_new_length <- length(ts1) - length(ts2)
  ts1_new_data <- ts1[1:ts1_new_length]
  
  ts12 <- ts( data = c(ts1_new_data, ts2), 
              frequency = freq,
              start = stats::start(ts1)
  )
  
  return(ts12)
}

fc_list_m <- list() 
extended_m_ts_list <- list()


for (i in seq_along(arima_names)) {
  print(paste("i =", i, ",", arima_names[i]))
  
  this_arima <- arima_list_m[[i]]
  monthly_series <- data_m_ts[, arima_names[i]]
  
  # series_date_max <- max(time(na.omit(monthly_series)))
  series_date_max <- monthly_series %>% na.omit() %>% time %>% max
  diff_decimal <- final_date_dec - series_date_max 
  diff_in_month <- as.integer(12 * diff_decimal)
  
  this_fc <- forecast(this_arima, h = diff_in_month)
  
  fc_mean <- this_fc$mean
  
  extended_monthly_series <- glue_x_mean(monthly_series, fc_mean)
  
  fc_list_m[[i]] <- this_fc
  extended_m_ts_list[[i]] <- extended_monthly_series
  
}

names(fc_list_m) <- arima_names
names(extended_m_ts_list) <- arima_names

ext_monthly_series_mts <- reduce(extended_m_ts_list, ts.union)
colnames(ext_monthly_series_mts) <- arima_names

index_date <- as.Date(time(ext_monthly_series_mts))

ext_monthly_series_tbl <- as.tibble(ext_monthly_series_mts) 
ext_monthly_series_tbl <- cbind(tibble(date = index_date), ext_monthly_series_tbl)

ext_series_xts_monthly <- tk_xts(ext_monthly_series_tbl, date_var = date)

ext_series_xts_quarterly <- apply.quarterly(ext_series_xts_monthly , mean)

ext_series_ts_quarterly <- tk_ts(ext_series_xts_quarterly, start = c(1993,1), frequency = 4)

rgdp_arimax_list <- list_along(arima_names)
tic()
for (i in seq_along(arima_names)) {
  
  print(i)
  
  x_series <-  ext_series_xts_quarterly[, 1] 
  x_series <- x_series["1993/2017-12"]
  
  this_arimax <- list_along(1:3)
  
  arima_gdp_x_0 <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                         seasonal = rgdp_seasonal, 
                         include.mean = rgdp_inc_mean,
                         lambda = 0, 
                         biasadj = TRUE,
                         xreg = lag.xts(x_series, k=0))
  
  arima_gdp_x_1 <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                         seasonal = rgdp_seasonal, 
                         include.mean = rgdp_inc_mean,
                         lambda = 0, 
                         biasadj = TRUE,
                         xreg = lag.xts(x_series, k=1))
  
  arima_gdp_x_2 <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                         seasonal = rgdp_seasonal, 
                         include.mean = rgdp_inc_mean,
                         lambda = 0, 
                         biasadj = TRUE,
                         xreg = lag.xts(x_series, k=2))
  
  this_arimax[[1]] <- arima_gdp_x_0
  this_arimax[[2]] <- arima_gdp_x_1
  this_arimax[[3]] <- arima_gdp_x_2
  
  rgdp_arimax_list[[i]] <-  this_arimax
  
}
toc()

names(rgdp_arimax_list) <- arima_names


myts1 <- ext_series_ts_quarterly[, 1]
myts2 <- ext_series_ts_quarterly[, 1:2]

## cross validation part

rgdp_tscv_list <- list_along(arima_names)

arimax_cv <- function(y_ts, xreg_ts, win_len, n_cv, x_maxlag, h_max) {
  
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
  


# 
# arcv <- arimax_cv(y_ts = rgdp_ts, xreg_ts = myts1, x_maxlag = 2, 
#                   win_len = 16, n_cv = 8, h_max = 4)  


arcv <- arimax_cv(y_ts = rgdp_ts, xreg_ts = myts2, x_maxlag = 2, 
                  win_len = 16, n_cv = 8, h_max = 4)  

# arcv <- arimax_cv(y_ts = rgdp_ts, xreg_ts = ext_series_ts_quarterly, 
#                   x_maxlag = 2, win_len = 16, n_cv = 8, h_max = 4)  


# for (xlag in seq.int(0, x_maxlag)) {
#   
#   this_lagged_x <- lag.xts(x_series, k = xlag)
  


# 
# for( i in seq_along(1:num_of_cv_elements)){
#   
#   
#   new_y <- subset(rgdp_ts, start = n - training_set_length - max_h - i + 1 + 1,
#          end = n - max_h - i + 1)
#   foo_list <- list() 
#   for (j in seq_along(arima_names)) {
#     x_series <-  ext_series_ts_quarterly[, 1]
#     x_series <-  window(x_series, end = c(2017, 4))
#     new_x <- subset(x_series, start = n - training_set_length - max_h - i + 1 + 1,
#                     end = n - max_h - i + 1)
#     
#     this_arimax <- list_along(1:3)
#     
#     arima_gdp_x_0 <- Arima(new_y, order = rgdp_order,
#                            seasonal = rgdp_seasonal, 
#                            include.mean = rgdp_inc_mean,
#                            lambda = 0, 
#                            biasadj = TRUE,
#                            xreg = lag.xts(new_x, k=0))
#     
#     
#     
#     arima_gdp_x_1 <- Arima(new_y,  order = rgdp_order,
#                            seasonal = rgdp_seasonal, 
#                            include.mean = rgdp_inc_mean,
#                            lambda = 0, 
#                            biasadj = TRUE,
#                            xreg = lag.xts(new_x, k=1))
#     
#     arima_gdp_x_2 <- Arima(new_y, order = rgdp_order,
#                            seasonal = rgdp_seasonal, 
#                            include.mean = rgdp_inc_mean,
#                            lambda = 0, 
#                            biasadj = TRUE,
#                            xreg = lag.xts(new_x, k=2))
#     
#     this_arimax[[1]] <- arima_gdp_x_0
#     this_arimax[[2]] <- arima_gdp_x_1
#     this_arimax[[3]] <- arima_gdp_x_2
#     
#     foo_list[[j]] <- this_arimax
#     
#   }
#   
#   
# 
# }
# 
# rgdp_ts
# new_y
# 
# ari_for_tsCV <- function(my_data, this_order, this_seasonal, this_mean = FALSE, 
#                          this_lambda = 0, this_bias = TRUE, h = 1) {
#   
#   my_arima_obj <- Arima(my_data, 
#                         order = this_order,
#                         seasonal = this_seasonal, 
#                         include.mean = this_mean,
#                         lambda = this_lambda, 
#                         biasadj = this_bias)
#   
#   my_fc_obj <- forecast(my_arima_obj, h = h)
#   
#   return(my_fc_obj)
# }
# 
# e_rgdp_1 <- rgdp_ts %>% tsCV(forecastfunction=ari_for_tsCV, this_order = rgdp_order,
#                              this_seasonal=rgdp_seasonal, h = 1)
# e_rgdp_4 <- rgdp_ts %>% tsCV(forecastfunction=ari_for_tsCV, this_order = rgdp_order,
#                              this_seasonal=rgdp_seasonal, h = 4)
# 
# mse_1 <- mean(e_rgdp_1^2, na.rm = TRUE)
# mse_4 <- mean(e_rgdp_4^2, na.rm = TRUE)
# rmse_1 <- sqrt(mse_1)
# rmse_4 <- sqrt(mse_4)
# 


