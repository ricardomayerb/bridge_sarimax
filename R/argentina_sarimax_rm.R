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

## rgdp

ari_for_tsCV <- function(my_data, this_order, this_seasonal, this_mean = FALSE, 
                         this_lambda = 0, this_bias = TRUE, this_xreg = NULL, h = 1) {
 
   my_arima_obj <- Arima(my_data, 
                         order = this_order,
        seasonal = this_seasonal, 
        include.mean = this_mean,
        lambda = this_lambda, 
        biasadj = this_bias,
        xreg = this_xreg)
  
  my_fc_obj <- forecast(my_arima_obj, h = h)
  
  return(my_fc_obj)
}

e <- rgdp_ts %>% tsCV(forecastfunction = ari_for_tsCV, this_order = rgdp_order, this_seasonal=rgdp_seasonal, h = 4)
e

tic()

arima_list <- list()

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


# p q P Q freq d D  
arima_order_names <- c("p_r", "q_r", "P_r", "Q_r", "freq_r", "d_r", "D_r")
arima_list_m_order <- list()

# rgdp
foo <- arima_gdp[["arma"]]
names(foo) <- arima_order_names; foo


moo <- tibble(id_r = "rgdp", p_r = foo[1], q_r = foo[2], 
       P_r = foo[3], Q_r = foo[4], freq_r = foo[5], 
       d_r = foo[6], D_r = foo[7])

dmetra_and_r_order_rgdp <- cbind(moo, country_rgdp_dmetra)
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

arima_list_m <- list()

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

x_cpi <- ext_series_xts_quarterly$cpi
x_cpi <- x_cpi["1993/2017-12"]
  
arima_gdp_cpi <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                   seasonal = rgdp_seasonal, 
                   include.mean = rgdp_inc_mean,
                   lambda = 0, 
                   biasadj = TRUE,
                   xreg = lag.xts(x_cpi, k=0))

e <- rgdp_ts %>% tsCV(forecastfunction = ari_for_tsCV, this_order = rgdp_order, this_seasonal=rgdp_seasonal, 
                      this_xreg = x_cpi, h = 4)
e

arima_gdp_cpi <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                       seasonal = rgdp_seasonal, 
                       include.mean = rgdp_inc_mean,
                       lambda = 0, 
                       biasadj = TRUE,
                       xreg = lag.xts(x_cpi, k=1))

arima_gdp_cpi <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                       seasonal = rgdp_seasonal, 
                       include.mean = rgdp_inc_mean,
                       lambda = 0, 
                       biasadj = TRUE,
                       xreg = lag.xts(x_cpi, k=2))

arima_gdp_cpicore <- Arima(window(rgdp_ts, start = c(1993, 1)), order = rgdp_order,
                       seasonal = rgdp_seasonal, 
                       include.mean = rgdp_inc_mean,
                       lambda = 0, 
                       biasadj = TRUE,
                       xreg = coredata(x_cpi))

summary(arima_gdp_cpicore )
summary(arima_gdp_cpi )



