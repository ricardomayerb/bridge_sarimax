library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

data_path <- "./data/excel_data/Argentina.xlsx"

arg_m <- read_excel(data_path, sheet = "monthly")
country_m <- arg_m 
start_year <- min(country_m$year)
start_month <- 1
this_start  <- c(start_year, start_month)
arima_names <- names(country_m)[5:ncol(country_m)]
data_m_ts <- tk_ts(country_m, frequency = 12, 
                   start = this_start)
data_m_ts <- data_m_ts[ , arima_names]

country_dmetra <- read_excel(data_path, sheet = "optimal")
country_dmetra <- country_dmetra %>% 
  separate(data = ., col = 1, into = c("freq_type", "id"))

country_rgdp_dmetra <- country_dmetra %>% 
  filter(id == "rgdp") %>% select(- freq_type)
  
country_monthly_dmetra <- country_dmetra %>% 
  filter(id != "rgdp") %>% select(- freq_type)


rgdp_order <- c(country_rgdp_dmetra$P, country_rgdp_dmetra$D,
                country_rgdp_dmetra$Q)

rgdp_seasonal <- c(country_rgdp_dmetra$BP, country_rgdp_dmetra$BD, 
                   country_rgdp_dmetra$BQ)

rgdp_inc_mean <- ifelse(country_rgdp_dmetra$Mean == 1, TRUE, FALSE)

# rgdp_arima_dmetra <- Arima(y = log_rgdp, order = rgdp_order,
#                            seasonal = rgdp_seasonal, 
#                            include.mean = rgdp_inc_mean)

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

}
toc()

# p q P Q freq d D  
foo <- this_arima_lam[["arma"]]
names(foo) <- c("p", "q", "P", "Q", "freq", "d", "D")
foo 

# playing with dates, to educate ourselves
final_date_dec <- 2019 + (11/12) 

print(date_decimal(final_date_dec))

year(date_decimal(final_date_dec))

month(date_decimal(final_date_dec))

zoo::as.yearmon(final_date_dec)



arima_list_m <- list()

tic()
for (i in seq_along(arima_names)) {
  
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



