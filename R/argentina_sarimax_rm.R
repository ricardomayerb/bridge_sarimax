library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)

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


print(names(country_m)[1:4])

ncol(country_m)


my_variable <- data_m_ts[, "cpi"]

arima_list_m <- list()


tic()
for (i in seq_along(arima_names)) {
  
  my_variable <- data_m_ts[, arima_names[i]]
  fit <- auto.arima(my_variable)
  
  arima_list_m[[i]] <- fit
  
}
toc()

names(arima_list_m) <- arima_names

foo <- as.list(data_m_ts)

monthly_arima_tbl <- tibble(id = arima_names, data = as.list(data_m_ts),
                            data_na_rm = map(data, na.omit),
                            fit = arima_list_m, 
                            indiv_dates = map(data_na_rm, time),
                            max_dates = unlist(map(indiv_dates, max)),
                            final_date = 2020,
                            date_diff = final_date - max_dates,
                            months_diff = 12 * date_diff) 


fc_m <- map(arima_list_m, forecast, h = 7) 
fc_m <- map2(arima_list_m, h_list, forecast, h = .y) 

# names(fc_m) <- arima_names




fit <- auto.arima(my_varible)
plot(forecast(fit, h = 20))
summary(fit)
