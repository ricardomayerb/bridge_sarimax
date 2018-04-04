library(xts)
library(forecast)
library(tidyverse)
library(readxl)
library(timetk)
library(tictoc)
library(lubridate)

data_path <- "./data/excel_data/Argentina.xlsx"

arg_m <- read_excel(data_path, sheet = "monthly")


# playing with dates, to educate ourselves
final_date_dec <- 2019 + (11/12) 

print(date_decimal(final_date_dec))

year(date_decimal(final_date_dec))

month(date_decimal(final_date_dec))

zoo::as.yearmon(final_date_dec)


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

monthly_arima_tbl <- tibble(id = arima_names, 
                            data = as.list(data_m_ts),
                            data_na_rm = map(data, na.omit),
                            fit = arima_list_m, 
                            dec_dates = map(data_na_rm, time),
                            dec_dates_max = unlist(map(dec_dates, max)),
                            dates_max = date_decimal(dec_dates_max),
                            final_date = 2020,
                            date_diff = final_date - dec_dates_max,
                            months_diff = 12 * date_diff
)

h_vec <- monthly_arima_tbl[["months_diff"]]

h_list <- as.list(monthly_arima_tbl$months_diff)

bad_arima <- arima_list_m[[16]]
bad_fc <- forecast(bad_arima, h = 23)
plot(bad_fc)

bfit <- auto.arima(data_m_ts[,16])
summary(bfit)
summary(bad_arima)

plot(forecast(bfit, h = 20))

for (i in seq_along(arima_names)) {
  print(paste("i =", i, ",", arima_names[i]))
  this_arima <- arima_list_m[[i]]
  this_h <- h_vec[i]
  print(this_h)
  this_fc <- forecast(this_arima, h = 25)
  
}

fc_m_h <- map2(arima_list_m, h_vec, forecast) 

fc_m <- map(arima_list_m, forecast, h = 7) 

monthly_fc_tbl <- monthly_arima_tbl %>% 
  mutate(fc = map2(.x = fit, .y = months_diff, 
                              ~ forecast(fit, h = months_diff)
                   )
  )


# fc_m <- map2(arima_list_m, h_list, forecast, h = .y) 

# names(fc_m) <- arima_names



