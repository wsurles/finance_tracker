library(rCharts)
library(lubridate)
library(dplyr)
Sys.setenv(TZ='GMT')

##| Convert R dates to javascript dates for use in D3 charts
to_jsdate <- function(date_){ 
  d <- as.Date(date_) 
  d <- as.POSIXct(format(d, tz="GMT"), origin="1970-01-01") 
  d <- as.numeric(d)
  d <- d * 1000 
}

