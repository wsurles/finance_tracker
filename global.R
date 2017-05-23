
##|-------------------------------------
##| Run the app locally
##|-------------------------------------

# library(shiny)
# runApp(launch.browser = T)

##|-------------------------------------
##| Libraries and Source Files
##|-------------------------------------

library(shiny)
library(shinydashboard)
library(rCharts)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)

source('lib/module_cash_flow_year.R', local = T)
source('lib/module_cash_flow_quarter.R', local = T)

Sys.setenv(TZ='GMT')

##| Convert R dates to javascript dates for use in D3 charts
to_jsdate <- function(date_){ 
  d <- as.Date(date_) 
  d <- as.POSIXct(format(d, tz="GMT"), origin="1970-01-01") 
  d <- as.numeric(d)
  d <- d * 1000 
}


##| --------------------------------------------
##| Get Data Functions
##| --------------------------------------------

getData <- reactive({
  df_trans <- read.csv('data/transactions.csv', stringsAsFactors = F)
})

getDates <- reactive({
  
  df_trans <- getData()
  
  unique_years <- 
    df_trans$Date %>%
    as.Date(., format = "%m/%d/%Y") %>%
    year(.) %>%
    unique(.)
  
  df_dates <- expand.grid(year = unique_years, yday = seq(1:366)) %>%
    mutate(
      date = as.Date(paste0(year,'-',yday), format = "%Y-%j"),
      date_str = as.character(date),
      month = month(date),
      mday = mday(date),
      quarter = quarter(date)
    ) %>%
    group_by(year, quarter) %>%
    mutate(
      qday = rank(yday),
      year_quarter = str_c(year, quarter, sep="-")
    ) %>%
    data.frame()
  
  head(df_dates)
  
  return(df_dates)
})