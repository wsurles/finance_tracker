
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
library(DT)

# source('lib/ui_tabs.R', local = T)
# source('lib/server_input.R', local = T)
# source('lib/server_categories.R', local = T)
source('lib/module_cash_flow.R', local = T)

Sys.setenv(TZ='GMT')

##| Convert R dates to javascript dates for use in D3 charts
to_jsdate <- function(date_){ 
  d <- as.Date(date_) 
  d <- as.POSIXct(format(d, tz="GMT"), origin="1970-01-01") 
  d <- as.numeric(d)
  d <- d * 1000 
}

