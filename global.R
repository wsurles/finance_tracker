
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
library(sunburstR)

source('lib/module_cash_flow_year.R', local = T)
source('lib/module_cash_flow_quarter.R', local = T)
source('lib/module_cash_flow_month.R', local = T)
source('lib/module_cash_flow_category.R', local = T)

Sys.setenv(TZ='GMT')


to_jsdate <- function(date_){
  ## Convert R dates to javascript dates for use in D3 charts
  d <- as.Date(date_)
  d <- as.POSIXct(format(d, tz="GMT"), origin="1970-01-01")
  d <- as.numeric(d)
  d <- d * 1000
}
