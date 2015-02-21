
##| --------------------------------------------
##| Crunch Data Functions
##| --------------------------------------------

crunchDataNetWorth <- function(df_worth) {

  df_worth2 <- df_worth %>%
    mutate(
      date = createDate(Dates),
      month = month(date),
      year = year(date),
      Net = as.numeric(gsub('\\$|,','',Net))
    ) %>%
    filter(year >= 2011) %>%
    mutate(
      diff_net = c(Net[1],diff(Net))
      ) %>%
    group_by(year) %.%
    mutate(
      sum_net = cumsum(diff_net) 
    )
  
  return(df_worth2)
}

createDate <- function(date) {
  ##| Converting to a date from the full month name with %B
  ##| did not work on the shinyapps server. It would create NAs.
  ##| It worked locally but not once deployed.
  ##| This process which converts to a month number first does work
  
  l <- strsplit(date, " ")
  len <- length(date)
  
  df <- as.data.frame(matrix(unlist(l), nrow=len, byrow=T))
  
  df$month <- match(substring(df$V1,1,3), month.abb)
  df$date_str <- paste(df$V2, df$month, '01', sep = "-")
  df$date <- as.Date(df$date_str, format = "%Y-%m-%d")
  
  return(df$date)
  
}

##| --------------------------------------------
##| Plot Functions
##| --------------------------------------------

createPlotNetWorthTotal <- function(df_worth) {

  n1 <- nPlot(Net ~ month, data = df_worth, group = "year", type = 'lineChart')
  return(n1)
  
}

createPlotNetWorthGrowth <- function(df_worth) {

  n2 <- nPlot(sum_net ~ month, data = df_worth, group = "year", type = 'lineChart')
  return(n2)
  
}

##| --------------------------------------------
##| Render UI Functions
##| --------------------------------------------


##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$plot_net_worth_total <- renderChart2({    
  
  if (is.null(input$file_worth)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_worth <- getDataWorth()
    df_worth2 <- crunchDataNetWorth(df_worth)
    n <- createPlotNetWorthTotal(df_worth2)  
  }
  return(n)
})  

output$plot_net_worth_growth <- renderChart2({    
  
  if (is.null(input$file_worth)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_worth <- getDataWorth()
    df_worth <- crunchDataNetWorth(df_worth)
    n <- createPlotNetWorthGrowth(df_worth)  
  }
  return(n)
})  

output$table_net_worth <- renderDataTable({

  if (is.null(input$file_worth)) {
    
    df_table <- data.frame(matrix(NA,1,5))
    colnames(df_table) <- c('Date', 'Month', 'Net Worth', 'Net Growth Year', 'Net Growth Month')
    
  } else {
    df_worth <- getDataWorth()
    df_worth <- crunchDataNetWorth(df_worth)
    
    df_worth <- df_worth %>%
    mutate(
      Net = format(round(Net), big.mark=',', scientific=F, justify= c('right')),
      diff_net = format(round(diff_net), big.mark=',', scientific=F, justify=c('right')),
      sum_net = format(round(sum_net), big.mark=',', scientific=F)
      ) %>%
    arrange(desc(date))
    
    df_table <- df_worth[, c('date', 'month', 'Net', 'sum_net', 'diff_net')]
    colnames(df_table) <- c('Date', 'Month', 'Net Worth', 'Net Growth Year', 'Net Growth Month')

  }
  
  return(df_table)

}) 
