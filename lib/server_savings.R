##| --------------------------------------------
##| Crunch Data Functions
##| --------------------------------------------

crunchDataSavings <- function(df_trans) {
  
  ## Filter
  df_trans2 <- df_trans %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      yday = yday(date),
      year = year(date)
    ) %>%
    filter(!(Category %in% c('Transfer','Credit Card Payment',
                             'Hide from Budgets & Trends',
                             'Transfer for Cash Spending',
                             'Cash & ATM','Withdrawal',
                             'Sell','Buy','Deposit',
                             'Federal Tax', 'State Tax')))

  ## Expand Dates
  grid <- expand.grid(year = unique(df_trans2$year), yday = seq(1:365))
  df <- merge(grid, df_trans2, by=c("year","yday"), all.x = T)
  
  ## Crunch by year and month
  df_trans2 <- df %>%
    mutate(
      date = as.Date(paste0(year,'-',yday), format = "%Y-%j"),
      date_str = as.character(date),
      yday = yday(date),
      mday = mday(date),
      month = month(date),
      year = year(date),
      Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount),
      Amount = ifelse(is.na(Amount), 0, Amount)
    ) %>%
    group_by(year) %>%
    mutate(
      cum_year_savings = cumsum(Amount),
      cum_year_savings_str = paste0('$', prettyNum(round(cum_year_savings), big.mark=",",scientific=F))
    ) %>%
    group_by(year, month) %>%
    mutate(
      cum_month_savings = cumsum(Amount),
      cum_month_savings_str = paste0('$', prettyNum(round(cum_month_savings), big.mark=",",scientific=F))
    ) %>%
    arrange(date)
    # filter(date <= today())
  
  df_trans2 <- as.data.frame(df_trans2)
  
  # ## Expand Dates
  # df_trans2 <- df_trans %>%
  #   mutate(
  #     date = as.Date(Date, format = "%m/%d/%Y"),
  #     yday = yday(date),
  #     year = year(date)
  #   )
  
  # grid <- expand.grid(year = unique(df_trans2$year), yday = seq(1:365))
  # df <- merge(grid, df_trans2, by=c("year","yday"), all.x = T)
  
  # ## Crunch
  # df_trans2 <- df %>%
  #   mutate(
  #     date = as.Date(paste0(year,'-',yday), format = "%Y-%j"),
  #     date_str = as.character(date),
  #     yday = yday(date),
  #     mday = mday(date),
  #     month = month(date),
  #     year = year(date),
  #     Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount),
  #     Amount = ifelse(is.na(Amount), 0, Amount)
  #   ) %>%
  #   filter(year >= 2011) %>%
  #   filter(!(Category %in% c('Transfer','Credit Card Payment',
  #                            'Hide from Budgets & Trends',
  #                            'Transfer for Cash Spending',
  #                            'Cash & ATM','Withdrawal',
  #                            'Sell','Buy','Deposit',
  #                            'Federal Tax', 'State Tax'))
  #   ) %>%
  #   arrange(date) %>%
  #   group_by(year) %>%
  #   mutate(
  #     cum_year_savings = cumsum(Amount),
  #     cum_year_savings_str = paste0('$', prettyNum(round(cum_year_savings), big.mark=",",scientific=F))
  #   ) %>%
  #   group_by(year, month) %>%
  #   mutate(
  #     cum_month_savings = cumsum(Amount),
  #     cum_month_savings_str = paste0('$', prettyNum(round(cum_month_savings), big.mark=",",scientific=F))
  #   )

  # df_trans2 <- as.data.frame(df_trans2)
  
  return(df_trans2)
}

filterSavings <- function(df_trans2) {
    
  df <- df_trans2
  df$show <- TRUE
  
  ## Filter dots 
  if (is.null(input$savings_year) == F & is.null(input$savings_month) == F ) {
    
    df$show[!(df$year %in% input$savings_year & df$month %in% input$savings_month)] <- FALSE
  
  } else if (is.null(input$savings_year) == F) {
  
    df$show[!(df$year %in% input$savings_year)] <- FALSE
  
  } else if (is.null(input$savings_month) == F) {
  
    df$show[!(df$month %in% input$savings_month)] <- FALSE
  
  } else {
    
    df$show[!(df$year == max(df$year) & df$month == max(df$month))] <- FALSE
  
  }
  
  df <- as.data.frame(df) %>%
    filter(show == TRUE)
  
  return(df)

}

##| --------------------------------------------
##| Plot Functions
##| --------------------------------------------

createPlotCumSavingsYear <- function(df_trans2) {
  
  ## Select on the values I need in chart so it loads faster
  df_trans3 <- df_trans2 %>%
    filter(date <= today()) %>%
    select(year, yday, date_str, cum_year_savings, cum_year_savings_str)
    
  ## Keep only the last value for each day so chart is smoother and loads faster
  df <- df_trans3[!duplicated(df_trans3[c("yday","year")], fromLast = T),]
  
  n <- nPlot(cum_year_savings ~ yday, data = df, group = "year", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Year')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_year_savings_str + '</b></p>'
        }
        !#")
  n$xAxis(tickValues = "#! function (x) {    
    tickvalues = [1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335];
    return tickvalues;
  } !#")
  
  # n$xAxis(tickFormat = "#! function (x) {
  # tickformat = ['0-1000', '1000-1500', '1500-1700', '1700-1820', '1820-1913', 
  # '1913-1950', '1950-1970', '1970-1990', '1990-2012', '2012-2030', '2030-2050', 
  # '2050-2070', '2070-2100'];
  # return tickformat[x-1];
  # } !#")

  return(n)  
}

createPlotTotalSavingsMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    group_by(year, month) %>%
    summarize(
      savings = max(cum_month_savings)
    )
  
  grid <- expand.grid(year = unique(df_trans3$year), month = as.numeric(seq(1:12)))
  df <- merge(grid, df_trans3, by=c("year","month"), all.x = T)
  df[is.na(df)] <- 0
  df <- arrange(df, year, month)
  
  n <- nPlot(savings ~ month, data = df, group = "year", type = 'multiBarChart')
  
  unique_year <- unique(df$year)
  exclude_year <- unique_year[unique_year != tail(unique_year,1)]
  n$set(disabled = exclude_year)
  
  return(n)
}

createPlotCumSavingsMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    # filter(date <= today()) %>%
    mutate(
      year_mon = paste0(year,'-',month)
    ) %>%
    select(year_mon, mday, date_str, cum_month_savings, cum_month_savings_str) %>%
    arrange(date_str)
    
  df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
  
  n <- nPlot(cum_month_savings ~ mday, data = df, group = "year_mon", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Month')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year_mon + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_month_savings_str + '</b></p>'
        }
        !#")
  return(n)
}

##| --------------------------------------------
##| Render UI Functions
##| --------------------------------------------

output$savings_year <- renderUI({
  
  df_trans <- getData()
  df_trans2 <- crunchDataSavings(df_trans)
  
  year_list <- sort(unique(df_trans2$year), decreasing = T)
  year_selected <- max(year_list)
  
  selectizeInput(inputId = "savings_year",
              label = h4("Year:"),
              choices = year_list,
              multiple = TRUE,
              selected = year_selected)
})

output$savings_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    month_selected <- seq(month(today()), month(today())-2)
    
    selectizeInput(inputId = "savings_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE,
                   selected = month_selected)
  }
  
})

##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$plot_savings_yoy <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    n <- createPlotCumSavingsYear(df_trans2)  
  }

  return(n)
})

output$plot_total_savings_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    n <- createPlotTotalSavingsMonth(df_trans2)  
  }
  
  return(n)
}) 

output$plot_cum_savings_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    df3 <- filterSavings(df_trans2)
    n <- createPlotCumSavingsMonth(df3)  
  }
  
  return(n)
})  

output$table_savings <- renderDataTable({

  if (is.null(input$file_transactions)) {
    
    df_table <- data.frame(matrix(NA,1,4))
    colnames(df_table) <- c('Date' , 'Description', 'Amount', 'Category')
  
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    df3 <- filterSavings(df_trans2)
  
  #   df4$link <- createButtonLinkSalesforce(df4$sf_id)
  #   https://wwws.mint.com/transaction.event?startDate=8/1/2014&endDate=8/31/2014&exclHidden=T
    df_table <- df3 %>%
      select(date , Description, Amount, Category) %>%
      filter(!(is.na(Description))) %>%
      arrange(Amount)
    
    colnames(df_table) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df_table)
  }
})
