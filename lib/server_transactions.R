##| --------------------------------------------
##| Crunch Data Functions
##| --------------------------------------------

crunchDataTransactions <- function(df_trans) {

  str(df_trans)
  head(df_trans)
  
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
                             'Federal Tax', 'State Tax'
    ))) %>%       
    filter(!(Category %in% c('Bonus', 'Interest Income', 
                             'Paycheck', 'Reimbursement', 
                             'Rental Income', 'Returned Purchase', 
                             'Credit Card Cashback', 'Gift Received', 
                             'Side Job'))) %>%
    filter(!(Category %in% c('Charity','Gift','Church Tithe','Missions Support')))
  
  grid <- expand.grid(year = unique(df_trans2$year), yday = seq(1:365))
  df <- merge(grid, df_trans2, by=c("year","yday"), all.x = T)
  head(df)
  
  df2 <- df_trans %>%
    group_by(Year, Category) %>%
    summarize(
      count = n(),
      sum_amount = sum(Amount)
    )
  
  
  return(df_trans2)
}

filterSpending <- function(df_trans2) {
    
  df <- df_trans2
  df$show <- TRUE
  
  ## Filter dots 
  if (is.null(input$spending_year) == F & is.null(input$spending_month) == F ) {
    
    df$show[!(df$year %in% input$spending_year & df$month %in% input$spending_month)] <- FALSE
  
  } else if (is.null(input$spending_year) == F) {
  
    df$show[!(df$year %in% input$spending_year)] <- FALSE
  
  } else if (is.null(input$spending_month) == F) {
  
    df$show[!(df$month %in% input$spending_month)] <- FALSE
  
  }
  
  df <- as.data.frame(df) %>%
    filter(show == TRUE)
  
  return(df)

}

##| --------------------------------------------
##| Plot Functions
##| --------------------------------------------

createPlotCumSpendingYear <- function(df_trans2) {

  df_trans3 <- select(df_trans2, cum_year_spending, cum_year_spending_str, yday, year, date_str)
  
  df <- df_trans3[!duplicated(df_trans3[c("yday","year")], fromLast = T),]

  n <- nPlot(cum_year_spending ~ yday, data = df, group = "year", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Year')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_year_spending_str + '</b></p>'
        }
        !#")
  
  return(n)
}

createPlotTotalSpendingMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    group_by(year, month) %>%
    summarize(
      spending = max(cum_month_spending)
    )

  grid <- expand.grid(year = unique(df_trans3$year), month = as.numeric(seq(1:12)))
  df <- merge(grid, df_trans3, by=c("year","month"), all.x = T)
  df[is.na(df)] <- 0
  df <- arrange(df, year, month)

  n <- nPlot(spending ~ month, data = df, group = "year", type = 'multiBarChart')

  unique_year <- unique(df$year)
  exclude_year <- unique_year[unique_year != tail(unique_year,1)]
  n$set(disabled = exclude_year)

  return(n)
}

createPlotCumSpendingMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    mutate(
      year_mon = paste0(year,'-',month)
    ) %>%
    select(year_mon, mday, date_str, cum_month_spending, cum_month_spending_str) %>%
    arrange(date_str)
    
  df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
  
  n <- nPlot(cum_month_spending ~ mday, data = df, group = "year_mon", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Month')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year_mon + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_month_spending_str + '</b></p>'
        }
        !#")
  
  return(n)
}

##| --------------------------------------------
##| Render UI Functions
##| --------------------------------------------

output$spending_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    year_selected <- max(year_list)
    
    selectizeInput(inputId = "spending_year",
                label = h4("Year:"),
                choices = year_list,
                multiple = TRUE,
                selected = year_selected)
  }
})

output$spending_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    month_selected <- seq(month(today()), month(today())-2)

    selectizeInput(inputId = "spending_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE,
                   selected = month_selected)
  }
  
})

##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$plot_cum_spending_year <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    n <- createPlotCumSpendingYear(df_trans2)  
  }
  
  return(n)
})  

output$plot_total_spending_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    
    n <- createPlotTotalSpendingMonth(df_trans2)  
  }
  
  return(n)
}) 

output$plot_cum_spending_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    df3 <- filterSpending(df_trans2)
    n <- createPlotCumSpendingMonth(df3)  
  }
  
  return(n)
})  

output$table_spending <- renderDataTable({

  if (is.null(input$file_transactions)) {

  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    df3 <- filterSpending(df_trans2)
    
    df_table <- df3 %>%
      select(date , Description, Amount, Category) %>%
      filter(!(is.na(Description))) %>%
      arrange(desc(Amount))

    
    colnames(df_table) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df_table)
  }
})
