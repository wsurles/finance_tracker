
##| --------------------------------------------
##| Crunch Data Functions
##| --------------------------------------------

##|---------
##| Savings
##|---------

crunchDataSavings <- function(df_trans) {
  
  ## Expand Dates
  df_trans2 <- df_trans %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      yday = yday(date),
      year = year(date)
    )
  
  grid <- expand.grid(year = unique(df_trans2$year), yday = seq(1:365))
  df <- merge(grid, df_trans2, by=c("year","yday"), all.x = T)
  
  ## Crunch
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
    ) %.%
    filter(year >= 2011) %>%
    filter(!(Category %in% c('Transfer','Credit Card Payment',
                             'Hide from Budgets & Trends',
                             'Transfer for Cash Spending',
                             'Cash & ATM','Withdrawal',
                             'Sell','Buy','Deposit',
                             'Federal Tax', 'State Tax'))
    ) %>%
    arrange(date) %>%
    group_by(year) %.%
    mutate(
      cum_year_savings = cumsum(Amount),
      cum_year_savings_str = paste0('$', prettyNum(round(cum_year_savings), big.mark=",",scientific=F))
    ) %>%
    group_by(year, month) %.%
    mutate(
      cum_month_savings = cumsum(Amount)
    )

  df_trans2 <- as.data.frame(df_trans2)
  
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
  
  }
  
  df <- as.data.frame(df) %>%
    filter(show == TRUE)
  
  return(df)

}


##|-------------------
##| Income
##|-------------------

crunchDataIncome <- function(df_trans) {
  
  df_trans2 <- df_trans %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      date_str = as.character(date),
      yday = yday(date),
      mday = mday(date),
      month = month(date),
      year = year(date),
      year_mon = paste(year,month,sep = "-"),
      Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount)
    ) %.%
    filter(year >= 2011) %>%
    filter(Category %in% c('Bonus', 'Interest Income', 
                             'Paycheck', 'Reimbursement', 
                             'Rental Income', 'Returned Purchase', 
                             'Credit Card Cashback', 'Gift Received', 
                             'Side Job')
    ) %>%
    arrange(date) %>%
    group_by(year) %.%
    mutate(
      cum_year_income = cumsum(Amount),
      cum_year_income_str = paste0('$', prettyNum(round(cum_year_income), big.mark=",",scientific=F))
    )

  return(df_trans2)
}

filterIncome <- function(df_trans2) {
    
  df <- df_trans2
  df$show <- TRUE
  
  ## Filter dots 
  if (is.null(input$income_year) == F & is.null(input$income_month) == F ) {
    
    df$show[!(df$year %in% input$income_year & df$month %in% input$income_month)] <- FALSE
  
  } else if (is.null(input$income_year) == F) {
  
    df$show[!(df$year %in% input$income_year)] <- FALSE
  
  } else if (is.null(input$income_month) == F) {
  
    df$show[!(df$month %in% input$income_month)] <- FALSE
  
  }
  
  df <- as.data.frame(df) %>%
    filter(show == TRUE) %>%
    arrange(desc(date))
  
  return(df)
                
}

##|--------
##| Giving
##|--------

crunchDataGiving <- function(df_trans) {

df_trans2 <- df_trans %>%
  mutate(
    date = as.Date(Date, format = "%m/%d/%Y"),
    date_str = as.character(date),
    yday = yday(date),
    mday = mday(date),
    month = month(date),
    year = year(date),
    year_mon = paste(year,month,sep = "-")
  ) %>%
  filter(Transaction.Type == 'debit') %>%
  filter(year >= 2011) %>%
  filter(Category %in% c('Charity','Gift','Church Tithe','Missions Support')) %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(
    cum_year_giving = cumsum(Amount),
    cum_year_giving_str = paste0('$', prettyNum(round(cum_year_giving), big.mark=",",scientific=F))
  )

  return(df_trans2)
}

filterGiving <- function(df_trans2) {
    
  df <- df_trans2
  df$show <- TRUE
  
  ## Filter dots 
  if (is.null(input$giving_year) == F & is.null(input$giving_month) == F ) {
    
    df$show[!(df$year %in% input$giving_year & df$month %in% input$giving_month)] <- FALSE
  
  } else if (is.null(input$giving_year) == F) {
  
    df$show[!(df$year %in% input$giving_year)] <- FALSE
  
  } else if (is.null(input$giving_month) == F) {
  
    df$show[!(df$month %in% input$giving_month)] <- FALSE
  
  }
  
  df <- as.data.frame(df) %>%
    filter(show == TRUE) %>%
    arrange(desc(date))
  
  return(df)
                
}


##|----------
##| Spending
##|----------

crunchDataSpending <- function(df_trans) {

  df_trans2 <- df_trans %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      date_str = as.character(date),
      yday = yday(date),
      mday = mday(date),
      month = month(date),
      year = year(date),
      year_mon = paste(year,month,sep = "-"),
      Amount = ifelse(Transaction.Type == 'credit', Amount * -1, Amount)
    ) %.%
    filter(year >= 2011) %>%
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
    filter(!(Category %in% c('Charity','Gift','Church Tithe','Missions Support'))) %>%
    arrange(date) %>%
    group_by(year) %.%
    mutate(
      cum_year_spending = cumsum(Amount),
      cum_year_spending_str = paste0('$', prettyNum(round(cum_year_spending), big.mark=",",scientific=F))
    ) %>%
    group_by(year, month) %.%
    mutate(
      cum_month_spending = cumsum(Amount)
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

##|----------
##| Savings
##|----------
createPlotSavings <- function(df_trans2) {
  
  ## Select on the values I need in chart so it loads faster
  df_trans3 <- select(df_trans2, year, yday, date_str, cum_year_savings, cum_year_savings_str)
  
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

  return(n)  
}

createPlotTotalSavingsMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    group_by(year, month) %.%
    summarize(
      savings = max(cum_month_savings)
    )
  
#   str(df_trans3)
#   str(df)
  
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
    mutate(
      year_mon = paste0(year,'-',month)
    ) %>%
    select(cum_month_savings, mday, year_mon)
    
  df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
  
  n <- nPlot(cum_month_savings ~ mday, data = df, group = "year_mon", type = 'lineChart')
  return(n)
}

##|----------
##| Income
##|----------

createPlotIncome <- function(df_trans2) {
  
  df_trans3 <- select(df_trans2, cum_year_income, cum_year_income_str, yday, year, date_str)
  
  n <- nPlot(cum_year_income ~ yday, data = df_trans3, group = "year", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Year')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_year_income_str + '</b></p>'
        }
        !#")

  return(n)
}

##|----------
##| Spending
##|----------

createPlotCumSpendingYear <- function(df_trans2) {

  df_trans3 <- select(df_trans2, cum_year_spending, cum_year_spending_str, yday, year, date_str)
  
  n <- nPlot(cum_year_spending ~ yday, data = df_trans3, group = "year", type = 'lineChart')
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
    group_by(year, month) %.%
    summarize(
      spending = max(cum_month_spending)
    )

  n <- nPlot(spending ~ month, data = df_trans3, group = "year", type = 'multiBarChart')

  unique_year <- unique(df_trans3$year)
  exclude_year <- unique_year[unique_year != tail(unique_year,1)]
  n$set(disabled = exclude_year)

  return(n)
}

createPlotCumSpendingMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    select(cum_month_spending, mday, year)
    
  n <- nPlot(cum_month_spending ~ mday, data = df_trans3, group = "year", type = 'lineChart')
  return(n)
}

##|----------
##| Giving
##|----------

createPlotGiving <- function(df_trans2) {

  df_trans3 <- select(df_trans2, cum_year_giving, cum_year_giving_str, yday, year, date_str)
  
  n <- nPlot(cum_year_giving ~ yday, data = df_trans3, group = "year", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Year')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_year_giving_str + '</b></p>'
        }
        !#")

  return(n)
}


##| --------------------------------------------
##| Render UI Functions
##| --------------------------------------------

##| ---------
##| Savings
##| ---------

output$savings_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    year_selected <- year_list
    
    selectizeInput(inputId = "savings_year",
                label = h4("Year:"),
                choices = year_list,
                multiple = TRUE,
                selected = year_selected)
  }
})

output$savings_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    month_selected <- month(today())
    
    selectizeInput(inputId = "savings_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE,
                   selected = month_selected)
  }
  
})

##| ---------
##| Giving
##| ---------

output$giving_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    
    selectizeInput(inputId = "giving_year",
                label = h4("Year:"),
                choices = year_list,
                multiple = TRUE)
  }
})


output$giving_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    
    selectizeInput(inputId = "giving_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE)
  }
  
})

##| ---------
##| Income
##| ---------

output$income_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    
    selectizeInput(inputId = "income_year",
                label = h4("Year:"),
                choices = year_list,
                multiple = TRUE)
  }
})


output$income_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    
    selectizeInput(inputId = "income_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE)
  }
  
})

##| ---------
##| Spending
##| ---------

output$spending_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSpending(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    year_selected <- year_list
    
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
    month_selected <- month(today())

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

##|---------
##| Savings
##|---------

output$plot_savings_yoy <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataSavings(df_trans)
    n <- createPlotSavings(df_trans2)  
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
      arrange(Amount)
    
    colnames(df_table) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df_table)
  }
})

##|---------
##| Income
##|---------

output$plot_income <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    n <- createPlotIncome(df_trans2)  
  }
  
  return(n)
})  

output$table_income <- renderDataTable({

  if (is.null(input$file_transactions)) {

  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    df3 <- filterIncome(df_trans2)
    
    df4 <- select(df3, date , Description, Amount, Category)
    colnames(df4) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df4)
  }
})

##|---------
##| Giving
##|---------

output$plot_giving <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    n <- createPlotGiving(df_trans2)  
  }
  
  return(n)  
}) 


output$table_giving <- renderDataTable({

  if (is.null(input$file_transactions)) {

  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    df3 <- filterGiving(df_trans2)
    
    df4 <- select(df3, date , Description, Amount, Category)
    colnames(df4) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df4)
  }
})

##|---------
##| Spending
##|--------- 

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
    # df3 <- as.data.frame(df_trans2) %>%
    #   filter(month == input$spending_month) %>%
    #   arrange(desc(Amount))
    
    df_table <- df3 %>%
      select(date , Description, Amount, Category) %>%
      arrange(desc(Amount))
    
    colnames(df_table) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df_table)
  }
})
