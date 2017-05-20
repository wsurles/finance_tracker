
##| --------------------------------------------
##| Crunch Data Functions
##| --------------------------------------------

crunchDataIncome <- function(df_trans) {
  
  ## Filter
  df_trans2 <- df_trans %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      yday = yday(date),
      year = year(date)
    ) %>%
    filter(Category %in% c('Income', 'Bonus', 'Interest Income', 
                             'Paycheck', 'Reimbursement', 
                             'Rental Income', 'Returned Purchase', 
                             'Credit Card Cashback', 'Gift Received', 
                             'Side Job'))

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
      cum_year_income = cumsum(Amount),
      cum_year_income_str = paste0('$', prettyNum(round(cum_year_income), big.mark=",",scientific=F))
    ) %>%
    group_by(year, month) %>%
    mutate(
      cum_month_income = cumsum(Amount),
      cum_month_income_str = paste0('$', prettyNum(round(cum_month_income), big.mark=",",scientific=F))
    ) %>%
    filter(date <= today()) %>%
    arrange(date)

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
  #   filter(Category %in% c('Bonus', 'Interest Income', 
  #                            'Paycheck', 'Reimbursement', 
  #                            'Rental Income', 'Returned Purchase', 
  #                            'Credit Card Cashback', 'Gift Received', 
  #                            'Side Job')
  #   ) %>%
  #   arrange(date) %>%
  #   group_by(year) %>%
  #   mutate(
  #     cum_year_income = cumsum(Amount),
  #     cum_year_income_str = paste0('$', prettyNum(round(cum_year_income), big.mark=",",scientific=F))
  #   ) %>%
  #   group_by(year, month) %>%
  #   mutate(
  #     cum_month_income = cumsum(Amount),
  #     cum_month_income_str = paste0('$', prettyNum(round(cum_month_income), big.mark=",",scientific=F))
  #   )

  # df_trans2 <- as.data.frame(df_trans2)
  
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

##| --------------------------------------------
##| Plot Functions
##| --------------------------------------------

createPlotIncome <- function(df_trans2) {
  
  df_trans3 <- select(df_trans2, cum_year_income, cum_year_income_str, yday, year, date_str)
  
  df <- df_trans3[!duplicated(df_trans3[c("yday","year")], fromLast = T),]
  
  n <- nPlot(cum_year_income ~ yday, data = df, group = "year", type = 'lineChart')
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

createPlotTotalIncomeMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    group_by(year, month) %>%
    summarize(
      income = max(cum_month_income)
    )
  
  grid <- expand.grid(year = unique(df_trans3$year), month = as.numeric(seq(1:12)))
  df <- merge(grid, df_trans3, by=c("year","month"), all.x = T)
  df[is.na(df)] <- 0
  df <- arrange(df, year, month)
  
  n <- nPlot(income ~ month, data = df, group = "year", type = 'multiBarChart')
  
  unique_year <- unique(df$year)
  exclude_year <- unique_year[unique_year != tail(unique_year,1)]
  n$set(disabled = exclude_year)
  
  return(n)
}

createPlotCumIncomeMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    mutate(
      year_mon = paste0(year,'-',month)
    ) %>%
    select(year_mon, mday, date_str, cum_month_income, cum_month_income_str) %>%
    arrange(date_str)

  ## [Q] Why do I not have zeros here after expanding above???
  
    
  df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
  
  n <- nPlot(cum_month_income ~ mday, data = df, group = "year_mon", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Month')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year_mon + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_month_income_str + '</b></p>'
        }
        !#")
  return(n)
}

##| --------------------------------------------
##| Render UI Functions
##| --------------------------------------------

output$income_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    year_selected <- max(year_list)

    selectizeInput(inputId = "income_year",
                label = h4("Year:"),
                choices = year_list,
                multiple = TRUE,
                selected = year_selected)
  }
})


output$income_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    month_selected <- seq(month(today()), month(today())-2)

    selectizeInput(inputId = "income_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE,
                   selected = month_selected)
  }
  
})

##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$plot_income_yoy <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    n <- createPlotIncome(df_trans2)  
  }
  
  return(n)
})  

output$plot_total_income_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    n <- createPlotTotalIncomeMonth(df_trans2)  
  }
  
  return(n)
}) 

output$plot_cum_income_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    df3 <- filterIncome(df_trans2)
    n <- createPlotCumIncomeMonth(df3)  
  }
  
  return(n)
})  

output$table_income <- renderDataTable({

  if (is.null(input$file_transactions)) {

  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataIncome(df_trans)
    df3 <- filterIncome(df_trans2)
    
    df4 <- df3 %>%
      select(date , Description, Amount, Category) %>%
      filter(!(is.na(Description)))
    
    colnames(df4) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df4)
  }
})
