
moduleCashFlowUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, offset = 0, align = 'center',
        h3("Cumulative Cash Flow by Year"),
        showOutput(ns("plot_cumm_by_year"), "nvd3")
      )
    ),
    hr(),
    fluidRow(    
      column(12, offset = 0, align = 'center',
        h3("Monthly Cash Flow by Year"),
        showOutput(ns("plot_monthly_by_year"), "nvd3")
      )
    ),
    hr(),
    fluidRow(
      column(12, align = 'center',
        h3("Cumulative Cash Flow by Month")
      )
    ),
    fluidRow(
      column(3, offset = 3, uiOutput(ns("select_year"))),
      column(3, offset = 0, uiOutput(ns("select_month")))
    ),
    fluidRow(
      column(12, align = 'center',
        showOutput(ns("plot_cumm_by_month"), "nvd3")
      )
    ),
    fluidRow(
      column(12, align = 'center',
        h3("Table of Transactions"),
        dataTableOutput(ns('table_transactions'))
      )      
    )
  )
}

moduleCashFlow <- function(input, output, session) {
  
  ns <- session$ns

  getData <- function() {
    df_trans <- read.csv('data/transactions.csv', stringsAsFactors = F)
  }
  
  ##| --------------------------------------------
  ##| Crunch Data Functions
  ##| --------------------------------------------
  
  crunchData <- function(df) {
    
    list_exclude_all <- c('Transfer',
                          'Credit Card Payment',
                          'Hide from Budgets & Trends',
                          'Transfer for Cash Spending',
                          'Cash & ATM',
                          'Withdrawal',
                          'Sell',
                          'Buy',
                          'Deposit',
                          'Federal Tax', 
                          'State Tax')
    
    unique_years <- 
      df_trans$Date %>%
      as.Date(., format = "%m/%d/%Y") %>%
      year(.) %>%
      unique(.)
    
    df_dates <- expand.grid(year = unique_years, yday = seq(1:366))
    
    df_trans2 <- df_trans %>%
      filter(!(Category %in% list_exclude_all)) %>%
      mutate(
        Date = as.Date(Date, format = "%m/%d/%Y"),
        year = year(Date),
        yday = yday(Date)
        ) %>%
      right_join(df_dates, by=c("year","yday")) %>%
      arrange(year, yday) %>%
      mutate(
        Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount),
        Amount = ifelse(is.na(Amount), 0, Amount),
        date = as.Date(paste0(year,'-',yday), format = "%Y-%j"),
        month = month(date),
        mday = mday(date),
        date_str = as.character(date)
      ) %>%
      group_by(year) %>%
      mutate(
        cum_year_cash_flow = cumsum(Amount),
        cum_year_cash_flow_str = paste0('$', prettyNum(round(cum_year_cash_flow), big.mark=",",scientific=F))
      ) %>%
      group_by(year, month) %>%
      mutate(
        cum_month_cash_flow = cumsum(Amount),
        cum_month_cash_flow_str = paste0('$', prettyNum(round(cum_month_cash_flow), big.mark=",",scientific=F))
      ) %>%
      data.frame()
      
    return(df_trans2)
  }
  
  filterSavings <- function(df_trans2) {
      
    df <- df_trans2
    df$show <- TRUE
    
    ## Filter dots 
    if (is.null(input$year) == F & is.null(input$month) == F ) {
      
      df$show[!(df$year %in% input$year & df$month %in% input$month)] <- FALSE
    
    } else if (is.null(input$year) == F) {
    
      df$show[!(df$year %in% input$year)] <- FALSE
    
    } else if (is.null(input$month) == F) {
    
      df$show[!(df$month %in% input$month)] <- FALSE
    
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
  
  createPlotCummByYear <- function(df_trans2) {
    
    ## Select on the values I need in chart so it loads faster
    df_trans3 <- df_trans2 %>%
      select(year, yday, date_str, cum_year_cash_flow, cum_year_cash_flow_str)
      
    ## Keep only the last value for each day so chart is smoother and loads faster
    df_plot <- df_trans3[!duplicated(df_trans3[c("yday","year")], fromLast = T),]
    
    n <- nPlot(cum_year_cash_flow ~ yday, data = df_plot, group = "year", type = 'lineChart')
    n$xAxis(axisLabel = 'Day of Year')
    n$chart(tooltipContent = "#!
          function(key, x, y, d){ 
            return '<h3>' + d.point.year + '</h3>' +
            '<p><b>' + d.point.date_str + '</b></p>' + 
            '<p><b>'  + d.point.cum_year_cash_flow_str + '</b></p>'
          }
          !#")
    
    n$xAxis(tickValues = "#! 
      function (x) {    
        tickvalues = [1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335];
        return tickvalues;
      } !#")
    
    # n$xAxis(tickFormat = "#! 
    #   function (x) {
    #     tickformat = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan'];
    #     return tickformat[x-1];
    #   } !#")
    # n
  
    return(n)  
  }
  
  createPlotTotalSavingsMonth <- function(df_trans2) {
   
    df_trans3 <- df_trans2 %>%
      group_by(year, month) %>%
      summarize(
        savings = max(cum_month_cash_flow)
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
      select(year_mon, mday, date_str, cum_month_cash_flow, cum_month_cash_flow_str) %>%
      arrange(date_str)
      
    df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
    
    n <- nPlot(cum_month_cash_flow ~ mday, data = df, group = "year_mon", type = 'lineChart')
    n$xAxis(axisLabel = 'Day of Month')
    n$chart(tooltipContent = "#!
          function(key, x, y, d){ 
            return '<h3>' + d.point.year_mon + '</h3>' +
            '<p><b>' + d.point.date_str + '</b></p>' + 
            '<p><b>'  + d.point.cum_month_cash_flow_str + '</b></p>'
          }
          !#")
    return(n)
  }
  
  ##| --------------------------------------------
  ##| Render Input Functions
  ##| --------------------------------------------
  
  output$select_year <- renderUI({
    
    df_trans <- getData()
    df_trans2 <- crunchData(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    year_selected <- max(year_list)
    
    container <- selectizeInput(
      inputId = ns("year"),
      label = h4("Year:"),
      choices = year_list,
      multiple = TRUE,
      selected = year_selected
    )
    
    return(container)
  })
  
  output$select_month <- renderUI({
    
    df_trans <- getData()
    df_trans2 <- crunchData(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    month_selected <- seq(month(today()), month(today())-2)
    
    container <- selectizeInput(
      inputId = ns("month"),
      label = h4("Month:"),
      choices = month_list,
      multiple = TRUE,
      selected = month_selected
    )
    
    return(container)
  })
  
  ##| --------------------------------------------
  ##| Render Output Functions
  ##| --------------------------------------------
  
  output$plot_cumm_by_year <- renderChart2({    
    
    df_trans <- getData()
    df_trans2 <- crunchData(df_trans)
    n <- createPlotCummByYear(df_trans2)  
    return(n)
  })
  
  output$plot_monthly_by_year <- renderChart2({    
    
    df_trans <- getData()
    df_trans2 <- crunchData(df_trans)
    n <- createPlotTotalSavingsMonth(df_trans2)  
    return(n)
  }) 
  
  output$plot_cumm_by_month <- renderChart2({    
    
    df_trans <- getData()
    df_trans2 <- crunchData(df_trans)
    df3 <- filterSavings(df_trans2)
    n <- createPlotCumSavingsMonth(df3)  
    return(n)
  })  
  
  output$table_transactions <- renderDataTable({
  
    df_trans <- getData()
    df_trans2 <- crunchData(df_trans)
    df3 <- filterSavings(df_trans2)
  
    df_table <- df3 %>%
      select(date , year, month, Category, Description, Amount, Notes) %>%
      filter(!(is.na(Description))) %>%
      arrange(Amount)
    
    dt <- datatable(
      df_table,
      filter = "top",
      width = "900px",
      style = "bootstrap",
      fillContainer = FALSE,
      options = list(
        autoWidth = FALSE
      )
    )
  }, escape = FALSE, option=list(scrollX = TRUE))
}
