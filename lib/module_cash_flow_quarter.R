
moduleCashFlowQuarterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Filters",
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(3, offset = 3, uiOutput(ns("select_category_type"))),
        column(3, offset = 0, uiOutput(ns("select_category"))),
        column(6, offset = 3, uiOutput(ns("select_quarter")))
      )
    ), 
    fluidRow(
      column(12, offset = 0, align = 'center',
        h3("Cumulative Cash Flow by Quarter"),
        showOutput(ns("plot_cum_by_quarter"), "nvd3")
      )
    ),
    # hr(),
    # fluidRow(    
    #   column(12, offset = 0, align = 'center',
    #     h3("Monthly Cash Flow by Year"),
    #     showOutput(ns("plot_monthly_by_year"), "nvd3")
    #   )
    # ),
    # hr(),
    # fluidRow(
    #   column(12, align = 'center',
    #     h3("Cumulative Cash Flow by Month")
    #   )
    # ),
    # fluidRow(
    #   column(3, offset = 3, uiOutput(ns("select_year"))),
    #   column(3, offset = 0, uiOutput(ns("select_month")))
    # ),
    # fluidRow(
    #   column(12, align = 'center',
    #     showOutput(ns("plot_cumm_by_month"), "nvd3")
    #   )
    # ),
    fluidRow(
      column(12, align = 'center',
        h3("Table of Transactions"),
        dataTableOutput(ns('table_transactions'))
      )
    )
  )
}

moduleCashFlowQuarter <- function(input, output, session) {
  
  ns <- session$ns
  
  ##| --------------------------------------------
  ##| Crunch Data Functions
  ##| --------------------------------------------
  
  filterCategoryType <- reactive({
    
    validate(
      need(!is.null(input$select_category_type), "Loading Data ...")
    )
    
    df_trans <- getData()
    
    ##| Filter Category Type
    
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
  
    list_include_income <- c('Income', 
                             'Bonus', 
                             'Interest Income', 
                             'Paycheck', 
                             'Reimbursement', 
                             'Rental Income', 
                             'Returned Purchase', 
                             'Credit Card Cashback', 
                             'Gift Received', 
                             'Side Job')
    
    list_include_giving <- c('Charity',
                             'Gift',
                             'Church Tithe',
                             'Missions Support')
    
    list_include_spending <- setdiff(
      unique(df_trans$Category), 
      c(list_exclude_all, list_include_income, list_include_giving)
      )
    
    list_include_all <- setdiff(
      unique(df_trans$Category), 
      c(list_exclude_all)
      )
    
    if (input$select_category_type == 'All') {
      
      list_include <- list_include_all
    
    } else if (input$select_category_type == 'Income') {
      
      list_include <- list_include_income
    
    } else if (input$select_category_type == 'Spending') {
      
      list_include <- list_include_spending
    
    } else if (input$select_category_type == 'Giving') {
      
      list_include <- list_include_giving
    
    }
    
    df_trans2 <- df_trans %>%
      filter(Category %in% list_include)
      
    return(df_trans2)
      
  })
  
  filterCategory <- reactive({
    
    validate(
      need(!is.null(input$select_category), "Loading Data ...")
    )
    
    df_trans <- filterCategoryType()
    
    if (input$select_category == 'All Categories') {
      
      df_trans2 <- df_trans
      
    } else {
      
      df_trans2 <- df_trans %>%
        filter(Category %in% input$select_category)
    }
    
    return(df_trans2)
  })
  
  filterQuarter <- reactive({
    
    validate(
      need(!is.null(input$select_quarter), "Loading Data ...")
    )
    
    df_dates <- getDates()
      
    df_dates2 <- df_dates %>%
      filter(year_quarter %in% input$select_quarter)
    
    return(df_dates2)
  })
    
  crunchData <- reactive({
    
    df_trans <- filterCategory()
    df_dates <- filterQuarter()
    
    df_trans2 <- df_trans %>%
      mutate(
        date = as.Date(Date, format = "%m/%d/%Y")
        ) %>%
      right_join(df_dates, by="date") %>%
      arrange(year, yday) %>%
      mutate(
        Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount),
        Amount = ifelse(is.na(Amount), 0, Amount)
      ) %>%
      group_by(year_quarter) %>%
      mutate(
        cum_quarter_cash_flow = cumsum(Amount),
        cum_quarter_cash_flow_str = paste0('$', prettyNum(round(cum_quarter_cash_flow), big.mark=",",scientific=F))
      ) %>%
      data.frame()
      
    return(df_trans2)
  })
  
  # filterSavings <- reactive({
  #     
  #   df_trans2 <- crunchData()
  #   
  #   df <- df_trans2
  #   df$show <- TRUE
  #   
  #   ## Filter dots 
  #   if (is.null(input$year) == F & is.null(input$month) == F ) {
  #     
  #     df$show[!(df$year %in% input$year & df$month %in% input$month)] <- FALSE
  #   
  #   } else if (is.null(input$year) == F) {
  #   
  #     df$show[!(df$year %in% input$year)] <- FALSE
  #   
  #   } else if (is.null(input$month) == F) {
  #   
  #     df$show[!(df$month %in% input$month)] <- FALSE
  #   
  #   } else {
  #     
  #     df$show[!(df$year == max(df$year) & df$month == max(df$month))] <- FALSE
  #   
  #   }
  #   
  #   df <- as.data.frame(df) %>%
  #     filter(show == TRUE)
  #   
  #   return(df)
  # 
  # })
  
  ##| --------------------------------------------
  ##| Chart and Table Functions
  ##| --------------------------------------------
  
  createPlotCumByQuarter <- reactive({
    
    df_trans2 <- crunchData()
    
    ## Select on the values I need in chart so it loads faster
    df_trans3 <- df_trans2 %>%
      select(year_quarter, qday, date_str, cum_quarter_cash_flow, cum_quarter_cash_flow_str)
    
    ## Keep only the last value for each day so chart is smoother and loads faster
    df_plot <- df_trans3[!duplicated(df_trans3[c("qday","year_quarter")], fromLast = T),]
    
    n <- nPlot(cum_quarter_cash_flow ~ qday, data = df_plot, group = "year_quarter", type = 'lineChart')
    n$xAxis(axisLabel = 'Day of Quarter')
    n$chart(tooltipContent = "#!
          function(key, x, y, d){ 
            return '<h3>' + d.point.year + '</h3>' +
            '<p><b>' + d.point.date_str + '</b></p>' + 
            '<p><b>'  + d.point.cum_year_cash_flow_str + '</b></p>'
          }
          !#")
    
    return(n)  
  })
  
  # createPlotTotalSavingsMonth <- reactive({
  #  
  #   df_trans2 <- crunchData()
  #   
  #   df_trans3 <- df_trans2 %>%
  #     group_by(year, month) %>%
  #     summarize(
  #       sum_cash_flow = sum(Amount)
  #     )
  #   
  #   grid <- expand.grid(year = unique(df_trans3$year), month = as.numeric(seq(1:12)))
  #   df <- merge(grid, df_trans3, by=c("year","month"), all.x = T)
  #   df[is.na(df)] <- 0
  #   df <- arrange(df, year, month)
  #   
  #   n <- nPlot(sum_cash_flow ~ month, data = df, group = "year", type = 'multiBarChart')
  #   
  #   unique_year <- unique(df$year)
  #   exclude_year <- unique_year[unique_year != tail(unique_year,1)]
  #   n$set(disabled = exclude_year)
  #   
  #   return(n)
  # })
  # 
  # createPlotCumSavingsMonth <- reactive({
  #  
  #   df_trans2 <- filterSavings()
  #   
  #   df_trans3 <- df_trans2 %>%
  #     # filter(date <= today()) %>%
  #     mutate(
  #       year_mon = paste0(year,'-',month)
  #     ) %>%
  #     select(year_mon, mday, date_str, cum_month_cash_flow, cum_month_cash_flow_str) %>%
  #     arrange(date_str)
  #     
  #   df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
  #   
  #   n <- nPlot(cum_month_cash_flow ~ mday, data = df, group = "year_mon", type = 'lineChart')
  #   n$xAxis(axisLabel = 'Day of Month')
  #   n$chart(tooltipContent = "#!
  #         function(key, x, y, d){ 
  #           return '<h3>' + d.point.year_mon + '</h3>' +
  #           '<p><b>' + d.point.date_str + '</b></p>' + 
  #           '<p><b>'  + d.point.cum_month_cash_flow_str + '</b></p>'
  #         }
  #         !#")
  #   return(n)
  # 
  # })
  
  createDataTable <- reactive({
    
    df3 <- crunchData()
    
    df_table <- df3 %>%
      select(date , year, month, year_quarter, qday, Category, Description, Amount, Notes) %>%
      filter(!(is.na(Description))) %>%
      arrange(Amount)
    
    dt <- datatable(
      df_table,
      filter = "top",
      width = "900px",
      style = "bootstrap",
      fillContainer = FALSE,
      options = list(
        autoWidth = FALSE,
        escape = FALSE, 
        scrollX = TRUE
      )
    )
    
    return(dt)
  })
  
  ##| --------------------------------------------
  ##| Render Input Functions
  ##| --------------------------------------------
  
  output$select_category_type <- renderUI({
    
    choices_list <- c('All', 'Income', 'Spending', 'Giving')
    selected_default <- 'All'
    
    container <- selectizeInput(
      inputId = ns("select_category_type"),
      label = h4("Category Type:"),
      choices = choices_list,
      selected = selected_default,
      multiple = FALSE
    )
    
    return(container)
  })
  
  output$select_category <- renderUI({
    
    validate(
      need(!is.null(input$select_category_type), "Loading Data...")
    )
    
    df_trans <- filterCategoryType()
    
    df_category <- df_trans %>% 
      arrange(Category) %>% 
      distinct(Category)
    
    list_choices <- c('All Categories', df_category)
    selected_default <- 'All Categories'
    
    container <- selectizeInput(
      inputId = ns('select_category'),
      label = h4("Category:"),
      choices = list_choices,
      selected = selected_default,
      multiple = FALSE
    )
    
    return(container)
  }) 
  
  output$select_quarter <- renderUI({
    
    df_dates <- getDates()
    
    choices_list <- sort(unique(df_dates$year_quarter))
    selected_default <- choices_list[(length(choices_list)-5):length(choices_list)]
    
    container <- selectizeInput(
      inputId = ns("select_quarter"),
      label = h4("Quarter:"),
      choices = choices_list,
      selected = selected_default,
      multiple = TRUE
    )
    
    return(container)
  })
  
  # output$select_month <- renderUI({
  #   
  #   df_dates <- getDates()
  #   
  #   choices_list <- sort(unique(df_dates$month))
  #   selected_default <- seq(month(today()), month(today())-2)
  #   
  #   container <- selectizeInput(
  #     inputId = ns("month"),
  #     label = h4("Month:"),
  #     choices = choices_list,
  #     selected = selected_default,
  #     multiple = TRUE
  #   )
  #   
  #   return(container)
  # })
  
  ##| --------------------------------------------
  ##| Render Output Functions
  ##| --------------------------------------------
  
  output$plot_cum_by_quarter <- renderChart2({    
    
    n <- createPlotCumByQuarter()  
    return(n)
  })
  
  # output$plot_monthly_by_year <- renderChart2({    
  #   
  #   n <- createPlotTotalSavingsMonth()  
  #   return(n)
  # }) 
  # 
  # output$plot_cumm_by_month <- renderChart2({    
  #   
  #   n <- createPlotCumSavingsMonth()  
  #   return(n)
  # })  
  
  output$table_transactions <- renderDataTable({
  
    createDataTable()
  })
}
