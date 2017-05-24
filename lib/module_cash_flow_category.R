
moduleCashFlowCategoryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Filters",
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(3, offset = 3, uiOutput(ns("select_category_type"))),
        column(3, offset = 0, uiOutput(ns("select_date_range")))
      )
    ), 
    fluidRow(
      column(8, offset = 2, align = 'center',
             uiOutput(ns("select_category"))
      )
    ),
    fluidRow(
      column(12, offset = 0, align = 'center',
             h3("Cumulative Cash Flow by Quarter"),
             showOutput(ns("plot_cum_by_category"), "nvd3")
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

moduleCashFlowCategory <- function(input, output, session) {
  
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
    
    df_trans2 <- df_trans %>%
      filter(Category %in% input$select_category)
    
    return(df_trans2)
  })
  
  filterDateRange <- reactive({
    
    validate(
      need(!is.null(input$select_date_range), "Loading Data ...")
    )
    
    df_dates <- getDates()
    
    df_dates2 <- df_dates %>%
      filter(
        date >= input$select_date_range[1],
        date <= input$select_date_range[2]
      )
    
    return(df_dates2)
  })
  
  crunchData <- reactive({
    
    df_trans <- filterCategory()
    df_dates <- filterDateRange()
    
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
      complete(Category, nesting(date, date_str), fill = list(Amount=0)) %>%
      group_by(Category) %>%
      mutate(
        cum_category_cash_flow = cumsum(Amount),
        cum_category_cash_flow_str = paste0('$', prettyNum(round(cum_category_cash_flow), big.mark=",",scientific=F))
      ) %>%
      data.frame()
    
    # head(df_trans2)
    # str(df_trans2)
    # df_trans2 %>%
    #   filter(Category == 'Groceries')
    
    return(df_trans2)
  })
  
  filterCumTotal <- reactive({
    
    df_trans2 <- crunchData()
    
    df_tmp <- df_trans2 %>%
      group_by(Category) %>%
      summarize(cum_total = min(cum_category_cash_flow)) %>%
      filter(cum_total <= -150)
    
    df_trans3 <- df_trans2 %>%
      filter(Category %in% df_tmp$Category)
    
    return(df_trans3)
    
  })
  
  ##| --------------------------------------------
  ##| Chart and Table Functions
  ##| --------------------------------------------
  
  createPlotCumByCategory <- reactive({
    
    # df_trans2 <- crunchData()
    df_trans3 <- filterCumTotal()
    
    ## Select on the values I need in chart so it loads faster
    df_trans4 <- df_trans3 %>%
      select(Category, date, date_str, cum_category_cash_flow, cum_category_cash_flow_str) %>%
      mutate(date_js = to_jsdate(date))
    
    ## Keep only the last value for each day so chart is smoother and loads faster
    df_plot <- df_trans4[!duplicated(df_trans4[c("date","Category")], fromLast = T),]
    
    n <- nPlot(cum_category_cash_flow ~ date_js, data = df_plot, group = "Category", type = 'lineChart')
    n$xAxis(axisLabel = 'Day of Quarter')
    n$xAxis(tickFormat="#! function(d) {return d3.time.format('%Y-%m-%d')(new Date( d ));} !#" )
    # n$chart(forceY = c(0))
    n$chart(tooltipContent = "#!
            function(key, x, y, d){ 
            return '<h3>' + d.point.Category + '</h3>' +
            '<p><b>' + d.point.date_str + '</b></p>' + 
            '<p><b>'  + d.point.cum_category_cash_flow_str + '</b></p>'
            }
            !#")
    
    return(n)  
  })
  
  createDataTable <- reactive({
    
    df3 <- crunchData()
    
    df_table <- df3 %>%
      select(date, year, month, year_quarter, qday, Category, Description, Amount, Notes) %>%
      mutate(
        date = as.character(date),
        year = as.character(year),
        month = as.character(month),
        year_quarter = as.character(year_quarter),
        yday = as.character(qday)
      ) %>%
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
    
    list_choices <- df_category$Category
    selected_default <- list_choices
    
    container <- selectizeInput(
      inputId = ns('select_category'),
      label = h4("Category:"),
      choices = list_choices,
      selected = selected_default,
      multiple = TRUE
    )
    
    return(container)
  }) 
  
  output$select_date_range <- renderUI({
    
    df_dates <- getDates()
    df_trans <- getData()
    
    selected_date_start <- as.Date(str_c(year(Sys.Date()), "-01-01"))
    selected_date_end <- max(as.Date(df_trans$Date, format = "%m/%d/%Y"))
    
    date_min <- min(as.Date(df_trans$Date, format = "%m/%d/%Y"))
    date_max <- max(as.Date(df_trans$Date, format = "%m/%d/%Y"))
    
    container <- dateRangeInput(
      inputId = ns("select_date_range"), 
      label = h4("Date Range"),
      start  = selected_date_start,
      end    = selected_date_end,
      min    = date_min,
      max    = date_max,
      format = "yyyy-mm-dd"
    )
    
    return(container)
  })
  
  ##| --------------------------------------------
  ##| Render Output Functions
  ##| --------------------------------------------
  
  output$plot_cum_by_category <- renderChart2({    
    
    n <- createPlotCumByCategory()  
    return(n)
  })
  
  output$table_transactions <- renderDataTable({
    
    createDataTable()
  })
  }
