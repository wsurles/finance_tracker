
moduleCashFlowQuarterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Filters",
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(4, offset = 0, align = 'center', uiOutput(ns("select_category_type"))),
        column(4, offset = 0, align = 'center', uiOutput(ns("select_category")))
      )
    ),
    fluidRow(
      column(6, offset = 3, align = 'center', uiOutput(ns("select_quarter")))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center',
        h3("Cumulative Cash Flow by Quarter"),
        showOutput(ns("plot_cum_by_quarter"), "nvd3")
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

moduleCashFlowQuarter <- function(input, output, session) {
  
  ns <- session$ns
  
  ##| --------------------------------------------
  ##| Crunch Data Functions
  ##| --------------------------------------------
  
  # filterCategoryType <- reactive({
  #   
  #   validate(
  #     need(!is.null(input$select_category_type), "Loading Data ...")
  #   )
  #   
  #   df_trans <- getData()
  #   
  #   ##| Filter Category Type
  #   
  #   list_exclude_all <- c('Transfer',
  #                         'Credit Card Payment',
  #                         'Hide from Budgets & Trends',
  #                         'Transfer for Cash Spending',
  #                         'Cash & ATM',
  #                         'Withdrawal',
  #                         'Sell',
  #                         'Buy',
  #                         'Deposit',
  #                         'Federal Tax', 
  #                         'State Tax')
  # 
  #   list_include_income <- c('Income', 
  #                            'Bonus', 
  #                            'Interest Income', 
  #                            'Paycheck', 
  #                            'Reimbursement', 
  #                            'Rental Income', 
  #                            'Returned Purchase', 
  #                            'Credit Card Cashback', 
  #                            'Gift Received', 
  #                            'Side Job')
  #   
  #   list_include_giving <- c('Charity',
  #                            'Gift',
  #                            'Church Tithe',
  #                            'Missions Support')
  #   
  #   list_include_spending <- setdiff(
  #     unique(df_trans$Category), 
  #     c(list_exclude_all, list_include_income, list_include_giving)
  #     )
  #   
  #   list_include_all <- setdiff(
  #     unique(df_trans$Category), 
  #     c(list_exclude_all)
  #     )
  #   
  #   if (input$select_category_type == 'All') {
  #     
  #     list_include <- list_include_all
  #   
  #   } else if (input$select_category_type == 'Income') {
  #     
  #     list_include <- list_include_income
  #   
  #   } else if (input$select_category_type == 'Spending') {
  #     
  #     list_include <- list_include_spending
  #   
  #   } else if (input$select_category_type == 'Giving') {
  #     
  #     list_include <- list_include_giving
  #   
  #   }
  #   
  #   df_trans2 <- df_trans %>%
  #     filter(Category %in% list_include)
  #     
  #   return(df_trans2)
  #     
  # })
  # 
  
  filterCategory <- reactive({

    validate(
      need(!is.null(input$select_category), "Loading Data ...")
    )

    df_category_dim <- getDataCategoryDim()

    if (input$select_category == 'All Categories') {

      df_category_dim2 <- df_category_dim

    } else {

      df_category_dim2 <- df_category_dim %>%
        filter(category %in% input$select_category)
    }
    
    df_category_dim3 <- df_category_dim2 %>%
      filter(
        category_type %in% input$select_category_type
      )
      
    print(df_category_dim3)
    
    return(df_category_dim3)
  })
  # 
  # filterQuarter <- reactive({
  #   
  #   validate(
  #     need(!is.null(input$select_quarter), "Loading Data ...")
  #   )
  #   
  #   df_dates <- getDates()
  #     
  #   df_dates2 <- df_dates %>%
  #     filter(year_quarter %in% input$select_quarter)
  #   
  #   return(df_dates2)
  # })
    
  crunchData <- reactive({
    
    # df_trans <- filterCategory()
    # df_dates <- filterQuarter()
    
    df_trans <- getDataTrans()
    df_category_dim <- filterCategory()
    df_dates <- getDataDates()
    
    validate(
      need(nrow(df_category_dim) > 0, "No data returned from these filters")
    )

    comment <- function() {
      
      input <- list(
        select_quarter = c("2017-2","2017-1")
      )
    }
    
    df_trans2 <- df_trans %>%
      ## Join category dim and filter
      right_join(df_category_dim, by=c("Category" = "category")) %>%
      # filter(
      #   category_type %in% input$select_category_type,
      #   Category %in% input$select_category
      # ) %>%
      ## right_join to dates so all dates are present
      mutate(
        date = as.Date(Date, format = "%m/%d/%Y")
      ) %>%
      right_join(df_dates, by="date") %>%
      ## Complete the dates and filter
      mutate(
        date_str = as.character(date),
        year = year(date),
        month = month(date),
        mday = mday(date),
        yday = yday(date),
        year_month = str_c(year, month, sep="-"),
        quarter = quarter(date)
      ) %>%
      arrange(year, yday) %>%
      group_by(year, quarter) %>%
      mutate(
        qday = dense_rank(yday),
        year_quarter = str_c(year, quarter, sep="-")
      ) %>%
      filter(year_quarter %in% input$select_quarter) %>%
      ## Convert amounts based type and complete with 0s
      mutate(
        Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount),
        Amount = ifelse(is.na(Amount), 0, Amount)
      ) %>%
      ## Calculate cummulatives
      group_by(year_quarter) %>%
      mutate(
        cum_quarter_cash_flow = cumsum(Amount),
        cum_quarter_cash_flow_str = paste0('$', prettyNum(round(cum_quarter_cash_flow), big.mark=",",scientific=F))
      ) %>%
      data.frame()
      
    head(df_trans2)
    
    return(df_trans2)
  })
  
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
    n$chart(forceY = c(0))
    n$chart(tooltipContent = "#!
          function(key, x, y, d){ 
            return '<h3>' + d.point.year_quarter + '</h3>' +
            '<p><b>' + d.point.date_str + '</b></p>' + 
            '<p><b>'  + d.point.cum_quarter_cash_flow_str + '</b></p>'
          }
          !#")
    
    return(n)  
  })
  
  createDataTable <- reactive({
    
    df2 <- crunchData()
    
    df_table <- df2 %>%
      select(date, year, month, year_quarter, qday, Category, Group = category_group, Type = category_type, Description, Amount, Notes) %>%
      # mutate(
      #   date = as.character(date),
      #   year = as.character(year),
      #   month = as.character(month),
      #   year_quarter = as.character(year_quarter),
      #   yday = as.character(qday)
      #   ) %>%
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
    
    df_category_dim <- getDataCategoryDim()
    
    choices_list <- list("All Category Types", "Category Type" = c(unique(df_category_dim$category_type)))
    selected_default <- setdiff(unique(df_category_dim$category_type), c("Ignore","Savings"))
    
    container <- selectizeInput(
      inputId = ns("select_category_type"),
      label = h4("Category Type:"),
      choices = choices_list,
      selected = selected_default,
      multiple = TRUE
    )
    
    return(container)
  })
  
  output$select_category <- renderUI({
    
    validate(
      need(!is.null(input$select_category_type), "Loading Data...")
    )
    
    df_category_dim <- getDataCategoryDim()
    
    df_cat <- df_category_dim %>%
      filter(category_type %in% input$select_category_type)
    
    choices_list <- list("All Categories", "Category" = c(unique(df_category_dim$category)))
    selected_default <- "All Categories"
    
    # df_trans <- filterCategoryType()
    # 
    # df_category <- df_trans %>% 
    #   arrange(Category) %>% 
    #   distinct(Category)
    # 
    # list_choices <- c('All Categories', df_category)
    # selected_default <- 'All Categories'
    # 
    container <- selectizeInput(
      inputId = ns('select_category'),
      label = h4("Category:"),
      choices = choices_list,
      selected = selected_default,
      multiple = FALSE
    )
    
    return(container)
  }) 
  
  output$select_quarter <- renderUI({
    
    df_trans <- getDataTrans()
    
    dates <- as.Date(df_trans$Date, format = "%m/%d/%Y")
    min_date <- min(dates)
    max_date <- max(dates)
    
    df_dates <- data.frame(date = seq(min_date, max_date, 1)) %>%
      mutate(
        year_quarter = str_c(year(date), quarter(date), sep="-")
      ) %>%
      arrange(desc(date))
    
    choices_list <- unique(df_dates$year_quarter)
    
    selected_default <- c(
      str_c(year(max_date), quarter(max_date), sep="-"),
      str_c(year(max_date-90), quarter(max_date-90), sep="-")
    )
    
    # max_date <- max(dates)
    # 
    # df_dates <- data.frame(date = seq(min_date, max_date, 1))
    # 
    # df2 <- df_trans %>%
    #   mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
    #   left_join(df_dates, by = "date") %>%
    #   mutate(
    #     year = year(date),
    #     quarter = quarter(date),
    #     year_quarter = str_c(year, quarter, sep="-")
    #   )
    #   
    # choices_list <- sort(unique(df2$year_quarter), decreasing=T)
    # selected_default <- choices_list[1:2]
    # 
    
    container <- selectizeInput(
      inputId = ns("select_quarter"),
      label = h4("Quarter:"),
      choices = choices_list,
      selected = selected_default,
      multiple = TRUE
    )
    
    return(container)
  })
  
  ##| --------------------------------------------
  ##| Render Output Functions
  ##| --------------------------------------------
  
  output$plot_cum_by_quarter <- renderChart2({    
    
    n <- createPlotCumByQuarter()  
    return(n)
  })
  
  output$table_transactions <- renderDataTable({
  
    createDataTable()
  })
}
