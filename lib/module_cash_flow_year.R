
moduleCashFlowYearUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category_type"))),
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category_group"))),
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category")))
    ),
    fluidRow(
      column(4, offset = 4, align = 'center', uiOutput(ns("select_year")))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = 'center', h3("Cumulative Cash Flow by Year"))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', showOutput(ns("plot_cum_cash_flow"), "nvd3"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = 'center', h3("Monthly Cash Flow by Year"))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', showOutput(ns("plot_monthly_cash_flow"), "nvd3"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = 'center', h3("Table of Transactions"))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', dataTableOutput(ns('table_transactions')))
    )
  )
}

moduleCashFlowYear <- function(input, output, session,
  getDataTrans, getDataCategoryDim, getDataDates) {

  ns <- session$ns

  ##| --------------------------------------------
  ##| Crunch Data Functions
  ##| --------------------------------------------

  filterCategory <- reactive({

    validate(
      need(!is.null(input$select_category), "Loading Data ...")
    )

    df_category_dim <- getDataCategoryDim()

    list_category_type <- input$select_category_type

    list_category_group <- if (input$select_category_group == 'All Category Groups') {
                              unique(df_category_dim$category_group)
                            } else {
                              input$select_category_group
                            }
    list_category <- if (input$select_category == 'All Categories') {
                        unique(df_category_dim$category)
                      } else {
                        input$select_category
                      }

    df_category_dim2 <- df_category_dim %>%
      filter(
        category_type %in% list_category_type,
        category_group %in% list_category_group,
        category %in% list_category
      )

    return(df_category_dim2)
  })

  crunchDataCum <- reactive({

    df_trans <- getDataTrans()
    df_category_dim <- filterCategory()
    df_dates <- getDataDates()

    validate(
      need(nrow(df_category_dim) > 0, "No data returned from these filters")
    )

    df_trans2 <- df_trans %>%
      ## Join category dim and filter
      right_join(df_category_dim, by=c("Category" = "category")) %>%
      ## right_join to dates so all dates are present
      mutate(
        date = as.Date(Date, format = "%m/%d/%Y")
      ) %>%
      right_join(df_dates, by="date") %>%
      ## Complete the dates and filter
      mutate(
        date_str = as.character(date),
        year = year(date),
        month = str_pad(month(date), 2, pad = "0"),
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
      filter(year %in% input$select_year) %>%
      ## Convert amounts based type and complete with 0s
      mutate(
        Amount = ifelse(Transaction.Type == 'debit', Amount * -1, Amount),
        Amount = ifelse(is.na(Amount), 0, Amount)
      ) %>%
      ## Calculate cumulatives
      group_by(year) %>%
      mutate(
        cum_year_cash_flow = cumsum(Amount),
        cum_year_cash_flow_str = paste0('$', prettyNum(round(cum_year_cash_flow), big.mark=",",scientific=F))
      ) %>%
      data.frame()

    return(df_trans2)
  })

  crunchDataMonth <- reactive({

    df_trans2 <- crunchDataCum()

    df_months <- expand.grid(
      year = unique(df_trans2$year),
      year_month = unique(df_trans2$year_month)
      )

    df_trans3 <- df_trans2 %>%
      group_by(year, year_month) %>%
      summarize(
        sum_cash_flow = sum(Amount)
        ) %>%
      right_join(df_months, by = c("year","year_month")) %>%
      complete(year, year_month, fill = list(sum_cash_flow = 0)) %>%
      data.frame()

    return(df_trans3)
  })


  ##| --------------------------------------------
  ##| Chart and Table Functions
  ##| --------------------------------------------

  createPlotCumCashFlow <- reactive({

    df_trans2 <- crunchDataCum()

    ## Select on the values I need in chart so it loads faster
    df_trans3 <- df_trans2 %>%
      select(year, yday, date_str, cum_year_cash_flow, cum_year_cash_flow_str)

    ## Keep only the last value for each day so chart is smoother and loads faster
    df_plot <- df_trans3[!duplicated(df_trans3[c("yday","year")], fromLast = T),] %>%
      arrange(desc(year), yday)

    n <- nPlot(cum_year_cash_flow ~ yday, data = df_plot, group = "year", type = 'lineChart')
    n$xAxis(axisLabel = 'Day of Year')
    n$chart(forceY = c(0))
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
  })

  createPlotMonthlyCashFlow <- reactive({

    df_trans3 <- crunchDataMonth() %>%
      arrange(desc(year))

    n <- nPlot(sum_cash_flow ~ year_month, data = df_trans3, group = "year", type = 'multiBarChart')
    n$chart(stacked = T)
    n$xAxis(axisLabel = 'Year-Month')

    return(n)
  })

  createDataTableTrans <- reactive({

    df2 <- crunchDataCum()

    df_table <- df2 %>%
      filter(!(is.na(Description))) %>%
      select(date, year, month, mday, Category, Group = category_group, Type = category_type, Description, Amount, Notes) %>%
      mutate(
        year = as.character(year),
        month = as.character(month)
      ) %>%
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

  output$select_category_group <- renderUI({

    df_category_dim <- getDataCategoryDim()

    df_cat <- df_category_dim %>%
      filter(category_type %in% input$select_category_type)

    choices_list <- list("All Category Groups", "Category" = c(unique(df_category_dim$category_group)))
    selected_default <- "All Category Groups"

    container <- selectizeInput(
      inputId = ns('select_category_group'),
      label = h4("Category Group:"),
      choices = choices_list,
      selected = selected_default,
      multiple = FALSE
    )

    return(container)
  })

  output$select_category <- renderUI({

    df_category_dim <- getDataCategoryDim()

    df_cat <- df_category_dim %>%
      filter(category_type %in% input$select_category_type)

    choices_list <- list("All Categories", "Category" = c(unique(df_category_dim$category)))
    selected_default <- "All Categories"

    container <- selectizeInput(
      inputId = ns('select_category'),
      label = h4("Category:"),
      choices = choices_list,
      selected = selected_default,
      multiple = FALSE
    )

    return(container)
  })

  output$select_year <- renderUI({

    df_trans <- getDataTrans()

    dates <- as.Date(df_trans$Date, format = "%m/%d/%Y")
    min_date <- min(dates)
    max_date <- max(dates)

    df_dates <- data.frame(date = seq(min_date, max_date, 1)) %>%
      mutate(year = year(date)) %>%
      arrange(desc(date))

    choices_list <- unique(df_dates$year)
    selected_default <- choices_list[1:min(2, length(choices_list))]

    container <- selectizeInput(
      inputId = ns("select_year"),
      label = h4("Year:"),
      choices = choices_list,
      selected = selected_default,
      multiple = TRUE
    )

    return(container)
  })

  ##| --------------------------------------------
  ##| Render Output Functions
  ##| --------------------------------------------

  output$plot_cum_cash_flow <- renderChart2({
    createPlotCumCashFlow()
  })

  output$plot_monthly_cash_flow <- renderChart2({
    createPlotMonthlyCashFlow()
  })

  output$table_transactions <- renderDataTable({
    createDataTableTrans()
  })
}
