
moduleCashFlowCategoryUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category_type"))),
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category_group"))),
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category")))
    ),
    fluidRow(
      column(4, offset = 4, align = 'center', uiOutput(ns("select_date_range")))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = "center", h1("Cummulative Cash Flow by Category"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(2, offset = 4, align = 'center', uiOutput(ns("select_cum_min"))),
      column(2, offset = 0, align = 'center', uiOutput(ns("select_cum_max")))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', h3("Cumulative Cash Flow by Category"))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', showOutput(ns("plot_cum_by_category"), "nvd3"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = 'center', h3("Table of Transactions"))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', dataTableOutput(ns('table_transactions'))
      )
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = "center", h1("Total Cash Flow by Category"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = "center", h3("Sunburst of Categories"))
    ),
    fluidRow(
      column(11, offset = 0, align = "center", sunburstOutput(ns("sunburst"), width = "100%", height = "800px"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(12, offset = 0, align = 'center', h3("Table of Category Totals"))
    ),
    fluidRow(
      column(12, offset = 0, align = 'center', dataTableOutput(ns('table_cum_total')))
    )
  )
}

moduleCashFlowCategory <- function(input, output, session,
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

  filterDateRange <- reactive({

    validate(
      need(!is.null(input$select_date_range), "Loading Data ...")
    )

    df_dates <- getDataDates()

    df_dates2 <- df_dates %>%
      filter(
        date >= input$select_date_range[1],
        date <= input$select_date_range[2]
      )

    return(df_dates2)
  })

  crunchData <- reactive({

    df_trans <- getDataTrans()
    df_category_dim <- filterCategory()
    df_dates <- filterDateRange()

    validate(
      need(nrow(df_category_dim) > 0, "No data returned from these filters")
    )

    df_trans2 <- df_trans %>%
      right_join(df_category_dim, by=c("Category" = "category")) %>%
      mutate(
        date = as.Date(Date, format = "%m/%d/%Y")
      ) %>%
      right_join(df_dates, by="date") %>%
      mutate(
        date_str = as.character(date),
        year = year(date),
        month = str_pad(month(date), 2, pad = "0"),
        mday = mday(date),
        yday = yday(date),
        year_month = str_c(year, month, sep="-"),
        quarter = quarter(date)
      ) %>%
      arrange(date) %>%
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

    return(df_trans2)
  })

  calcCumTotal <- reactive({

    df_trans2 <- crunchData()

    df_cum_total <- df_trans2 %>%
      # group_by(Category) %>%
      group_by(category_type, category_group, Category) %>%
      summarize(cum_total = last(cum_category_cash_flow)) %>%
      filter(!is.na(category_type)) %>%
      data.frame()

    return(df_cum_total)

  })

  filterByCumTotal <- reactive({

    validate(
      need(!is.null(input$select_cum_min), "Loading Data ...")
    )

    df_cum_total <- calcCumTotal()
    df_trans2 <- crunchData()

    df_cum_total2 <- df_cum_total %>%
      filter(
        cum_total >= input$select_cum_min,
        cum_total <= input$select_cum_max
      )

    list_categories <- unique(df_cum_total2$Category)

    df_trans3 <- df_trans2 %>%
      filter(Category %in% list_categories)

    return(df_trans3)

  })

  ##| --------------------------------------------
  ##| Chart and Table Functions
  ##| --------------------------------------------

  createPlotCumByCategory <- reactive({

    # df_trans2 <- crunchData()
    df_trans3 <- filterByCumTotal()

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

  createDataTableTrans <- reactive({

    df_trans3 <- filterByCumTotal()

    df_table <- df_trans3 %>%
      select(date, Type = category_type, Group = category_group, Category, Description, Amount, Notes) %>%
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

  createSunburst <- reactive({

    df_cum_total <- calcCumTotal()

    df_sunburst <- df_cum_total %>%
      filter(category_type %in% c("Spending","Giving","Savings")) %>%
      mutate(
        seq = str_c(category_type, category_group, Category, sep="-"),
        cum_total = cum_total * -1
      ) %>%
      select(seq, cum_total) %>%
      arrange(desc(cum_total))

    sunburst(df_sunburst, count=T)

  })

  createDataTableCumTotal <- reactive({

    df_cum_total <- calcCumTotal()

    df_table <- df_cum_total %>%
      arrange(cum_total)

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
      multiple = TRUE
    )

    return(container)
  })

  output$select_date_range <- renderUI({

    df_trans <- getDataTrans()

    dates <- as.Date(df_trans$Date, format = "%m/%d/%Y")

    min_date <- min(dates)
    max_date <- max(dates)

    selected_date_start <- as.Date(str_c(year(Sys.Date()), "-01-01"))
    selected_date_end <- max_date

    container <- dateRangeInput(
      inputId = ns("select_date_range"),
      label = h4("Date Range"),
      start  = selected_date_start,
      end    = selected_date_end,
      min    = min_date,
      max    = max_date,
      format = "yyyy-mm-dd"
    )

    return(container)
  })

  output$select_cum_min <- renderUI({

    df_cum_total <- calcCumTotal()

    value_default <- min(df_cum_total$cum_total)

    container <- numericInput(
      inputId = ns('select_cum_min'),
      label = h4("Min Cummulative Value:"),
      value = value_default
    )

    return(container)
  })

  output$select_cum_max <- renderUI({

    df_cum_total <- calcCumTotal()

    value_default <- max(df_cum_total$cum_total)

    container <- numericInput(
      inputId = ns('select_cum_max'),
      label = h4("Max Cummulative Value:"),
      value = value_default
    )

    return(container)
  })

  ##| --------------------------------------------
  ##| Render Output Functions
  ##| --------------------------------------------

  output$plot_cum_by_category <- renderChart2({
    createPlotCumByCategory()
  })

  output$sunburst <- renderSunburst({
    createSunburst()
  })

  output$table_transactions <- renderDataTable({
    createDataTableTrans()
  })

  output$table_cum_total <- renderDataTable({
    createDataTableCumTotal()
  })
}
