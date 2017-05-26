
moduleCashFlowQuarterUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category_type"))),
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category_group"))),
      column(4, offset = 0, align = 'center', uiOutput(ns("select_category")))
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

  crunchData <- reactive({

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
      ## Calculate cumulatives
      group_by(year_quarter) %>%
      mutate(
        cum_quarter_cash_flow = cumsum(Amount),
        cum_quarter_cash_flow_str = paste0('$', prettyNum(round(cum_quarter_cash_flow), big.mark=",",scientific=F))
      ) %>%
      data.frame()

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

  output$select_category_group <- renderUI({

    validate(
      need(!is.null(input$select_category_type), "Loading Data...")
    )

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

    validate(
      need(!is.null(input$select_category_type), "Loading Data...")
    )

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
    selected_default <- choices_list[1:min(2, length(choices_list))]

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
