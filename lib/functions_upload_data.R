##|--------------------------
##| UI Layout
##|--------------------------

output$upload_data <- renderUI({
  tagList(
    fluidRow(
      column(4, offset = 4, align ='center', h3("Upload Transactions:"))
    ),
    fluidRow(
      column(4, offset = 4, align = 'center', uiOutput("select_file_transactions"))
    ),
    fluidRow(
      column(12, offset = 0, align ='center', verbatimTextOutput("sample_transactions_csv"))
    ),
    fluidRow(
      column(4, offset = 4, align = 'center', uiOutput("select_file_categories"))
    ),
    fluidRow(
      column(12, offset = 0, align ='center', verbatimTextOutput("sample_categories_csv"))
    )
  )
})

##| --------------------------------------------
##| Get Data Functions
##| --------------------------------------------

getDataTrans <- reactive({

  validate(
    need(!is.null(input$file_transactions), "Please load transactions.csv")
  )

  df_trans <- read.csv(input$file_transactions$datapath, stringsAsFactors = F)

  return(df_trans)
})

getDataCategoryDim <- reactive({

  validate(
    need(!is.null(input$file_categories), "Please load categories.csv")
  )

  df_category <- read.csv(input$file_categories$datapath, stringsAsFactors = F)

  return(df_category)
})

getDataDates <- reactive({

  df_trans <- getDataTrans()
  print("cha brah")
  print(head(df_trans))

  dates <- as.Date(df_trans$Date, format = "%m/%d/%Y")
  min_date <- min(dates) %>% year(.) %>% str_c(.,"-01-01") %>% as.Date(.)
  max_date <- max(dates) %>% year(.) %>% str_c(.,"-12-31") %>% as.Date(.)

  df_dates <- data.frame(date = seq(min_date, max_date, 1))

  return(df_dates)
})

##| --------------------------------------------
##| Render Input Functions
##| --------------------------------------------

output$select_file_transactions <- renderUI({

  container <- fileInput(
    inputId = 'file_transactions',
    label = 'load transactions.csv from mint.com',
    multiple = F,
    accept = c('csv')
  )

  return(container)

})

output$select_file_categories <- renderUI({

  container <- fileInput(
    inputId = 'file_categories',
    label = 'load categories.csv',
    multiple = F,
    accept = c('csv')
  )

  return(container)

})

##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$sample_transactions_csv <- renderPrint({

  df_trans <- getDataTrans()

  return(head(df_trans))

})

output$sample_categories_csv <- renderPrint({

  df_category_dim <- getDataCategoryDim()

  return(head(df_category_dim))

})
