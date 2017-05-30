##|--------------------------
##| UI Layout
##|--------------------------

output$upload_data <- renderUI({
  tagList(
    fluidRow(
      column(4, offset = 4, align ='center', h2("Upload Transactions:"))
    ),
    fluidRow(
      column(4, offset = 4, align = 'center', uiOutput("select_file_transactions"))
    ),
    fluidRow(
      column(4, offset = 4, align ='center', h4("Sample of loaded transaction data:"))
    ),
    fluidRow(
      column(6, offset = 3, align ='center', verbatimTextOutput("sample_transactions_csv"))
    ),
    ##--------------------------------------------------------------------------
    hr(),
    fluidRow(
      column(4, offset = 4, align ='center', h2("Upload Custom Categories:"))
    ),
    fluidRow(
      column(4, offset = 4, align ='center',
        h5("The default mint.com categories have already been loaded for you.
        If you want to add more categories or customize the categories,
        use the 'Download Categories' button below to get the categories csv file.
        Add and customize your categories then load them here."))
    ),
    fluidRow(
      column(4, offset = 4, align = 'center', uiOutput("select_file_categories"))
    ),
    fluidRow(
      column(12, offset = 0, align ='center',
        downloadButton('button_download_categories',
          label = "Download Default Categories"
        )
      )
    ),
    fluidRow(
      column(4, offset = 4, align ='center', h4("Sample of categories data:"))
    ),
    fluidRow(
      column(6, offset = 3, align ='center', verbatimTextOutput("sample_categories_csv"))
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
  print("running getDataTrans")
  df_trans <- read.csv(input$file_transactions$datapath, stringsAsFactors = F)

  return(df_trans)
})

getDataCategoryDim <- reactive({

  if (is.null(input$file_categories)) {

    df_category <- read.csv('data/category_dimension_defaults.csv', stringsAsFactors = F)

  } else {

    df_category <- read.csv(input$file_categories$datapath, stringsAsFactors = F)

  }

  ## Fill in any missing categories that are in the transactions data but not in the categories data
  ## I will give them group and type of "unknown"

  df_trans <- getDataTrans()
  df_category_trans <- data.frame(category = unique(df_trans$Category),
                                  stringsAsFactors = F)

  df_category2 <- df_category %>%
    right_join(df_category_trans, by = "category") %>%
    complete(category, fill = list(category_group = "Unknown", category_type = "Unknown")) %>%
    data.frame()

  return(df_category2)
})

getDataDates <- reactive({

  df_trans <- getDataTrans()

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
    label = '',
    multiple = F,
    accept = c('csv')
  )

  return(container)

})

output$button_download_categories <- downloadHandler(

  filename = function() {
    "category_dimension_defaults.csv"
  },
  content = function(file) {
    write.csv(getDataCategoryDim(), file)
  }
)

##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$sample_transactions_csv <- renderPrint({

  df <- getDataTrans() %>%
    head(20) %>%
    mutate(Original.Description = str_trunc(Original.Description, 25, side = "right"))

  return(df)

})

output$sample_categories_csv <- renderPrint({

  df <- getDataCategoryDim() %>%
    head(20)

  return(df)

})
