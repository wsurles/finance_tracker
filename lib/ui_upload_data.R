
##|--------------------------
##| Upload data
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
