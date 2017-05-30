
csvFileInput <- function(id, label = "CSV File") {
  ns <- NS(id)

  tagList(
    fileInput(inputId = ns('file'),
              label = label,
              multiple = F,
              accept = c('csv')
    )
  )
}

csvFile <- function(input, output, session) {

  csvFile <- reactive({
    validate(need(input$file, message = "Please Load CSV file"))
    return(input$file)
  })

  df_file <- reactive({
    read.csv(csvFile()$datapath, stringsAsFactors = F)
  })

  observe({
    msg <- sprintf("File %s was uploaded", csvFile()$name)
    cat(msg, "\n")
  })

  return(df_file)
}
