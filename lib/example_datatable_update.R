library(shiny)

runApp(shinyApp(
  
  ui = fluidPage(
    title = "minimal-working-example",
    fluidRow(
      column(3, inputPanel(
        selectInput("field", "Field", choices = names(mtcars)),
        numericInput("value", "Value", 0),
        actionButton("submit", "Submit")
      )),
      
      column(9,
             DT::dataTableOutput("table")
      )
    )
  ),
  
  server = function(input, output) {
    
    v <- reactiveValues(mtcars=mtcars)
    previousSelection <- NULL
    previousPage <- NULL
    
    observeEvent(input$submit, {
      previousSelection <<- input$table_rows_selected
      previousPage <<- input$table_rows_current[1] - 1
      
      v$mtcars[input$field] <- input$value
    })
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        v$mtcars,
        selection = list(mode = "single", target = "row", selected = previousSelection),
        options = list(pageLength = 5, displayStart = previousPage))
    })
  }
))