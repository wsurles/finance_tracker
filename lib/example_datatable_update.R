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

##-------------------------------------

shiny::runGitHub("rhandsontable", "jrowen", subdir = "inst/examples/rhandsontable_corr")


##----------------------------

devtools::install_github('rstudio/DT@feature/editor')

library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    DT::dataTableOutput('x1')
  ),
  server = function(input, output, session) {
    x = iris
    x$Date = Sys.time() + seq_len(nrow(x))
    output$x1 = DT::renderDataTable(x, selection = 'none')
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      x[i, j] <<- DT:::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE)
    })
  }
)