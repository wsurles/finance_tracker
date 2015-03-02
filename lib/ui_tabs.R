
##|---------
##| Upload
##|---------

output$ui_upload <- renderUI({
  
  list(
    fluidRow(
      column(3, offset = 3,
        h4("Upload Trends of Net Worth:"),
        fileInput(inputId = 'file_worth', 
                  label = '', 
                  multiple = F,
                  accept = c('csv')
        )
      ),
      column(3,
        h4("Upload Transactions:"),
        fileInput(inputId = 'file_transactions', 
                  label = '', 
                  multiple = F,
                  accept = c('csv')
        )
      )
    ),
    fluidRow(
      column(12,
        verbatimTextOutput("text_worth"), align = 'center'
      )
    ),
    fluidRow(
      column(12,
        verbatimTextOutput("text_trans"), align = 'center'
      )
    )
  )
})

##|-----------
##| Net Worth
##|-----------

output$ui_net_worth <- renderUI({
  

  list(
    
    ##| Chart
    fluidRow(
      column(12,
        h3("Net Worth Growth YOY", align = 'center'),
        h5("This shows my net worth growth by year. ", align = 'center'),
        showOutput("plot_net_worth_growth", "nvd3"),
        hr(),
        h3("Net Worth Total YOY", align = 'center'),
        h5("This shows my total net worth by year. ", align = 'center'),
        showOutput("plot_net_worth_total", "nvd3")
      )
    ),
    hr(),
    
    ##| Table
    fluidRow(
      column(12,
        column(10, offset = 1, dataTableOutput('table_net_worth'))
      )
    )
  )
})

##|---------
##| Savings
##|---------

output$ui_savings <- renderUI({
  list(
    fluidRow(
      column(12,
        h3("Cumulative Savings by Year", align = 'center'),
        showOutput("plot_savings_yoy", "nvd3"),
        hr(),

        h3("Total Monthly Savings", align = 'center'),
        showOutput("plot_total_savings_month", "nvd3"),
        hr(),

        h3("Cumulative Monthly Savings", align = 'center')
      )
    ),
    fluidRow(
      column(12, 
        column(3, offset = 3, uiOutput("savings_year")),
        column(3, offset = 0, uiOutput("savings_month"))
      )
    ),
    fluidRow(
      column(12, 
        showOutput("plot_cum_savings_month", "nvd3")
      )
    ),
    fluidRow(
      column(12, 
        h3("Table of Transactions", align = 'center'),
        column(10, offset = 1, dataTableOutput('table_savings'))
      )      
    )
  )
})  

##|---------
##| Income
##|---------

output$ui_income <- renderUI({
  list(
    fluidRow(
      column(12,
        h3("Cumulative Income by Year", align = 'center'),
        showOutput("plot_income_yoy", "nvd3"),
        hr(),

        h3("Total Monthly Income", align = 'center'),
        showOutput("plot_total_income_month", "nvd3"),
        hr(),

        h3("Cumulative Monthly Income", align = 'center')
      )
    ),
    fluidRow(
      column(12, 
        column(3, offset = 3, uiOutput("income_year")),
        column(3, offset = 0, uiOutput("income_month"))
      )
    ),
    fluidRow(
      column(12, 
        showOutput("plot_cum_income_month", "nvd3")
      )
    ),
    fluidRow(
      column(12, 
        h3("Table of Transactions", align = 'center'),
        column(10, offset = 1, dataTableOutput('table_income'))
      )      
    )
  )
}) 

##|----------
##| Spending
##|----------

output$ui_spending <- renderUI({
  list(
    fluidRow(
      column(12,
        h3("Cumulative Spending by Year", align = 'center'),
        showOutput("plot_cum_spending_year", "nvd3"),
        
        h3("Total Monthly Spending", align = 'center'),
        showOutput("plot_total_spending_month", "nvd3"),
        hr(),

        h3("Cumulative Monthly Spending", align = 'center')
    )),
    fluidRow(
      column(12,
        column(3, offset = 3, uiOutput("spending_year")),
        column(3, offset = 0, uiOutput("spending_month"))
    )),
    fluidRow(
      column(12,
        showOutput("plot_cum_spending_month", "nvd3")
    )),
    fluidRow(
      h3("Table of Transactions", align = 'center'),
      column(10, offset = 1, dataTableOutput('table_spending'))
    )
  )
}) 

##|---------
##| Giving
##|---------

output$ui_giving <- renderUI({
  list(
    fluidRow(
      column(12,
        h3("Cumulative Giving by Year", align = 'center'),
        showOutput("plot_giving_yoy", "nvd3"),
        hr(),

        h3("Total Monthly Giving", align = 'center'),
        showOutput("plot_total_giving_month", "nvd3"),
        hr(),

        h3("Cumulative Monthly Giving", align = 'center')
      )
    ),
    fluidRow(
      column(12, 
        column(3, offset = 3, uiOutput("giving_year")),
        column(3, offset = 0, uiOutput("giving_month"))
      )
    ),
    fluidRow(
      column(12, 
        showOutput("plot_cum_giving_month", "nvd3")
      )
    ),
    fluidRow(
      column(12, 
        h3("Table of Transactions", align = 'center'),
        column(10, offset = 1, dataTableOutput('table_giving'))
      )      
    )
  )
}) 

##|---------
##| Transactions
##|---------

output$ui_transactions <- renderUI({
  list(
    fluidRow(
      column(12,
        h3("Transactions Explore", align = 'center'),
        showOutput("plot_transactions_explore", "nvd3"),
        hr()
      )
    ),
    fluidRow(
      column(12, 
        h3("Table of Transactions", align = 'center'),
        column(10, offset = 1, dataTableOutput('table_transactions'))
      )      
    )
  )
}) 

