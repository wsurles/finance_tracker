
shinyUI(
	
  navbarPage(
  
    id = "nbp",
	title = "Finance", 
	theme = "bootstrap.css",
    collapsable = TRUE,
    footer = fluidRow(hr(),
              column(12, offset = 0, 
                span(strong("CONFIDENTIAL"), style = "font-family:arial;color:black;font-size:22px;"))),
    
    tabPanel("Upload", value = "upload", uiOutput("ui_upload")),
    tabPanel("Net Worth", value = "net_worth", uiOutput("ui_net_worth")),
    tabPanel("Savings", value = "savings", uiOutput("ui_savings")),
    tabPanel("Income", value = "income", uiOutput("ui_income")),
    tabPanel("Spending", value = "spending", uiOutput("ui_spending")),
    tabPanel("Giving", value = "giving", uiOutput("ui_giving")),
    tabPanel("Transactions", value = "transactions", uiOutput("ui_transactions"))
  
	)
)
