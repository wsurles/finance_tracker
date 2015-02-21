
options(RCHART_WIDTH = 1000)

shinyServer(function(input, output, session) {
  
  source('lib/ui_tabs.R', local = T)
  source('lib/server_input.R', local = T)
  source('lib/server_categories.R', local = T)
  source('lib/server_net_worth.R', local = T)
  source('lib/server_savings.R', local = T)
  source('lib/server_income.R', local = T)
  source('lib/server_spending.R', local = T)
  source('lib/server_giving.R', local = T)
  
})

#changing things


