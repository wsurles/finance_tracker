
options(RCHART_WIDTH = 1000)

shinyServer(function(input, output, session) {
  
  
  source('lib/ui_tabs.R', local = T)
  source('lib/server_input.R', local = T)
  
  callModule(moduleCashFlowYear, "year")
  callModule(moduleCashFlowQuarter, "quarter")
  callModule(moduleCashFlowMonth, "month")
  callModule(moduleCashFlowCategory, "category")
  
})


