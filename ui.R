
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Surlygon Finances'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Load Transactions", tabName = 'load_transactions'),
        menuItem("Cash Flow Year", tabName = 'cash_flow_year'),
        menuItem("Cash Flow Quarter", tabName = 'cash_flow_quarter')
      )
    ),
    dashboardBody(
      includeCSS("www/custom.css"),
      tabItems(
        tabItem(tabName = "load_transactions", uiOutput("ui_upload")),
        tabItem(tabName = "cash_flow_year", moduleCashFlowYearUI("year")),
        tabItem(tabName = "cash_flow_quarter", moduleCashFlowQuarterUI("quarter"))
      )
    )
  )
)