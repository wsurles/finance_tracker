
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Surlygon Finances'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Load Transactions", tabName = 'load_transactions'),
        menuItem("Cash Flow", tabName = 'cash_flow')
      )
    ),
    dashboardBody(
      includeCSS("www/custom.css"),
      tabItems(
        tabItem(tabName = "load_transactions", uiOutput("ui_upload")),
        tabItem(tabName = "cash_flow", moduleCashFlowUI("all"))
      )
    )
  )
)