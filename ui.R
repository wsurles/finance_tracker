
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Surlygon Finances'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload Data", tabName = 'upload_data'),
        menuItem("Cash Flow",
          menuSubItem("By Year", tabName = 'cash_flow_year'),
          menuSubItem("By Quarter", tabName = 'cash_flow_quarter'),
          menuSubItem("By Month", tabName = 'cash_flow_month'),
          menuSubItem("By Category", tabName = 'cash_flow_category')
        )
      )
    ),
    dashboardBody(
      includeCSS("www/custom.css"),
      tabItems(
        tabItem(tabName = "upload_data", uiOutput("upload_data")),
        tabItem(tabName = "cash_flow_year", moduleCashFlowYearUI("year")),
        tabItem(tabName = "cash_flow_quarter", moduleCashFlowQuarterUI("quarter")),
        tabItem(tabName = "cash_flow_month", moduleCashFlowMonthUI("month")),
        tabItem(tabName = "cash_flow_category", moduleCashFlowCategoryUI("category"))
      )
    )
  )
)