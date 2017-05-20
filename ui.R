
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Surlygon Finances'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Load Transactions", tabName = 'load_transactions'),
        menuItem("Cash Flow",
          menuSubItem("All",      tabName = "all"),
          menuSubItem("Income",   tabName = "income"),
          menuSubItem("Spending", tabName = "spending"),
          menuSubItem("Giving",   tabName = "giving"),
          menuSubItem("Saving",   tabName = "saving")
        )
      )
    ),
    dashboardBody(
      includeCSS("www/custom.css"),
      tabItems(
        tabItem(tabName = "load_transactions", uiOutput("ui_upload")),
        tabItem(tabName = "all",               uiOutput("ui_savings")),
        tabItem(tabName = "income",            uiOutput("ui_income")),
        tabItem(tabName = "spending",          uiOutput("ui_spending")),
        tabItem(tabName = "giving",            uiOutput("ui_giving"))
      )
    )
  )
)