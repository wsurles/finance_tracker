##| --------------------------------------------
##| Get Data Functions
##| --------------------------------------------

##| -----------
##| Net Worth
##| -----------

getDataWorth <- reactive({
  
  file_worth <- input$file_worth
  
  if(is.null(file_worth) || is.na(file_worth)) {
  } else {
    isolate({
      df_worth <- read.csv(file_worth$datapath, stringsAsFactors = F)
      # setwd("~/Dev/non_work_projects/my_life/finance")
      # df_worth <- read.csv('../data/trends.csv', stringsAsFactors = F)
    })
  }
  return(df_worth)
})

##| -------------
##| Transactions
##| -------------

getData <- reactive({
  
  file_trans <- input$file_transactions
  
  if(is.null(file_trans) || is.na(file_trans)) {
  } else {
    isolate({
      df_trans <- read.csv(file_trans$datapath, stringsAsFactors = F)
      # setwd("~/Dev/non_work_projects/my_life/finance")
	    # df_trans <- read.csv('data/transactions.csv', stringsAsFactors = F)
    })
  }

  return(df_trans)
})



##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

##| -------------
##| Net Worth
##| -------------

output$text_worth <- renderPrint({
  
  if (is.null(input$file_worth)) {
    
    return('No Worth Data Yet')
  
  } else {
  
    df_worth <- getDataWorth()
    # print(head(df_worth))
    return(head(df_worth))
  }
  
})

##| -------------
##| Transactions
##| -------------

output$text_trans <- renderPrint({
  
  if (is.null(input$file_transactions)) {
    
    return('No Transactions Data Yet')
  
  } else {
  
    df_trans <- getData()
    return(head(df_trans[,1:7]))
  }
  
})