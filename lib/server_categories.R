##| Create selection input - Owner

category_ignore <- c('Transfer','Credit Card Payment',
  'Hide from Budgets & Trends',
  'Transfer for Cash Spending',
  'Cash & ATM','Withdrawal',
  'Sell','Buy','Deposit',
  'Federal Tax', 'State Tax')


category_income <-  c('Bonus', 'Interest Income', 
 'Paycheck', 'Reimbursement', 
 'Rental Income', 'Returned Purchase', 
 'Credit Card Cashback', 'Gift Received', 
 'Side Job')

category_giving <-  c('Charity','Gift','Church Tithe','Missions Support')

output$ignore <- renderUI({
    
  ignore_list <- sort(unique(df_trans$Category))
  
  selectizeInput(inputId = "category_ignore",
                 label = h4("Ignore:"),
                 choices = ignore_list,
                 multiple = TRUE,
                 selected = 'Transfer')
})