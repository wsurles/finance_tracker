library(rCharts)
library(lubridate)
library(dplyr)

setwd("~/Dev/non_work_projects/finance_tracker")
df_trans <- read.csv('data/transactions.csv', stringsAsFactors = F)

head(df_trans)
str(df_trans)

str(df2)
head(df2,20)

crunch_trans <- function(df_trans) {
  
  df2 <- df_trans %>%
    filter(!(Category %in% c('Transfer','Credit Card Payment',
                             'Hide from Budgets & Trends',
                             'Transfer for Cash Spending',
                             'Cash & ATM','Withdrawal',
                             'Sell','Buy','Deposit',
                             'Federal Tax', 'State Tax'))) %>%       
    filter(!(Category %in% c('Bonus', 'Interest Income', 'Income',
                             'Paycheck', 'Reimbursement', 
                             'Rental Income', 'Returned Purchase', 
                             'Credit Card Cashback', 'Gift Received', 
                             'Side Job', 'Dividend & Cap Gains'))) %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      date_str = as.character(date),
      year = year(date),
      yday = yday(date),
      month = month(date),
      mday = mday(date),
      Amount = ifelse(Transaction.Type == 'credit', Amount * -1, Amount)
    ) %>%
    group_by(year, Category) %>%
    summarize(
      sum_amount = sum(Amount, na.rm = T),
      count = n()
    ) %>%
    group_by() %>%
    mutate(
      sum_amount_pretty = paste0('$', prettyNum(round(sum_amount), big.mark=",",scientific=F))
      ) %>%
    data.frame()

  return(df2)
}

df2 <- crunch_trans(df_trans)
str(df2)
head(df2, 100)

p <- nPlot(sum_amount ~ count, group = 'year', data = df2, type = 'scatterChart')
p$xAxis(axisLabel = 'Number of Transactions')
p$yAxis(axisLabel = 'Total Spending')
p$chart(size = '#! function(d){return d.sum_amount} !#')
# p$xAxis(tickFormat="#! function(d) {return Math.round(Math.pow(10, d));}!#")
# p$yAxis(tickFormat="#! function(d) {return Math.round(Math.pow(10, d));}!#")
# p$chart(color = color_module)
p$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.Category + '</h3>' +
          '<p>' +  d.point.year + '</p>' + 
          '<p><b>' +  d.point.sum_amount_pretty + '</b></p>' +
          '<p>' +  d.point.count + ' Transactions' + '</p>'
          
        }
        !#")
p

