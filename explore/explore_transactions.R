library(rCharts)
library(lubridate)
library(dplyr)

##|----------------
##| Get Data
##|----------------
setwd("~/Dev/non_work_projects/finance_tracker")
df_trans <- read.csv('data/transactions.csv', stringsAsFactors = F)

##|----------------
##| Crunch Data
##|----------------

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
  data.frame()

df_expand <- expand.grid(year = unique(df2$year), Category = unique(df2$Category)) %>%
  mutate(Category = as.character(Category)) %>%
  data.frame()

df4 <- left_join(df_expand, df2, by=c("year","Category"))
df4[is.na(df4)] <- 0
df5 <- df4 %>%
  group_by(Category) %>%
  mutate(total = sum(sum_amount)) %>%
  group_by() %>%
  arrange(desc(total)) %>%
  mutate(
    sum_amount_pretty = paste0('$', prettyNum(round(sum_amount), big.mark=",",scientific=F))
  ) %>%
  data.frame()

head(df5)
##|----------------
##| Scatter Chart
##|----------------
p <- nPlot(sum_amount ~ count, group = 'year', data = df5, type = 'scatterChart')
p$xAxis(axisLabel = 'Number of Transactions')
p$yAxis(axisLabel = 'Total Spending')
p$chart(size = '#! function(d){return d.sum_amount} !#')
p$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.Category + '</h3>' +
          '<p>' +  d.point.year + '</p>' + 
          '<p><b>' +  d.point.sum_amount_pretty + '</b></p>' +
          '<p>' +  d.point.count + ' Transactions' + '</p>'
          
        }
        !#")
p


##|----------------
##| Bar Chart
##|----------------
p2 <- nPlot(sum_amount ~ Category, group = 'year', data = df5, type = "multiBarHorizontalChart")
p2$chart(margin = list(top = 100, right = 20, bottom = 50, left = 180), height=2000, width=800)
p2

