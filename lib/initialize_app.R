
##|-------------------------------------
##| Load the data manually
##|-------------------------------------

## Transactions
df_trans <- read.csv("/Users/surwi01/Documents\ Personal/Finance/mint_data_shared/transactions.csv", stringsAsFactors = F)
head(df_trans)

## Categories
df_category <- read.csv('data/category_dimension_defaults.csv', stringsAsFactors = F)
# df_category <- read.csv(input$file_categories$datapath, stringsAsFactors = F)
head(df_category)

## Dates
dates <- as.Date(df_trans$Date, format = "%m/%d/%Y")
min_date <- min(dates) %>% year(.) %>% str_c(.,"-01-01") %>% as.Date(.)
max_date <- max(dates) %>% year(.) %>% str_c(.,"-12-31") %>% as.Date(.)

df_dates <- data.frame(date = seq(min_date, max_date, 1))
head(df_dates)

## First, need to filter the categories down to just the ones that show up in the transactions file

df_category_trans <- data.frame(category = unique(df_trans$Category),
                                stringsAsFactors = F)

df_category2 <- df_category %>%
  right_join(df_category_trans, by = "category") %>%
  complete(category, fill = list(category_group = "Unknown", category_type = "Unknown")) %>%
  data.frame()

str(df_category2)
head(df_category2)
table(df_category2$category_group, useNA = "ifany")
table(df_category2$category_type, useNA = "ifany")

## Then, for anything that is in the transactions but not in the categories dimension
## we need to give them a default group and type as unknown or something like that
## So they will not be filtered out by the right join onto the categories table, 
## and will still show up in the charts
