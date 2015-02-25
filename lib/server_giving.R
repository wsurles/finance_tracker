
##| --------------------------------------------
##| Crunch Data Functions
##| --------------------------------------------

crunchDataGiving <- function(df_trans) {

  ## Filter
  df_trans2 <- df_trans %>%
    mutate(
      date = as.Date(Date, format = "%m/%d/%Y"),
      yday = yday(date),
      year = year(date)
    ) %>%
    filter(Category %in% c('Charity','Gift','Church Tithe','Missions Support'))

  ## Expand Dates
  grid <- expand.grid(year = unique(df_trans2$year), yday = seq(1:365))
  df <- merge(grid, df_trans2, by=c("year","yday"), all.x = T)
  
  ## Crunch by year and month
  df_trans2 <- df %>%
    mutate(
      date = as.Date(paste0(year,'-',yday), format = "%Y-%j"),
      date_str = as.character(date),
      yday = yday(date),
      mday = mday(date),
      month = month(date),
      year = year(date),
      Amount = ifelse(is.na(Amount), 0, Amount)
    ) %>%
    group_by(year) %.%
    mutate(
      cum_year_giving = cumsum(Amount),
      cum_year_giving_str = paste0('$', prettyNum(round(cum_year_giving), big.mark=",",scientific=F))
    ) %>%
    group_by(year, month) %.%
    mutate(
      cum_month_giving = cumsum(Amount),
      cum_month_giving_str = paste0('$', prettyNum(round(cum_month_giving), big.mark=",",scientific=F))
    ) %>%
    filter(date <= today()) %>%
    arrange(date)

  df_trans2 <- as.data.frame(df_trans2)

  return(df_trans2)
}

filterGiving <- function(df_trans2) {
    
  df <- df_trans2
  df$show <- TRUE
  
  ## Filter dots 
  if (is.null(input$giving_year) == F & is.null(input$giving_month) == F ) {
    
    df$show[!(df$year %in% input$giving_year & df$month %in% input$giving_month)] <- FALSE
  
  } else if (is.null(input$giving_year) == F) {
  
    df$show[!(df$year %in% input$giving_year)] <- FALSE
  
  } else if (is.null(input$giving_month) == F) {
  
    df$show[!(df$month %in% input$giving_month)] <- FALSE
  
  }
  
  df <- as.data.frame(df) %>%
    filter(show == TRUE) 
    # %>%
    # arrange(desc(date))
  
  return(df)
                
}

##| --------------------------------------------
##| Plot Functions
##| --------------------------------------------

createPlotGiving <- function(df_trans2) {

  df_trans3 <- select(df_trans2, cum_year_giving, cum_year_giving_str, yday, year, date_str)
  
  df <- df_trans3[!duplicated(df_trans3[c("yday","year")], fromLast = T),]

  n <- nPlot(cum_year_giving ~ yday, data = df, group = "year", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Year')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_year_giving_str + '</b></p>'
        }
        !#")

  return(n)
}


createPlotTotalGivingMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    group_by(year, month) %.%
    summarize(
      giving = max(cum_month_giving)
    )
  
  grid <- expand.grid(year = unique(df_trans3$year), month = as.numeric(seq(1:12)))
  df <- merge(grid, df_trans3, by=c("year","month"), all.x = T)
  df[is.na(df)] <- 0
  df <- arrange(df, year, month)
  
  n <- nPlot(giving ~ month, data = df, group = "year", type = 'multiBarChart')
  
  unique_year <- unique(df$year)
  exclude_year <- unique_year[unique_year != tail(unique_year,1)]
  n$set(disabled = exclude_year)
  
  return(n)
}

createPlotCumGivingMonth <- function(df_trans2) {
 
  df_trans3 <- df_trans2 %>%
    mutate(
      year_mon = paste0(year,'-',month)
    ) %>%
    select(year_mon, mday, date_str, cum_month_giving, cum_month_giving_str) %>%
    arrange(date_str)
  
  df <- df_trans3[!duplicated(df_trans3[c("mday","year_mon")], fromLast = T),]
  
  n <- nPlot(cum_month_giving ~ mday, data = df, group = "year_mon", type = 'lineChart')
  n$xAxis(axisLabel = 'Day of Month')
  n$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year_mon + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + d.point.cum_month_giving_str + '</b></p>'
        }
        !#")
  
  return(n)
}

##| --------------------------------------------
##| Render UI Functions
##| --------------------------------------------

output$giving_year <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    
    year_list <- sort(unique(df_trans2$year), decreasing = T)
    year_selected <- max(year_list)

    selectizeInput(inputId = "giving_year",
                label = h4("Year:"),
                choices = year_list,
                multiple = TRUE,
                selected = year_selected)
  }
})

output$giving_month <- renderUI({
  
  if (is.null(input$file_transactions)) {
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    
    month_list <- sort(unique(df_trans2$month))
    month_selected <- seq(month(today()), month(today())-2)
    
    selectizeInput(inputId = "giving_month",
                   label = h4("Month:"),
                   choices = month_list,
                   multiple = TRUE,
                   selected = month_selected)
  }
  
})

##| --------------------------------------------
##| Render Output Functions
##| --------------------------------------------

output$plot_giving_yoy <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    n <- createPlotGiving(df_trans2)  
  }
  
  return(n)  
}) 

output$plot_total_giving_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    n <- createPlotTotalGivingMonth(df_trans2)  
  }
  
  return(n)
}) 

output$plot_cum_giving_month <- renderChart2({    
  
  if (is.null(input$file_transactions)) {
    n <- nPlot(y ~ x, data = data.frame(x=1,y=2), type = "scatter")
  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    df3 <- filterGiving(df_trans2)
    n <- createPlotCumGivingMonth(df3)  
  }
  
  return(n)
})  

output$table_giving <- renderDataTable({

  if (is.null(input$file_transactions)) {

  } else {
    df_trans <- getData()
    df_trans2 <- crunchDataGiving(df_trans)
    df3 <- filterGiving(df_trans2)
    
    df4 <- df3 %>% 
      select(date , Description, Amount, Category) %>%
      filter(!(is.na(Description)))

    colnames(df4) <- c('Date' , 'Description', 'Amount', 'Category')
    return(df4)
  }
})
