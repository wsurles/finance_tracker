
options(RCHART_WIDTH = 1000)

shinyServer(function(input, output, session) {

  source('lib/functions_upload_data.R', local = T)

  callModule(moduleCashFlowYear, "year", getDataTrans, getDataCategoryDim, getDataDates)
  callModule(moduleCashFlowQuarter, "quarter", getDataTrans, getDataCategoryDim, getDataDates)
  callModule(moduleCashFlowMonth, "month", getDataTrans, getDataCategoryDim, getDataDates)
  callModule(moduleCashFlowCategory, "category", getDataTrans, getDataCategoryDim, getDataDates)


})
