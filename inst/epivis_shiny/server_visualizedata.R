#----------------------------------------------------
# EPIVIS SHINY APP DATA VISUALIZATION METHODS
#
# This file contains all the reactive and ui elements
# for generating data visualizations for the epivis shiny app.
# This file is added with source(..., local = TRUE)
# so variables here are accessible in server.R
#----------------------------------------------------


output$summaryVisualization<-renderPlot({
  generateSummaryVisualization(inputDataValues$dataSec)
})