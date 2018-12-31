#----------------------------------------------------
# EPIVIS SHINY APP DATA VIS *SUPPORT* METHODS
#
# Helpder function that support visualizing the data
#----------------------------------------------------

addPlotItem<-function(datItem = NULL){
  uiOut<-div(id = "individualPlot",
             if(datItem == "js"){
               leafletOutput("mapPlot")
             }else{
               shiny::plotOutput("gridPlot")
             }
  )
}
