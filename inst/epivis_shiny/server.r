library(shiny)
#library(epivis) #this will load all the analytic functions in the R subfolder

source("utils/data_load_support.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) #kill the app when the browser is closed
  
  #Data Input Commands
  source("server_inputdata.R",local=TRUE)

  

})