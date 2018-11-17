library(shiny)
library(dplyr)
library(stringr)

#library(epivis) #this will load all the analytic functions in the R subfolder

source("utils/data_load_support.R") #additional functions that support input of data to shiny app
#source("utils/data_vis_support.R") #additiona functions that support data visualization in shiny app

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) #kill the app when the browser is closed
  
  #Data Input Commands
  source("server_inputdata.R",local=TRUE)
  
  #Data Visualization Commands
  #source("server_visualizeData.R",local=TRUE)
  

})