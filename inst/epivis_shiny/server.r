devtools::load_all()
library(ape)
library(dplyr)
library(ggplot2)
library(ggtree)
library(shiny)
library(stringr)

#library(epivis) #this will load all the analytic functions in the R subfolder

source("utils/data_load_support.R") #additional functions that support input of data to shiny app
source("utils/data_vis_support.R") #additional functions that support data visualization in shiny app

liveStatus<-TRUE #testing code for data input functions. eventually should be removed
dataDict<-readxl::read_xlsx(path="data_dictionaries/universal_data_dictionary.xlsx")

#identifies the enhancements for each chart type
chartInfo<-chartSupport("data_dictionaries/charts_and_enhancements.xlsx")
entityNames<-getEntityNames(chartInfo)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) #kill the app when the browser is closed
  
  #Eventually, swap this modules
  source("server_inputdata.R",local=TRUE)
  source("server_visualizedata.R",local=TRUE)
  
  #***************************************************************
  #
  # 0. Load in all data, store it in a data structure
  #
  #***************************************************************
  
  #-------------------------------------
  # Reactive values to store input data
  #------------------------------------
  inputDataValues<-reactiveValues(
    numDataSources = 0,
    dataSrc = NULL,
    allObj=NULL,
    allObjMeta = NULL,
    varComp = NULL
  )
  
  #-------------------------------------
  # Reactive values to store visualizations
  #------------------------------------
  datavis<-reactiveValues(
    visIndividual = NULL,
    selectedVis = NULL
  )
  
  
  #-------------------------------------
  # Data input & data summaries
  #-------------------------------------
  #Data Input Summary Tables - Mainly for testing purposes
  output$dataSummaryTable<-renderTable({
    if(is.null(inputDataValues$allObjMeta))
      return(NULL)
    
    return(inputDataValues$allObjMeta)
  })
  
  #Data Visualization summary and linkages
  output$dataLinkageTable<-renderTable({
    if(is.null(inputDataValues$varComp))
      return(NULL)
    
    return(inputDataValues$varComp)
  })
  
  #***************************************************************
  #
  # 1. Visualize that data!
  #
  #***************************************************************

  #-------------------------------------
  # Visualizing individual data sources
  #-------------------------------------
  #Dropdown list of data to visualize in individual data visualization
  output$visOptions<-renderUI({
    if(is.null(datavis$visIndividual))
      return(NULL)
    #browser()
    selectedItem<-NULL
    if(!is.null(datavis$selectedVis))
      selectedItem<-datavis$selectedVis
    
    selectizeInput("visDataSet",
                   label="Choose a dataset",
                   choices=names(datavis$visIndividual),
                   selected=selectedItem,
                   multiple=FALSE)
  })
  
  
  
  
  #---------------------------------------
  # Creating a summary data visualization
  #---------------------------------------
  #TO DO

})