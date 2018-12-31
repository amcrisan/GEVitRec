devtools::load_all()
library(ape)
library(dplyr)
library(ggplot2)
library(ggtree)
library(shiny)
library(shinyFiles)
library(stringr)

#library(epivis) #this will load all the analytic functions in the R subfolder

source("utils/data_load_support.R") #additional functions that support input of data to shiny app
source("utils/data_vis_support.R") #additiona functions that support data visualization in shiny app

liveStatus<-TRUE #testing code for data input functions. eventually should be removed
dataDict<-readxl::read_xlsx(path="data_dictionaries/universal_data_dictionary.xlsx")
chartSupport<-readxl::read_xlsx(path="utils/supported_charts.xlsx")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) #kill the app when the browser is closed
  
  source("server_inputdata.R",local=TRUE)
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
    visIndividual = NULL
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

    selectizeInput("visDataSet",
                   label="Choose a dataset",
                   choices=names(datavis$visIndividual),
                   selected=NULL,
                   multiple=FALSE)
  })
  
  #Dynamic generation of ui output functions
  #depending upon dropdown choice
  output$indiVis<-renderUI({
    if(is.null(input$visDataSet))
      return(NULL)
    
    uiOut<-NULL
    
    datItem<-datavis$visIndividual[[input$visDataSet]]$plotClass
    
    if(datItem == "js"){
      uiOut<-tagList(leafletOutput("mapPlot"))
    }else{
      uiOut<-tagList(shiny::plotOutput("gridPlot"))
    }
    
    uiOut
    
  })
  
  #Render functions for the individual plot types
  #Note that this is done by plot type
  #So everything that's grid graphics gets farmed to 'renderPlot'
  #Special plot types from HTML widgets also get their own specific render functions
  
  # Render : Grid plot
  output$gridPlot<-renderPlot({
    if(is.null(input$visDataSet))
      return(NULL)
    
    visItem<-datavis$visIndividual[[input$visDataSet]]
    
    if(visItem$plotClass != "grid")
      return(NULL)
    
    visItem$plot
  })
  
  # Render : Leaflet map
  output$mapPlot<-renderLeaflet({
    if(is.null(input$visDataSet))
      return(NULL)
    
    visItem<-datavis$visIndividual[[input$visDataSet]]
    
    if(visItem$plotClass != "js")
      return(NULL)
    
    visItem$plot
  })
  

  #---------------------------------------
  # Creating a summary data visualization
  #---------------------------------------
  #TO DO

})