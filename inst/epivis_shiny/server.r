library(shiny)
library(dplyr)
library(stringr)

#library(epivis) #this will load all the analytic functions in the R subfolder

source("utils/data_load_support.R") #additional functions that support input of data to shiny app
#source("utils/data_vis_support.R") #additiona functions that support data visualization in shiny app

liveStatus<-TRUE #testing code for data input functions. eventually should be removed
dataDict<-readxl::read_xlsx(path="data_dictionaries/universal_data_dictionary.xlsx")
chartSupport<-readxl::read_xlsx(path="utils/supported_charts.xlsx")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) #kill the app when the browser is closed
  
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
  # Data input & data summaries
  #-------------------------------------
  source("server_inputdata.R",local=TRUE)

  #Data Input Summary Tables - Mainly for testing purposes
  output$dataSummaryTable<-renderTable({
    if(is.null(inputDataValues$allObjMeta))
      return(NULL)
    
    return(inputDataValues$allObjMeta)
  })
  
  #Data Visualization links
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
  # Reactive to store visualizations
  #------------------------------------
  
  visData<-reactiveValues(
    individualVis=NULL,
    summaryVis = NULL
  )
  
  #-------------------------------------
  # Visualizing individual data sources
  #-------------------------------------
  #Dropdown list of data to visualize in individual data visualization
  output$dataOptions<-renderUI({
    if(is.null(inputDataValues$allObjMeta))
      return(NULL)
    
    #show all major data types (not individual variables)
    majorDataType<-dplyr::filter(inputDataValues$allObjMeta,!grepl("^table_",dataSource))
    selectizeInput("visDataSet",
                   label="Choose a dataset",
                   choices=as.character(majorDataType$dataID),
                   selected=NULL,
                   multiple=FALSE)
  })
  
  output$indVis<-renderUI({
    if(is.null(visData$individualVis))
      return(NULL)
    
    visData$individualVis
    
  })
  
  observeEvent(input$visDataSet,{
    visData$individualVis<-p("FIRE THE MISSLES!")
  })
  
  #source("server_visualizeData.R",local=TRUE)
  
  #---------------------------------------
  # Creating a summary data visualization
  #---------------------------------------
  #TO DO

})