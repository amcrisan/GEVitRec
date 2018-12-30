devtools::load_all()
library(ape)
library(dplyr)
library(ggplot2)
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
  
  output$tableOptions<-renderUI({
    if(is.null(input$visDataSet) | is.null(inputDataValues$allObj))
      return(NULL)
    
   #Did the user select a table?
    dataVis<-input$visDataSet
    itemVis<-dplyr::filter(inputDataValues$allObjMeta,dataID == dataVis)
    
    optionUI<-NULL
    if(as.character(itemVis$dataType)=="table"){
      #for tables, get the possible variables to visualize
      visOptions<-dplyr::filter(inputDataValues$allObjMeta,dataSource == dataVis)$dataID %>% as.character()
      optionUI<-div(
        column(4,
               HTML("<p>Please choose what variables you want to visualize and the genepi DRIVE engine will take it from there. If you want the DRIVE enginge to come up with what it judges to be the (up to) five best charts for you data than leave the box below empty and select the 'visualize it!' button.</p>")
               ),
        column(8,
               selectizeInput("tabVisChoices",
                              label="Create visualizations with the following variables:",
                              choices = visOptions,
                              selected=NULL,
                              multiple=TRUE,
                              width='35%'),
               actionButton("excelsior","visualize it!",width='35%')
               )
        )
      
    }else{
      optionUI<-div(actionButton("excelsior","visualize it!"))
    }
    return(optionUI)
  })
  
  output$indVis<-renderPlot({
    if(is.null(visData$individualVis))
      return(NULL)
    
    visData$individualVis
    
  })
  
  observeEvent(input$excelsior,{
    visData$individualVis<-p("I AM A THING")
    if(!is.null(input$tabVisChoices)){
      visData$individualVis<-plot_decider(input$visDataSet,
                                          inputDataValues$allObj,
                                          inputDataValues$allObjMeta,
                                          input$tabVisChoices)
    }else{
      visData$individualVis<-plot_decider(input$visDataSet,inputDataValues)
    }
  })
  

  #source("server_visualizeData.R",local=TRUE)
  
  #---------------------------------------
  # Creating a summary data visualization
  #---------------------------------------
  #TO DO

})