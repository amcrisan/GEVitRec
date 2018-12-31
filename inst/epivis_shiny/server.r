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
  
  #Dynamic generation of ui output functions
  #depending upon dropdown choice
  output$indiVis<-renderUI({
    if(is.null(input$visDataSet))
      return(NULL)
    
    uiOut<-NULL
    
    datavis$selectedVis<-input$visDataSet
    
    datItem<-datavis$visIndividual[[input$visDataSet]]$plotClass
    
    if(datItem == "js"){
      uiOut<-tagList(column(7,
                            leafletOutput("mapPlot")),
                     column(5,p("Stuff will go here"),
                            uiOutput("visEnhanceButton"))
      )
    }else{
      uiOut<-tagList(column(7,
                            shiny::plotOutput("gridPlot")),
                     column(5,
                            uiOutput("visEnhanceButton"),
                            uiOutput("shapeFillUI")
                            ))
    }
    
    uiOut
    
  })
  
  #Modify plots if the user chooses to enhance the plot
  observeEvent(input$redraw,{
    visItem<-datavis$visIndividual[[input$visDataSet]]
    
    enhanceList<-c(shapeFill = ifelse(is.null(input$shapeFill),NULL,input$shapeFill),
                   otherVar = NULL)
    
    #Remove empty items that are null
    enhanceList<-base::Filter(Negate(is.null), enhanceList)
    
    #only redraw if there's something to modify
    if(length(enhanceList)!=0){
      df<-visItem$source
      datSrc<-as.character(unique(df$dataSource))
      tmp<-chooseVisualization(df, inputDataValues$allObj[datSrc],enhanceList=enhanceList)
      #browser()
      datavis$visIndividual[[input$visDataSet]]$plot<-tmp[[1]]$plot
    }
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
  
  #button to modify visualization
  output$visEnhanceButton<-renderUI({
    actionButton(inputId = "redraw","Enhance Visualization!")
  })
  
  #Options functions that allow modifications of base chart type
  output$shapeFillUI<-renderUI({
    if(is.null(input$visDataSet))
      return(NULL)
    
    visItem<-datavis$visIndividual[[input$visDataSet]]
    datSrcs<-as.character(unique(visItem$source$dataSource))
    
    #check to see if it is able
    #browser()
    tmp<-inputDataValues$allObjMeta
    tmp<-dplyr::filter(tmp,dataID %in% as.character(datSrcs))%>%
      dplyr::filter(dataType == "table")
    
    if(nrow(tmp) == 0){
      return(NULL)
    }else if(nrow(tmp) > 1){
      print("Multiple data sources - something went wrong here...")
      return(NULL)
    }
    
    tmp<-inputDataValues$allObj[[as.character(tmp$dataID)]]@data[[1]]
    
    
    selectInput(inputId = "shapeFill",
                label = "Shape Fill",
                choice = colnames(tmp),
                selected=NULL,
                multiple=FALSE)
  })

  
  #---------------------------------------
  # Creating a summary data visualization
  #---------------------------------------
  #TO DO

})