#----------------------------------------------------
# EPIVIS SHINY APP DATA VISUALIZATION METHODS
#
# This file contains all the reactive and ui elements
# for generating data visualizations for the epivis shiny app.
# This file is added with source(..., local = TRUE)
# so variables here are accessible in server.R
#----------------------------------------------------
#-----------------------------------------
# UI ELEMENTS FOR VISUALIZATIONS
#-----------------------------------------

#Dynamic generation of ui output functions
#depending upon dropdown choice
output$indiVis<-renderUI({
  if(is.null(input$visDataSet))
    return(NULL)
  
  uiOut<-NULL
  
  # for tables, it's impossible to come up
  # a visualization until the user provides some
  # variables. If consulting regularily
  # this could be annoying, so in the long
  # run, providing a way to assess the commonly
  # used variables and generating those visualizations
  # automatically is the way to go. This could be done
  # by storing some data in the app...
  datavis$selectedVis<-input$visDataSet
  
  visItem<-datavis$visIndividual[[input$visDataSet]]$plotClass
  
  if(stringr::str_detect(visItem,"js")){
    #Java Script
    uiOut<-tagList(column(7,
                          leafletOutput("mapPlot")),
                   column(5,p("Stuff will go here"),
                          uiOutput("visEnhanceButton"))
    )
  }else{
    #Standard Grid Graphics
    if(stringr::str_detect(visItem,"chartType")){
      uiOut<-tagList(column(7,
                            shiny::textOutput("gridTextTemp")),
                     column(5,
                            uiOutput("visEnhanceButton"),
                            uiOutput("variableSelector")
                     ))
    }else{
      uiOut<-tagList(column(7,
                            shiny::plotOutput("gridPlot")),
                     column(5,
                            uiOutput("visEnhanceButton"),
                            uiOutput("areaFillUI")
                     ))
    }
  }
  
  uiOut
  
})


#Modify plots if the user chooses to enhance the plot
observeEvent(input$redraw,{
  visItem<-datavis$visIndividual[[input$visDataSet]]
  
  #List of elements to enhance visual appearance
  enhanceList<-list(areaFill = ifelse(is.null(input$areaFill),NULL,input$areaFill),
                    pointFill = NULL,
                    pointColour = NULL,
                    pointTransparency = NULL,
                    lineTransparency = NULL,
                    areaTransparency = NULL,
                    pointSize = NULL,
                    lineWidth = NULL,
                    xPos = NULL,#tabular data only
                    yPos = NULL, #tabular data only
                    visVars = ifelse(is.null(input$visVarsSelected),NULL,list(vars=input$visVarsSelected)) #tabular data only
  )
  
  #Remove empty items that are null
  enhanceList<-base::Filter(Negate(is.null), enhanceList)
  
  #only redraw if there's something to modify
  if(length(enhanceList)!=0){
    df<-visItem$source
    datSrc<-as.character(unique(df$dataSource))
  
    datCat<-dplyr::filter(inputDataValues$allObjMeta,dataID %in% datSrc) %>%
      select(dataCategory) %>%
      unique()
    
    datCat<-as.character(datCat$dataCategory)
    
    #multiple data sources
    if(length(datCat)>1){
      stop("There's a problem here, there are too many categoies")
    }
    
    if(datCat == "table"){
      if(!is.null(enhanceList[["visVars"]]))
      datavis$tableVarSelected<-unlist(enhanceList[["visVars"]])
      
      if(length(datSrc)>1){
        print("Something weird here in table land")
      }
      df<- dplyr::filter(inputDataValues$allObjMeta,dataSource == datSrc)
    }
    
    #browser()
    tmp<-chooseVisualization(dat=df, objData = inputDataValues$allObj[datSrc],enhanceList=enhanceList,datCat = datCat)
    datavis$visIndividual[[input$visDataSet]]<-tmp[[1]]
  }
})

#button to modify visualization
output$visEnhanceButton<-renderUI({
  actionButton(inputId = "redraw","Enhance Visualization!")
})


#-----------------------------------------
# RENDERING FUNCTIONS FOR DIFFERENT VIS
#-----------------------------------------
#Render functions for the individual plot types
#Note that this is done by plot type
#So everything that's grid graphics gets farmed to 'renderPlot'
#Special plot types from HTML widgets also get their own specific render functions

# Render : Grid plot
output$gridPlot<-renderPlot({
  if(is.null(input$visDataSet))
    return(NULL)
  
  visItem<-datavis$visIndividual[[input$visDataSet]]
  
  if(stringr::str_detect(visItem$plotClass,"js") | stringr::str_detect(visItem$plotClass,"chartType"))
    return(NULL)
  
  visItem$plot
  
})

# Render : Leaflet map
output$mapPlot<-renderLeaflet({
  if(is.null(input$visDataSet))
    return(NULL)
  
  visItem<-datavis$visIndividual[[input$visDataSet]]
  
  if(stringr::str_detect(visItem$plotClass,"grid"))
    return(NULL)
  
  visItem$plot
})

# Render : temporary while working out common statistical charts
output$gridTextTemp<-renderText({
  if(is.null(input$visDataSet))
    return(NULL)
  
  visItem<-datavis$visIndividual[[input$visDataSet]]
  
  #browser()
  if(stringr::str_detect(visItem$plotClass,"chartType"))
    return(visItem$plot)
  
  return(NULL)
})

#-----------------------------------------
# OPTIONS FOR ENHANCING THE APPEARANCE
#-----------------------------------------
#Options functions that allow modifications of base chart type
output$areaFillUI<-renderUI({
  if(is.null(input$visDataSet))
    return(NULL)
  
  visItem<-datavis$visIndividual[[input$visDataSet]]
  datSrcs<-as.character(unique(visItem$source$dataSource))
  
  #check to see if it is table
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
  
  
  selectInput(inputId = "areaFill",
              label = "Shape Fill",
              choice = colnames(tmp),
              selected=NULL,
              multiple=FALSE)
})

output$variableSelector<-renderUI({
  if(is.null(input$visDataSet))
    return(NULL)
  
  selected<-NULL
  if(!is.null(datavis$tableVarSelected))
    selected<-datavis$tableVarSelected
  
  #no check, just grab the table columns
  visItem<-datavis$visIndividual[[input$visDataSet]]
  id<-as.character(unique(visItem$source$dataSource))
  
  tmp<-inputDataValues$allObj[[id]]@data[[1]]
  
  selectInput(inputId = "visVarsSelected",
              label = "Choose variables to visualize",
              choice = colnames(tmp),
              selected=selected,
              multiple=TRUE)
})

