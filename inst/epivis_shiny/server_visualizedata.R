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
  enhanceList<-list(areaFill = getOptions(input$areaFill),
                    pointFill = NULL,
                    pointColour = NULL,
                    pointTransparency = NULL,
                    lineTransparency = NULL,
                    areaTransparency = NULL,
                    pointSize = NULL,
                    lineWidth = NULL,
                    xPos = NULL,#tabular data only
                    yPos = NULL, #tabular data only
                    visVars = getOptions(input$visVarsSelected) #tabular data only
  )
  
  #Remove empty items that are null
  enhanceList<-base::Filter(Negate(is.null), enhanceList)
  
  #only redraw if there's something to modify
  if(length(enhanceList)!=0){
    df<-visItem$source
    datSrc<-as.character(unique(df$dataID))
  
    datCat<-visItem$datCat

    if(datCat %in% c("temporal","table")){
      datSrc<-as.character(unique(df$dataSource))
      df<- dplyr::filter(inputDataValues$allObjMeta,dataSource == datSrc)
    }
  
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
  
  if(stringr::str_detect(visItem$plotClass,"chartType"))
    return(visItem$plot)
  
  return(NULL)
})

#-----------------------------------------
# OPTIONS FOR ENHANCING THE APPEARANCE
#-----------------------------------------
#Options functions that allow modifications of base chart type

#UI ELEMENT : Selecting variables to fill an area
output$areaFillUI<-renderUI({
  if(is.null(input$visDataSet))
    return(NULL)
  
  visItem<-datavis$visIndividual[[input$visDataSet]]
  datSrcs<-as.character(unique(visItem$source$dataSource))
  
  #check to see if it is table
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
 
   #check to see if there are already selected vis variables associated with the vis item
  selected<-NULL
  if(!is.null(visItem$enhancements)){
    selected<-unname(unlist(visItem$enhancements[['visVars']]))
    optionsList <- NULL
  }else{
    optionsList<- list(
      placeholder = 'Please type to select variables to visualize',
      onInitialize = I('function() { this.setValue(""); }')
    )
  }
  
  
  selectizeInput(inputId = "areaFill",
              label = "Shape Fill",
              choice = colnames(tmp),
              selected=selected,
              multiple=FALSE,
              options = optionsList)
})


# UI ELEMENT : Selecting variables to visualize in table data
output$variableSelector<-renderUI({
  if(is.null(input$visDataSet))
    return(NULL)
  
  #no check, just grab the table columns
  visItem<-datavis$visIndividual[[input$visDataSet]]
  id<-as.character(unique(visItem$source$dataSource))
  
  if(visItem$datCat !="table")
    return(NULL)
  
  tmp<-inputDataValues$allObj[[id]]@data[[1]]
  
  #check to see if there are already selected vis variables associated with the vis item
  selected<-NULL
  if(!is.null(visItem$enhancements)){
    selected<-unname(unlist(visItem$enhancements[['visVars']]))
    optionsList <- NULL
  }else{
    optionsList<- list(
      placeholder = 'Please type to select variables to visualize',
      onInitialize = I('function() { this.setValue(""); }')
    )
  }

  #UI element
  selectizeInput(inputId = "visVarsSelected",
              label = "Choose variables to visualize",
              choice = colnames(tmp),
              selected=selected,
              multiple=TRUE,
              options = optionsList)
})

