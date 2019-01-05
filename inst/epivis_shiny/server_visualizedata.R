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
  
  visItem<-datavis$visIndividual[[input$visDataSet]]
  
  #browser()
  plotClass<-strsplit(visItem$plotClass,"-")[[1]][1] #temporary, replace later
  chartType<-strsplit(visItem$plotClass,"-")[[1]][2] #temporary, replace later
  
  chartInfoSpecific<-base::Filter(Negate(is.null), chartInfo[[chartType]]) 
  
  #automatically generate the interface elements
  uiOut<-tagList(column(7,
                        if(plotClass == "grid"){
                          if(chartType == "chartType"){
                            shiny::textOutput("gridTextTemp")
                          }else{
                            shiny::plotOutput("gridPlot")
                          }
                        }else{
                          leaflet::leafletOutput("mapPlot")
                        }
                        ),
                 column(5,
                        uiOutput("visEnhanceButton"),
                        lapply(names(chartInfoSpecific),function(x){
                          markElement<-chartInfoSpecific[[x]]$markElement
                          #conditional below is temporary
                          if(length(markElement)==1){
                            if(markElement %in% c("point","line","area")){
                              x<-paste(markElement,x,sep="_")
                            }
                              uiOutput(paste(x,"UI",sep="_"))
                          }
                        })
                        )
                 )
  uiOut
})


#Modify plots if the user chooses to enhance the plot
observeEvent(input$redraw,{
  visItem<-datavis$visIndividual[[input$visDataSet]]
  
  #List of elements to enhance visual appearance
  enhanceList<-sapply(entityNames,function(x){
    if(is.na(x))
      return(NULL)
    
    return(getOptions(input[[paste(x,"selected",sep="_")]]))
    
    })

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


# Automatically generate refender functionz on start
# areaFill = getOptions(input$areaFill),
# pointFill = NULL,
# pointColour = NULL,
# pointTransparency = NULL,
# lineTransparency = NULL,
# areaTransparency = NULL,
# pointSize = NULL,
# lineWidth = NULL,
# timeOne = NULL,
# timeTwo = NULL,
# xPos = NULL,#tabular data only
# yPos = NULL, #tabular data only
# visVars = getOptions(input$visVarsSelected) #tabular data only

#UI ELEMENTS THAT THE USER CAN MODIFY FOR CHARTS
observe({
  elementNames<-c("visVars","area_fill")
  lapply(elementNames,function(x){
    output[[paste0(x,"_UI")]]<-renderUI({
      
      if(is.null(input$visDataSet))
        return(NULL)
      
      #no check, just grab the table columns
      visItem<-datavis$visIndividual[[input$visDataSet]]
      id<-as.character(unique(visItem$source$dataSource))
      
      if(any(visItem$datCat ==c("phyloTree","spatial","genomic")))
        return(NULL)
      
      tmp<-inputDataValues$allObj[[id]]@data[[1]]
      
      #check to see if there are already selected vis variables associated with the vis item
      selected<-NULL
      if(!is.null(visItem$enhancements)){
        selected<-unname(unlist(visItem$enhancements[[x]]))
        optionsList <- NULL
      }else{
        optionsList<- list(
          placeholder = 'Please type to select variables to visualize',
          onInitialize = I('function() { this.setValue(""); }')
        )
      }
      
      #UI element
      selectizeInput(inputId = paste0(x,"_selected"),
                     label = "Choose variables to visualize",
                     choice = colnames(tmp),
                     selected=selected,
                     multiple=TRUE,
                     options = optionsList)
    })
  })
})

