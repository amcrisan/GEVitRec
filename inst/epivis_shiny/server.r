library(shiny)
#library(epivis) #this will load all the analytic functions in the R subfolder

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  values<-reactiveValues(
    numDataSources = 0
  )
  
  observeEvent(input$addDataSource,{
    values$numDataSources<-values$numDataSources + 1
    
    insertUI(selector = "#addDataSource",
             where = "afterEnd",
             ui = div(id = paste0("dataSource",values$numDataSources),
               fluidRow(
               column(4,
               textInput("text",label="Enter some text",value = paste0("dataSource",values$numDataSources))),
               column(5,
               selectInput("datType",
                           label = "Choose Data Type",
                           selected=NULL,
                           multiple=FALSE,
                           choices = c("Phylogenetic Tree",
                                       "Line list",
                                       "Spatial Data",
                                       "Genomic"))),
               column(2,
                      style = "margin-top: 25px;",
                      actionButton(paste0("removeDataSource",values$numDataSources),label=icon("times")))
             )))
  })
  
  #Remove data sources
  dataInputObserve<-observe({
    print("IT ME!")
    reactOpts<-reactiveValuesToList(input)
    reactOpts<-reactOpts[grepl("removeDataSource",names(reactOpts))]
    reactOpts<-unlist(reactOpts)
    
    idxRemove<-which(reactOpts>0)
    print(reactOpts)
    
    if(length(idxRemove)>0){
      removeDiv<-names(reactOpts[idxRemove])
      removeDiv<-paste0("#dataSource",gsub("removeDataSource","",removeDiv))
      
      removeUI(selector = removeDiv,immediate=TRUE,multiple=TRUE)
   
    }
  })
  
  observeEvent(input$loadData,{
    dataInputObserve$destroy()
  })

})