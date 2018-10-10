#----------------------------------------------------
# EPIVIS SHINY APP DATA INPUT METHODS
#
# This file contains all the reactive and ui elements
# for loading data files into the epivis shiny app.
# This file is added with source(..., local = TRUE)
# so variables here are accessible in server.R
#----------------------------------------------------

inputDataValues<-reactiveValues(
  numDataSources = 0
)

observeEvent(input$addDataSource,{
  inputDataValues$numDataSources<-inputDataValues$numDataSources + 1
  
  insertUI(selector = "#dataInputOptions",
           where = "afterEnd",
           ui = addUI(datType = input$datType,datNum = inputDataValues$numDataSources,datName = input$datTypeName)
           )
  
  #start observer that will remove divs
  dataInputObserve$resume()
})


#Remove data sources
dataInputObserve<-observe({
  reactOpts<-reactiveValuesToList(input)
  reactOpts<-reactOpts[grepl("removeDataSource",names(reactOpts))]
  reactOpts<-unlist(reactOpts)
  
  idxRemove<-which(reactOpts>0)
  
  #remove the data input divs
  if(length(idxRemove)>0){
    print(reactOpts)
    removeDiv<-names(reactOpts[idxRemove])
    removeDiv<-paste0("#dataSource",gsub("removeDataSource","",removeDiv))
    
    removeUI(selector = removeDiv,immediate=TRUE,multiple=TRUE)
    
  }
},suspended = TRUE)


observeEvent(input$loadData,{
  dataInputObserve$suspend()
  
  reactOpts<-reactiveValuesToList(input)
  dataPath<-reactOpts[grepl("dataSource",names(reactOpts))]
  dataType<-reactOpts[grepl("datType",names(reactOpts))]
  
  #create a data.frame that has all of the data, it's source files
  inputData<-data.frame(interalID = names(dataPath),
                        dataPath = unname(unlist(dataType)),
                        dataType = unname(unlist(dataType)))
  
  #print(inputData)
  
})


