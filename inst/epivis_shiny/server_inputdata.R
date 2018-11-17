#----------------------------------------------------
# EPIVIS SHINY APP DATA INPUT METHODS
#
# This file contains all the reactive and ui elements
# for loading data files into the epivis shiny app.
# This file is added with source(..., local = TRUE)
# so variables here are accessible in server.R
#----------------------------------------------------

inputDataValues<-reactiveValues(
  numDataSources = 0,
  dataSrc = NULL
)


#Add a new UI elements whenever the user wants to add a new data source
observeEvent(input$addDataSource,{
  inputDataValues$numDataSources<-inputDataValues$numDataSources + 1
  
  #keep a record of the data that was added
  inputDataValues$dataSrc<-rbind(inputDataValues$dataSrc,
                                 c(paste0("#dataSource",inputDataValues$numDataSources),input$datType))
  
  #create a UI element so that user can add the data source
  insertUI(selector = "#dataInputOptions",
           where = "afterEnd",
           ui = addUI(datType = input$datType,datNum = inputDataValues$numDataSources,datName = input$datTypeName)
           )
  
  #start observer that will remove divs
  dataInputRemoveObserve$resume()
})


#Remove a UI element whenever the user wants to remove a data source
dataInputRemoveObserve<-observe({
  reactOpts<-reactiveValuesToList(input)
  reactOpts<-reactOpts[grepl("removeDataSource",names(reactOpts))]
  reactOpts<-unlist(reactOpts)
  
  idxRemove<-which(reactOpts>0)
  
  #remove the data input divs
  if(length(idxRemove)>0){
    removeDiv<-names(reactOpts[idxRemove])
    removeDiv<-paste0("#dataSource",gsub("removeDataSource","",removeDiv))
    
    removeUI(selector = removeDiv,immediate=TRUE,multiple=TRUE)
    
    #also remove the elements from the input data frame
    keepIdx<-which(!(inputDataValues$dataSrc[1,] %in% removeDiv))
    
    inputDataValues$dataSrc<-inputDataValues$dataSrc[keepIdx,]
    
  }
},suspended = TRUE)


#Loading all of the data
#Right now, it will load and silently die if things are wrong
#TO DO: Add error checking - espeically if no data is loaded, it's all NULL
#TO DO: Add size checking for files that are too large
observeEvent(input$loadData,{

  #get the data sources from the list of reactive elements
  reactOpts<-reactiveValuesToList(input)
  
  dataPath<-reactOpts[grepl("dataSource",names(reactOpts))]
  dataPath[sapply(dataPath, is.null)] <- NULL #get rid of those removed data elements
  IDs<-paste0("#",gsub("_Files","",names(dataPath)))
  dataPath<-sapply(dataPath,rbind) %>% t() %>% as.data.frame()
  dataPath$internalID<-IDs
  
  #finalize the input data source matrix
  inputDataValues$dataSrc<-as.data.frame(inputDataValues$dataSrc)
  colnames(inputDataValues$dataSrc)<-c("internalID","dataType")
  
  #the idea here is to load the data at the last possible minute, 
  #and avoid loading data source that are not needed
  inputDataValues$dataSrc<-dplyr::left_join(inputDataValues$dataSrc,dataPath,by="internalID")
  
  #stop listening for the data removal button
  dataInputRemoveObserve$suspend()
  
  updateTabItems(session,"sideBarMenuOptions","data_vis")
  
})


