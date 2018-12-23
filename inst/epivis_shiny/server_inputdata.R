#----------------------------------------------------
# EPIVIS SHINY APP DATA INPUT METHODS
#
# This file contains all the reactive and ui elements
# for loading data files into the epivis shiny app.
# This file is added with source(..., local = TRUE)
# so variables here are accessible in server.R
#----------------------------------------------------


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
  #get the list of reactive elements
  reactOpts<-reactiveValuesToList(input)
  
  #find those elements tagged with "remove data source", and clean it up a little
  reactOpts<-reactOpts[grepl("removeDataSource",names(reactOpts))]
  
  if(length(reactOpts)>1){
    reactOpts<-data.frame(id = paste0("#d",gsub("removeD","",names(reactOpts))),
                     removeValue = unname(sapply(reactOpts,function(x){x})),
                     stringsAsFactors = FALSE)
    
    #identify elements for removal
    idxRemove<-which(reactOpts$removeValue>0)
    
    #remove the data input divs
    if(length(idxRemove)>0){
      #actually remove the elmements from the UI 
      removeDiv<-as.character(reactOpts[idxRemove,"id"])
      removeUI(selector = removeDiv,immediate=TRUE,multiple=TRUE)
      
      #also remove the elements from the input data frame
      keepIdx<-which(!(inputDataValues$dataSrc[,1] %in% removeDiv))
      inputDataValues$dataSrc<-inputDataValues$dataSrc[keepIdx,]
    }
  }
},suspended = TRUE)


#Loading all of the data
#Right now, it will load and silently die if things are wrong
#TO DO: Add error checking - espeically if no data is loaded, it's all NULL
#TO DO: Add size checking for files that are too large
observeEvent(input$loadData,{

  #browser()
  #get the data sources from the list of reactive elements
  reactOpts<-reactiveValuesToList(input)

  #cleaning up the data to sort out what's actually been loaded into the system
  dataPath<-reactOpts[grepl("dataSource",names(reactOpts))]
dataPath<-dplyr::bind_rows(dataPath,.id = toString(names(dataPath)))
  
  dataPath$internalID<-paste0("#",gsub("_Files","",dataPath[,1]))
  
  #finalize the input data source matrix
  inputDataValues$dataSrc<-as.data.frame(inputDataValues$dataSrc)
  colnames(inputDataValues$dataSrc)<-c("internalID","dataType")
  
  #the idea here is to load the data at the last possible minute, 
  #and avoid loading data source that are not needed
  inputDataValues$dataSrc<-dplyr::left_join(inputDataValues$dataSrc,dataPath,by="internalID")
  
  #stop listening for the data removal button
  dataInputRemoveObserve$suspend()
  
  #---------------------------------------------------------------------
  # DATA STRUCTURE TO RULE THEM ALL
  # Puts all the data into three data structures for further processing
  #---------------------------------------------------------------------
  
  allObj<-makeGEVITRobj(dataSrc = inputDataValues$dataSrc,liveStatus = liveStatus)
  
  allObjMeta<-data.frame(dataID = sapply(allObj,function(x){x@id}),
                         dataType= sapply(allObj,function(x){x@type}),
                         dataSource = sapply(allObj,function(x){x@source}))
  
  # 1. READ THE INFORMATION OUT OF THE TABLES (IF ANY)
  #load necessary data dictionary
  #browser()
  tabScanned<-scanTab(objData=allObj,objMeta=allObjMeta,dataDict=dataDict)
  
  # 2. FIND LINKS BETWEEN DIFFERENT DATA OBJECTS
  varComp<-findLink(allObj=allObj,allObjMeta=allObjMeta)
  
  #3. LAST STEP : ADD TABLE DATA TO OBJECT META
  dataType = apply(tabScanned[,c("class","specialClass")],1,function(x){
    ifelse(is.na(x[2]),x[1],paste(x[1],x[2],sep=";"))
    })
  
  tabScanned<-data.frame(dataID=tabScanned$variable,
                         dataType= dataType,
                         dataSource = tabScanned$tableSource)
  
  allObjMeta<-rbind(allObjMeta,tabScanned)
  
  #Assignment to reactive variables and removal of extra stuff
  inputDataValues$allObj<-allObj
  inputDataValues$allObjMeta<-allObjMeta
  inputDataValues$varComp<-varComp
  
  #remove(c("allObj","allObjMeta","varComp"))
  #gc()
  
  #### NOW UPDATE TO THE VISUALIZATION TAB
  
  updateTabItems(session,"sideBarMenuOptions","data_vis")
  
})


