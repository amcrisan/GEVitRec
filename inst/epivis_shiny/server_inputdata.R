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
  
  inputDataValues$dataSrc<-dplyr::left_join(inputDataValues$dataSrc,dataPath,by="internalID")
  
  #stop listening for the data removal button
  dataInputRemoveObserve$suspend()
  
  #---------------------------------------------------------------------
  # DATA STRUCTURE TO RULE THEM ALL
  # Puts all the data into three data structures for further processing
  #---------------------------------------------------------------------
  #this is when the data finally gets loaded into the system
  allObj<-makeGEVITRobj(dataSrc = inputDataValues$dataSrc,liveStatus = liveStatus)
  
  allObjMeta<-data.frame(dataID = sapply(allObj,function(x){x@id}),
                         dataType= sapply(allObj,function(x){x@type}),
                         dataSource = sapply(allObj,function(x){x@source}))
  
  #-------- LOAD IN AND READ DATA ------------------
  # 1. READ THE INFORMATION OUT OF THE TABLES (IF ANY)
  #load necessary data dictionary
  if(any(allObjMeta$dataType == "table"))
    tabScanned<-scanTab(objData=allObj,objMeta=allObjMeta,dataDict=dataDict)
  
  # 2. FIND LINKS BETWEEN DIFFERENT DATA OBJECTS
  varComp<-findLink(allObj=allObj,allObjMeta=allObjMeta)
  
  #3. ADD TABLE DATA TO OBJECT META
  #browser()
  if(any(allObjMeta$dataType == "table")){
    #if there are tables, add scanned tabluar data to table
    #treat each column as an individual vector varaible
    
    dataType <- apply(tabScanned[,c("class","specialClass")],1,function(x){
      ifelse(is.na(x[2]),x[1],paste(x[1],x[2],sep=";"))
    })
    
    tabScanned<-data.frame(dataID=tabScanned$variable,
                           dataType= dataType,
                           dataSource = tabScanned$tableSource)
    
    allObjMeta<-rbind(allObjMeta,tabScanned)
  }
  
  #4. CLEAN AND FINESSE

  #give each list item the name of its objects to make it easier to retrieve
  names(allObj)<-sapply(allObj,function(x){x@id})
  
  #separate between data types and variables types
  allObjMeta <- mutate(allObjMeta,
                       dataEntity = ifelse(dataType %in% c("spatial","phyloTree","dna","table"), 
                                            "dataType",
                                            ifelse(stringr::str_detect(dataType,";"),"specialVariableType","variableType")))
  
  allObjMeta<-allObjMeta %>%
    filter(dataEntity %in% c("dataType","specialVariableType")) %>%
    mutate(dataCategory = sapply(dataType,function(x){
      strVal<-stringr::str_split(x,";")[[1]]
      if(length(strVal) == 1){
        return(as.character(x))
      }else{
        if(strVal[2] %in% c("latitude","longitude")){
          return("spatial")
        }else{
          return(strVal[2])
        }
      }}))
  
  
  #Assignment to reactive variables and removal of extra stuff
  inputDataValues$allObj<-allObj
  inputDataValues$allObjMeta<-allObjMeta
  inputDataValues$varComp<-varComp
  
  
  #-------- PREP DATA VIS OBJECT ------------------

  allVis<-c()
  for(dt in unique(allObjMeta$dataCategory)){
    
    if(!(dt %in% c("spatial","phyloTree","temporal")))
      next()
    
    df<-filter(allObjMeta,dataCategory== dt)
    
    #source of relevant variable tabular data
    varSrc<-filter(df,stringr::str_detect(dataType,";")) %>% 
      select(dataSource) %>% 
      unique()
    
    #ids of data types
    datSrc<-filter(df,!stringr::str_detect(dataType,";")) %>% 
      select(dataID) %>% 
      unique()
    
    datSrc<-c(as.character(varSrc[,1]),as.character(datSrc[,1]))
    
    #remove any NA's
    datSrc<-datSrc[!is.na(datSrc)]
    
    tmp<-chooseVisualization(df, allObj[datSrc])
    
    if(length(tmp) == 1){
      allVis[[dt]]<-tmp[[1]]
    }else{
      #for things like phylogenetic trees, common statistical charts, ect. that
      #are one function call but that must be separate views
      for(i in 1:length(tmp)){
        allVis[[as.character(tmp[[i]]$source$dataID)]] <-tmp[[i]]
      }
    }
  }
  
  datavis$visIndividual<-allVis

  #### NOW UPDATE TO THE VISUALIZATION TAB
  updateTabItems(session,"sideBarMenuOptions","data_vis")
  
})


