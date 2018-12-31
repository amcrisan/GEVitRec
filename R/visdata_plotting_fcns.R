#************************************************************
#
# FUNCTIONS FOR VISUALIZING DATA
#
# Eventually, this will be replaced by the gevitR package
# and all these functions will do is create the specifications
#*************************************************************
chooseVisualization<-function(dat=NULL,objData = NULL,enhanceList=NULL){
  datCat<-unique(dat$dataCategory)

  if(length(datCat)>1) stop("Something is wrong in the data category")
  #NOTES TO SELF:
  #right now, assuming that temporal data is coming from a tabular source
  #in the future, people might just want to import a timeline they've already
  #generated and linke them to others, so maybe consider supporting that
  pList<-switch(datCat,
                "spatial" = plotMap(dat,objData),
                "phyloTree"= plotPhyloTree(dat,objData),
                "temporal" = plotTimeline(dat,objData,enhanceList),
                NULL)
  
  return(pList)
  
}

#***********************************************
# FUNCTION FOR PLOTTING A MAP AND ADDING LAYERS
#***********************************************
plotMap<-function(dat = NULL,objData = NULL){
  #create a base chart
  baseMap<-leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      position = 'topright', 
      width = 200, height = 200,
      toggleDisplay = FALSE)
  
  #add polygons for spatial data first
  shapeFiles<-dplyr::filter(dat,dataType == "spatial")
  
  if(nrow(shapeFiles)>0){
    for(id in as.character(shapeFiles$dataID)){
      tmp<-sf::as_Spatial(objData[[id]]@data$geometry)
      baseMap<-baseMap %>% addPolygons(data = tmp)
    }
  }
  
  #add markers if there are markers to add
  coordVars<-dplyr::filter(dat,stringr::str_detect(dataType,"longitude|latitude"))
  if(nrow(coordVars)>0){
    # make sure that both latitude and longitude varaibles are present
    # also make sure that they are in the same table
    for(tab in as.character(unique(coordVars$dataSource))){
      tmp<-dplyr::filter(coordVars,dataSource == tab)
      if(nrow(tmp) == 2 & all(sort(as.character(tmp$dataID)) == c("latitude","longitude"))){
        tabDat<-objData[[tab]]@data[[1]][,c("latitude","longitude")]
        baseMap<-baseMap %>% 
          addCircleMarkers(data = tabDat,
                           clusterOptions = markerClusterOptions())
      }
      
    }
  }
  
  pList<-c()
  pList[["spatial"]] <- list(source = dat,
                      plot = baseMap,
                      plotClass = "js")
  return(pList)
  
}

#***********************************************
# FUNCTION FOR PLOTTING PHYLOGENETIC TREE
#***********************************************
plotPhyloTree<-function(dat = NULL,objData = NULL){
  #Right now, trees are not combineable
  #So if multiple trees are supplied, multiple trees are visualized
  
  pList<-c()
  if(nrow(dat)>1){
    for(i in 1:nrow(dat)){
      #call yo self!
      id<-as.character(dat[i,]$dataID)
      pList[[id]]<-plotPhyloTree(dat[i,],objData[id])[[1]]
    }
    return(pList)
  }else{
    id<-as.character(dat$dataID)
    treePlot<-ggplot2::ggplot(objData[[id]]@data[[1]],aes(x=x,y=y)) +
      ggtree::geom_tree()+
      ggtree::theme_tree()
    
    pList[[id]]<-list(source = dat,
                      plot = treePlot,
                      plotClass = "grid")
    return(pList)
  }
}

#***********************************************
# FUNCTION FOR PLOTTING A GENOMIC MAP
#***********************************************
plotGenomeMap<-function(dat = NULL, objData = NULL){
  print("IMPLEMENT SOON")
}

#***********************************************
# FUNCTION FOR PLOTTING A TIMELINE
#***********************************************
plotTimeline<-function(dat = NULL,objData = NULL, enhanceList = NULL,timevar = NULL, to=NULL,from=NULL,timeline_type="epicurve"){
  #print("IMPLEMENT SOON")
  pList<-c()
  if(length(objData)>1){
    # Plot temporal data for one spreadsheet at a time
    ids<-names(objData)
    for(id in ids){
      #call yo self!
      idx<-which(dat$dataSource == id)
    
      pList[[id]]<-plotTimeline(dat[idx,],objData[id],enhanceList)[[1]]
    }
  }

  id <- as.character(unique(dat$dataSource))
  #sometimes there are multiple dates try to do some disambiguation
  #there could be some clever functions written here to combined dates
  #and figure out durations, but would get bad quickly, it should be handled at the UI layer
  if(is.null(timevar) & is.null(to) & is.null(from)){
    #user has not provided any time variables, so just randomly pick one
    #and just visualize an epicurve
    timevar<-as.character(sample(dat$dataID,1))
  }else if(!is.null(to) & !is.null(from) & is.null(timevar)){
    #change chart type to gant
    timeline_type<-"gant"
  }else if(!is.null(to) & !is.null(from) & is.null(timevar)){
    print("This is a strange scenario...")
    #favour gant chart if way too much data is provided
    timeline_type<-"gant"
  }
  
  timelinePlot<-switch(timeline_type,
         "gant" = plotGantTimeline(dat,objData,enhanceList,to,from),
         "epicurve" = plotEpicurve(dat,objData[[id]],enhanceList,timevar))
  
  
  pList[[id]]<-timelinePlot
  
  return(pList)
}

plotEpicurve<-function(dat=NULL,objData = NULL,enhanceList=NULL,timevar = NULL){
  #browser()
  tabDat<-objData@data[[1]]

  #TO: check if timevar can be converted to a date variable, need input from user
  if(is.numeric(tabDat[,timevar])){
    tabDat$epiVisTimeVar<-factor(as.character(tabDat[,timevar]),levels=sort(unique(tabDat[,timevar])))
  }else{
    tabDat$epiVisTimeVar<-timevar
  }
  
  #summarizing case counts by the time variables
  if(!is.null(enhanceList)){
    if("shapeFill" %in% names(enhanceList)){
      tab<-tabDat %>% 
        dplyr::group_by_("epiVisTimeVar",enhanceList[["shapeFill"]]) %>%
        count()
    }else{
      tab<-dplyr::count(tabDat,epiVisTimeVar)
    }
  }
    
  #base curve
  epicurve<-ggplot2::ggplot(data=tab,aes(x = epiVisTimeVar,y=n))+
    ggplot2::ylab("case counts")+
    ggplot2::xlab(timevar)+
    ggplot2::theme_bw()
    
    if(is.null(enhanceList)){
      epicurve<-epicurve + ggplot2::geom_bar(stat = "identity")
    }else{
      epicurve<-epicurve + ggplot2::geom_bar(stat = "identity",aes_string(fill = enhanceList[["shapeFill"]]))
    }

  return(list(source = dat,
       plot = epicurve,
       plotClass = "grid"))
}

#helper function to attempt to try dates
dateConv<-function(dateVal = NULL, dateSep =NULL,dateOrder=NULL){
  if(is.null(dateSep) & is.null(dateOrder))
    return(dateVal)
}
