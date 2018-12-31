#******************************************************
#
# FUNCTIONS FOR VISUALIZING DATA
#
#******************************************************
chooseVisualization<-function(dat=NULL,objData = NULL){
  datCat<-unique(dat$dataCategory)
  
  if(length(datCat)>1) stop("Something is wrong in the data category")
  
  pList<-switch(datCat,
                "spatial" = plotMap(dat,objData),
                "phyloTree"= plotPhyloTree(dat,objData),
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
  #So if multiple tree are supplied, multiple trees are visualized
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

