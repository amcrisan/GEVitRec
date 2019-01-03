#************************************************************
#
# FUNCTIONS FOR VISUALIZING DATA
#
# Eventually, this will be replaced by the gevitR package
# and all these functions will do is create the specifications
#*************************************************************
chooseVisualization<-function(dat=NULL,objData = NULL,enhanceList=NULL,datCat = NULL){

  #NOTES TO SELF:
  #right now, assuming that temporal data is coming from a tabular source
  #in the future, people might just want to import a timeline they've already
  #generated and linke them to others, so maybe consider supporting that
  pList<-switch(datCat,
                "spatial" = plotMap(dat,objData),
                "phyloTree"= plotPhyloTree(dat,objData),
                "temporal" = plotTimeline(dat,objData,enhanceList),
                "table" = plotCommonStat(dat,objData,enhanceList),
                NULL)
  
  return(pList)
  
}

#***********************************************
# FUNCTION FOR PLOTTING A MAP AND ADDING LAYERS
#***********************************************
plotMap<-function(dat = NULL,objData = NULL,enhanceList=NULL){
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
                      datCat = "spatial",
                      plot = baseMap,
                      plotClass = "js-map",
                      enhancements = enhanceList)
  return(pList)
  
}

#***********************************************
# FUNCTION FOR PLOTTING PHYLOGENETIC TREE
#***********************************************
plotPhyloTree<-function(dat = NULL,objData = NULL,enhanceList = NULL){
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
    treePlot<-ggtree::ggplot(objData[[id]]@data[[1]],aes(x=x,y=y)) +
      ggtree::geom_tree()+
      ggtree::theme_tree()
    
    pList[[id]]<-list(source = dat,
                      datCat = "phyloTree",
                      plot = treePlot,
                      plotClass = "grid-phyloTree",
                      enhancements = enhanceList)
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
  pList<-c()
  if(length(objData)>1){
    # Plot temporal data for one spreadsheet at a time
    ids<-names(objData)
    for(id in ids){
      #call yo self!
      idx<-which(dat$dataSource == id)
    
      pList[[id]]<-plotTimeline(dat[idx,],objData[id],enhanceList)[[1]]
    }
    return(pList)
  }

  id <- as.character(unique(dat$dataSource))
  #sometimes there are multiple dates try to do some disambiguation
  #there could be some clever functions written here to combined dates
  #and figure out durations, but would get bad quickly, it should be handled at the UI layer
  if(is.null(timevar) & is.null(to) & is.null(from)){
    #user has not provided any time variables, so just randomly pick one
    #and just visualize an epicurve
    timevar<-dplyr::filter(dat,dataCategory == "temporal") %>%
      sample_n(1)
    timevar<-as.character(timevar$dataID)
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
  tabDat<-objData@data[[1]]

  #TO: check if timevar can be converted to a date variable, need input from user
  if(is.numeric(tabDat[,timevar])){
    tabDat$epiVisTimeVar<-factor(as.character(tabDat[,timevar]),levels=sort(unique(tabDat[,timevar])))
  }else{
    tabDat$epiVisTimeVar<-timevar
  }
  
  #summarizing case counts by the time variables
  if(!is.null(enhanceList)){
    if("areaFill" %in% names(enhanceList)){
      tab<-tabDat %>% 
        dplyr::group_by_("epiVisTimeVar",unlist(enhanceList[["areaFill"]])) %>%
        count()
    }else{
      tab<-dplyr::count(tabDat,epiVisTimeVar)
    }
  }else{
    tab<-dplyr::count(tabDat,epiVisTimeVar)
  }
    
  #base curve
  epicurve<-ggplot2::ggplot(data=tab,aes(x = epiVisTimeVar,y=n))+
    ggplot2::ylab("case counts")+
    ggplot2::xlab(timevar)+
    ggplot2::theme_bw()
    
    if(is.null(enhanceList)){
      epicurve<-epicurve + ggplot2::geom_bar(stat = "identity")
    }else{
      epicurve<-epicurve + ggplot2::geom_bar(stat = "identity",aes_string(fill = unlist(enhanceList[["areaFill"]])))
    }

  return(list(source = dat,
       datCat = "temporal",
       plot = epicurve,
       plotClass = "grid-barchart",
       enhancements = enhanceList))
}

#helper function to attempt to try dates
dateConv<-function(dateVal = NULL, dateSep =NULL,dateOrder=NULL){
  if(is.null(dateSep) & is.null(dateOrder))
    return(dateVal)
}


#*************************************************
# FUNCTION FOR PLOTTING COMMON STATISTICAL CHARTS
#*************************************************
plotCommonStat<- function(dat = NULL,objData = NULL,enhanceList = NULL){

  pList<-c()
  #if the user has provided some variables, use a decision tree 
  #to figure out what to plot. This is pretty straight forward
  id <- as.character(unique(dat$dataSource))
  
  #if there's more than one table, visualize each separately
  #table separately (for now)
  if(length(id) > 1){
    for(idVal in id){
      #make sure to only include those variables when calling functions
      visVars<-unlist(enhanceList[["visVars"]])
      tmp<-dplyr::filter(dat,dataSource == idVal)
      enhanceTmp<-sapply(enhanceList,function(x,visVars){
        y<-intersect(unlist(x),visVars)
        if(length(y)== 0)
          return(NULL)
        return(unlist(y))
      },visVars = as.character(tmp$dataID))
      
      #call yo-self!
      pList[[idVal]]<-dplyr::filter(tmp,objData[idVal],enhanceTmp)[[1]]
    }
    return(pList)
  }
  
  #without user input, its not possible to establish what to draw
  # #in the future, we'll store some data to get a better first guess
  if(is.null(enhanceList[["visVars"]])){
    pList[[id]]<-list(source = dat,
                plot = NULL,
                datCat = "table",
                plotClass = "grid-chartType",
                enhancements = enhanceList)
    
    return(pList)
  }
  
  #if there is user input, carry on
  
  tabDat<-objData[[id]]@data[[1]]
  visVars<-unlist(enhanceList[["visVars"]])
  
  #on the off case x and y pos variables are
  #specified, but not vis vars make sure to account for that.
  #This can happen if users delete a variable in a different dialogue box
  visVars<-unique(c(visVars, enhanceList[["xPos"]],enhanceList[["yPos"]]))
  
  #Deciding what to visualize
  #** 1. How many variables has the user specified? **
  #  i - one variable (bar chart, histogram)
  #  ii - two variables (scatter plot,boxplot,category stripe)
  #  iii - more than two variables (all of the above with enhancements, heatmap)
  
  #** 2. Are the choosen variables all numeric, categorical, or mixed? **
  
  #** 3. Did the user specify x and y variables? **
  #note, when the application starts, 
  #users currently don't have the option specify x and y variables, 
  #but can do so later. EpiDRIVE tries to come up with a good vis to start
  
  # if yes - these are first class variables
  
  #if no - default priorities as follows:
  #  i - Numeric (Integer)
  #  ii - Numeric (Double)
  #  iii - Categorical (currently, order is arbitrarily assigned)
  
  dat<-dplyr::filter(dat,dataID %in% visVars)
  varTypes<-c(sum(stringr::str_detect(as.character(dat$dataType),"character|factor")),
              sum(stringr::str_detect(as.character(dat$dataType),"double|integer"))
  )
  names(varTypes)<-c("Categorical","Numeric")
  numVar<-sum(varTypes)
  
  if(numVar == 1){
    #univariate visualizations
    varType<-names(varTypes)[which(varTypes>0)]
    chartType<-univariatePlot(tabDat,visVars,varType,enhanceList)
  }else if(numVar == 2){
    #bivariate visualizations
    varType<-names(varTypes)[which(varTypes>0)]
    chartType<-bivariatePlot(tabDat,visVars,varType,enhanceList)
  }else{
    #multivariate visualizations
    varType<-names(varTypes)[which(varTypes>0)]
    chartType<-multivarablePlot(tabDat,visVars,varType,enhanceList)
  }
  
  chartType$source<-dplyr::filter(dat,dataID %in% visVars)
    
  pList[[id]]<-chartType
  
  return(pList)

}

#Common statistical charts for single variables
univariatePlot<-function(tabDat=NULL,visVars=NULL,varType = NULL,enhanceList = NULL){
  chartType<-NULL
  return(list(source = NULL,
              datCat = "table",
              plot = "univariate plot",
              plotClass = "grid-chartType",
              enhancements = enhanceList))
}

#Common statistical charts for two variables
bivariatePlot<-function(tabDat=NULL,visVars=NULL,varType = NULL,enhanceList = NULL){
  chartType<-NULL
  return(list(source = NULL,
              datCat = "table",
              plot = "bivariate plot",
              plotClass = "grid-chartType",
              enhancements = enhanceList))
}

#Common statistical charts for more than two variables
multivarablePlot<-function(tabDat=NULL,visVars=NULL,varType = NULL,enhanceList = NULL){
  chartType<-NULL
  return(list(source = NULL,
              datCat = "table",
              plot = "multivariable plot",
              plotClass = "grid-chartType",
              enhancements = enhanceList))
}