#----------------------------------------------------
# EPIVIS SHINY APP DATA VIS *SUPPORT* METHODS
#
# Helpder function that support visualizing the data
#----------------------------------------------------

#generate the summary data visualizations
generateSummaryVisualization<-function(inputData){
  #figure out what kind of data you have
  
  #scan data to identify potentially common linking variables
  
  
  #just for tabluar data, classify some of the columns by data type
  
}


### NOTE - DECIDE NOW OR LATER WHETHER TO PROMOTE THESE FUNCTIONS TO PACKAGE LEVEL
#scan tablular data, flagg items that are
getTableItems<-function(tabDat = NULL){
  tabDatMeta<-sapply(tabDat,class) %>% stack()
  colnames(tabDatMeta)<-c("colType","colName")
  
  #now, some cleaning up to detect special words that relate to time, geography, and genomics
  #these are special classes of generally continous variables that can be used in the visualization
  #GEOGRAPHY VARIABLES
  geo_var_latlong<-c("latitude","longitude","lat","lon")
  idx<-idxVarType(geo_var_latlong,tabDatMeta$colName)
  if(!is.null(idx)) tabDatMeta[idx,]$colType<-"lat:long"
  
  #TEMPORAL VARIABLES
  time_var<-c("date","month","year","day")
  idx<-idxVarType(time_var,tabDatMeta$colName)
  if(!is.null(idx)) tabDatMeta[idx,]$colType<-"temporal"
  
  return(tabDatMeta)
}

#simple fuction to find some 
idxVarType<-function(var,listVar){
  listVar<-as.character(listVar)
  idx<-sapply(var,function(x){grepl(x,listVar)}) %>% 
    apply(.,1,any) %>% which()
  
  return(idx)
}

#a function that attempts to identify linking variables between different
#data types. Most useful when there is a table invovled.
linkingVars<-function(datDF=NULL){
  
}



