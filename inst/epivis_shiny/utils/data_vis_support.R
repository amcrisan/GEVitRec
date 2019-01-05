#----------------------------------------------------
# EPIVIS SHINY APP DATA VIS *SUPPORT* METHODS
#
# Helpder function that support visualizing the data
#----------------------------------------------------
chartSpecs<-function(chartTypes=NULL,colVals=NULL){
  tst<-lapply(colVals,function(xVal,dat){
    dat<-dat %>%
      dplyr::select_("dataType",as.character(xVal)) %>%
      mutate(markElement := strsplit(!!sym(xVal),split=";")) %>%
      tidyr::unnest(markElement) %>%
      dplyr::select(dataType,markElement)
    
      if(sum(is.na(dat$markElement)) == nrow(dat))
        return(NULL)
    
    dat
  },dat=chartTypes)
  names(tst)<-colVals
  return(tst)
}


chartSupport<-function(inputFile = NULL){
  chartTypes<-readxl::read_xlsx(inputFile,na="NA")
  colVals<-setdiff(colnames(chartTypes),c("chartType","dataType"))
  
  tmp<-chartTypes %>% 
    group_by(chartType) %>% 
    dplyr::do(chartInfo=chartSpecs(.,colVals))
  
  #make this into a list of items
  chartTypeElements<-c()
  for(i in 1:nrow(tmp)){
    chartType<-tmp[i,"chartType"]
    chartTypeElements[[chartType$chartType]]<-tmp[i,"chartInfo"][[1]][[1]]
  }
  
  return(chartTypeElements)
}

#Helper function for auto generating UI elements
getOptions<-function(item=NULL){
  if(is.null(item))
    return(NULL)
  
  return(list(vars=item))
}

#autogenerating the enhacement list
getEntityNames<-function(chartInfo= NULL){
  entityNames<-lapply(chartInfo,function(x){
    lapply(names(x),function(y,dat){
      tmp<-dat[[y]]
      
      if(is.null(tmp))
        return(NA)
      
      tmp<-tmp %>% mutate(elemName= ifelse(stringr::str_detect(markElement,"point|line|area"),
                                           paste(markElement,y,sep="_"),
                                           y))
      
      tmp$elemName
    },dat=x)
  }) %>% unlist() %>% unique()
  
  return(entityNames)
}