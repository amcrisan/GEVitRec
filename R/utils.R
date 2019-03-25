`+.uneval` <- function(a,b) {
  `class<-`(modifyList(a,b), "uneval")
}

#internal functions checks field details
#used in data harmonization
check_field<-function(entity_graph,objMeta,obj){
  entity_graph_table<-as_tibble(entity_graph)
  field_info<-apply(entity_graph_table,1,function(x,meta,obj){
    if(!is.na(x[6]))
      return(x[6])
    
    if(x[4] == "dataSource")
      return(NA)
    
    get_field_details(field = x[1],
                      field_type = x[2],
                      datSource = x[3],
                      obj = obj,
                      meta = meta)
  },meta = objMeta,obj=obj)
  
  return(field_info)
}

#internal function to get object name
#used in data harmonization
check_name<-function(entity_graph){
  entity_graph_table<-as_tibble(entity_graph)
  
  envName<-entity_graph_table$dataEnvName
  empty_idx<-which(is.na(envName))
  
  tmp<-entity_graph_table[empty_idx,]$dataSource
  
  source_idx<-which(tmp == entity_graph_table$name)
  env_tmp<-entity_graph_table[source_idx,]$dataEnvName
  
  envName[empty_idx]<-env_tmp
  
  return(envName)
}