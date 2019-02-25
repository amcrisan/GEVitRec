get_spec_list<-function(harmon_obj = NULL, usrChoices=NULL){
  print("Generating possible specifications")
 
  # --- Prep input data ----
  objMeta<-harmon_obj[["dataMeta"]]
  obj<-harmon_obj[["dataObj"]]
  entity_graph<-harmon_obj[["entityGraph"]]
  
  edgeList<-dplyr::filter(harmon_obj[["edgeList"]],link_type == "var-var")
  rev_edge<-cbind(edgeList[,2],edgeList[,1],edgeList[,3:4])
  names(rev_edge)<-names(edgeList)
  edgeList<-rbind(edgeList,rev_edge)
  
  datOnly<-dplyr::filter(objMeta,dataEntity == "dataType")
  
  E(entity_graph)$weight<-1-as.numeric(E(entity_graph)$weights)
  
  entity_graph_table<-as_tibble(entity_graph)
  
  #fill in those missing variables for the ID
  entity_graph_table$feild_detail<-apply(entity_graph_table[,1:6],1,function(x,meta,obj){
    if(!is.na(x[6]))
      return(x[6])
    
    if(x[4] == "dataType")
      return(NA)
    
    get_feild_details(feild = x[1],
                      feild_type = x[2],
                      datSource = x[3],
                      obj = obj,
                      meta = meta)
  },meta = objMeta,obj=obj)
  
  # --- Identify graph components, build specs for each ----
  #number of data types per component
  component_info<- entity_graph_table %>%
    dplyr::filter(dataEntity == "dataType") %>%
    dplyr::group_by(component) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n))
  
  spec_list_all<-c()

  for(comp in component_info$component){
 
    comp_info<-dplyr::filter(component_info,component == comp)
    
    #get all of the data in that component
    comp_data<-dplyr::filter(entity_graph_table,dataEntity=="dataType") %>%
      dplyr::filter(component == comp)
    
    if(nrow(comp_data)==1){
      #if there's just a single data type, just make that chart
      dats<-dplyr::filter(datOnly,dataID %in% comp_data$name)
      
      vars<-dplyr::filter(entity_graph_table,dataSource %in% as.character(dats$dataID)) %>% 
        dplyr::filter(dataEntity == "feild")
      
      all_chart_specs<-get_all_chart_specs(vars,dats,usrChoices)
      chart_id<-sprintf("comp%d_path1",comp)
      #clean_up_spec(all_chart_specs)
      
      spec_list_all[[chart_id]]$specs<-all_chart_specs
      next
    }
    
    #Find paths between variables as these are essentially
    #seeds for the specifications
    dats<-as.numeric(V(entity_graph)[as.character(comp_data$name)])
    all_paths<-list()
    for(source in dats){
      # DEV NOTE:
      #all shortest paths seems to have some randomness to it that is not
      #stable. Better to use all simple paths and do the rank here.
      dat_paths<-igraph::all_simple_paths(entity_graph,from=source,to=setdiff(dats,source))
      all_paths<-append(all_paths,dat_paths)
    }
    
    #rank these paths
    rank_summary<-rank_paths(all_paths,entity_graph,datOnly)
    #arbitrarily, keep the top ten paths
    max_vis<-10
    if(nrow(rank_summary$path_rank)<10){
      max_vis<-nrow(rank_summary$path_rank)
    }
    
    #higher scores are favourable
    path_keep<-order(rank_summary$summary_rank,decreasing = TRUE)[1:max_vis]
    
    # --- Generate specifications ----
    path_count<-1
    for(idx in path_keep){
      path_var<-as_ids(all_paths[[idx]])
      
      #all the data types in that path
      dats<-dplyr::filter(datOnly,dataID %in% gsub("_gevitID","",path_var))
      
      #all possible variables associated with that data type
      vars<-dplyr::filter(entity_graph_table,dataSource %in% as.character(dats$dataID)) %>% 
        dplyr::filter(dataEntity == "feild")
      
      #variables that must appear because they are
      #are on the 'critical path' or because the user
      #has specificed them
      required_var<-path_var[path_var %in% vars$name]
      
      if(!is.null(usrChoices)){
        required_var<-c(required_var,usrChoices)
      }
      
      #Now create some specifications:
      #each data source can produce one or more charts
      #and the seed specifications for these charts
      #are based upon the require specifications
      all_path_specs<-get_all_chart_specs(vars,dats,required_var)
      
      chart_id<-sprintf("comp%d_path%d",comp,path_count)
      spec_list_all[[chart_id]]$specs<-all_path_specs
      path_count<-path_count+1
    }
  }
  
  #clean up the final spec list
  spec_list_all<-clean_up_spec(spec_list_all)
  
  return(spec_list_all)
}