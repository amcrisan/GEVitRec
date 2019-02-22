#' Returning ranked paths through entity graph
#' @title rank_paths
#' @param dat_paths 
#' @param entity_graph 
#' @param datOnly 
#'
#' @return
rank_paths<-function(dat_paths=NULL,entity_graph=NULL,datOnly=NULL){
  
  path_rank<-sapply(dat_paths,function(path_val,entity_graph){
    VP = as.numeric(path_val)
    EP = rep(VP, each=2)[-1]
    EP = EP[-length(EP)]
    
    #a perfectly aligned path has a weight of one, because everything is a strong connection
    strength_var<-sum(E(entity_graph)$weight[get.edge.ids(entity_graph, EP)])/(length(VP)-1)
    
    #calculate diversity : number of different data types in the path
    #calculate relevance : using chart scores from gevit, emphasize the relevant visualizations
    datTypes<-dplyr::filter(datOnly,dataID %in% as_ids(path_val)) %>%
      dplyr::inner_join(chart_scores,by=c("dataType" = "dataSource")) %>%
      group_by(dataID) %>%
      do(summarise(.,max(rescale)))
    
    diversity<-nrow(datTypes)
    relevance<-sum(datTypes$`max(rescale)`)
    c(strength_var,diversity,relevance)
  },entity_graph = entity_graph) %>% t()
  
  path_rank_summary_score<-apply(path_rank,2,function(x){
    #higher values are given a higher rank
    #objective is to maximize strength, diversity, and relevance
    rank(x) 
  }) %>% rowSums()
  
  return(list(path_rank = path_rank, summary_rank = path_rank_summary_score))
}

#' Generate a chart specification by data type
#' @title get_all_chart_specs
#' @param vars 
#' @param dats 
#' @param required_var 
#'
#' @return
#' @examples
get_all_chart_specs<-function(vars=NULL,dats=NULL,required_var=NULL){
  
  all_specs<-c()
  
  for(dat in dats$dataID){
    vis_feilds<-dplyr::filter(vars,dataSource == dat)
    dat_type<-dplyr::filter(dats,dataID == dat)$dataType
    
    #the required variables *within this specific dataset
    required_tmp<-intersect(required_var,vis_feilds$name)
    
    if(dat == "table"){
      chart_specs<-get_charts_specs(vis_feilds,required_tmp,dat_type)
      all_specs[[dat]]<-chart_specs
    }else{
      #remove ID objects
      vis_feilds<-dplyr::filter(vis_feilds,!grepl("_gevitID",name))
      chart_specs<-get_charts_specs(vis_feilds,required_tmp,dat_type)
      all_specs[[dat]]<-chart_specs
    }
  }
  
  #return all specifications
  return(all_specs)
}

clean_up_spec<-function(all_specs = NULL,max_comp_spec=20){
  print("Cleaning specifications up")
  #browser()
  comp_only<-sapply(names(all_specs),function(x){strsplit(x,"_")[[1]][1]})
  for(comp in unique(comp_only)){
    idx_comp<-which(grepl(comp,names(all_specs)))
  }
}

convert_to_mincombinr<-function(){
 #create a mincombir compatible specification
 print("Generating mincombinr spec")
}
