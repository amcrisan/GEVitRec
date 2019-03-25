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


#' get_vis_fields
#' use fields from datasets with which there are exact linkages
#' @param fields 
#' @param datSource 
#' @param edgeList 
#' @param path_var 
#'
#' @return
#'
#' @examples
get_vis_fields<-function(fields=NA,datSource = NA,edgeList=NA,path_var=NA,browse = FALSE){
  #check if this is just a single component
  if(browse){
    browser()
  }
  
  datSrcNum<-which(path_var %in% unique(fields$dataSource))
  if(length(datSrcNum)==1){
    #single dataset, so just show the fields related to that dataset
    fields_use<-dplyr::filter(fields, dataSource %in% datSource)
    return(fields_use)
  }
  
  #find exact matches within the data set
  linkStrength<-dplyr::filter(edgeList,dataset %in% path_var) %>%
    dplyr::filter(dataset %in% fields$name & var %in% fields$name)
  
  if(sum(as.numeric(linkStrength$weights)) == 0){
    #all datasets are linked, so the full specturm of
    #so use all the possible fields to make a specification
    return(fields)
  }else{

    link_idx<-which(linkStrength$weights == 0)
    
    #if there are no matches, only use fields from a single data source
    if(length(link_idx) == 0){
      fields_use<-dplyr::filter(fields, dataSource %in% datSource)
      return(fields_use)
    }
    
    link_tmp<-unique(unlist(linkStrength[link_idx,1:2]))
    
    if(length(link_tmp) > 2){
      #not dealing with this scenario just yet
      fields_use<-dplyr::filter(fields, dataSource %in% datSource)
      return(fields_use)
    }else if(length(link_idx)==2){
      #use only certain fields to make a specification
      linked_field<-unique(unlist(linkStrength[link_idx,1:2]))
      datSrc<-dplyr::filter(fields,name %in% linked_field)$dataSource
      fields_use<-dplyr::filter(fields,dataSource %in% datSrc)
      return(fields_use)
    }
  }
}

#' Generate a chart specification by data type
#' @title get_all_chart_specs
#' @param vars 
#' @param dats 
#' @param required_var 
#'
#' @return
#' @examples
get_all_chart_specs<-function(vars=NULL,dats=NULL,required_var=NULL,edgeList = NULL, path_var=NULL){
 # load("inst/recommender_examples/example_entity_gen.rda")
  all_specs<-c()
  
  #Find highly connected nodes, use those
  #to seed all specifications based upon the linkage strength
  
  #all exact matches: 
  #spatial align, or color align
  #all other matches:
  #unaligned or small multiples
  
  for(dat in dats$dataID){
    #vis_fields<-dplyr::filter(vars,dataSource == dat)
    vis_fields<-get_vis_fields(vars,dat,edgeList,path_var)
    dat_type<-dplyr::filter(dats,dataID == dat)$dataType
    #dat_env_name<-dplyr::filter(dats,dataID == dat)$dataEnvName
    
    #the required variables *within this specific dataset
    required_tmp<-intersect(required_var,vis_fields$name)
    
    #remove ID objects
    if(is.null(vis_fields)){
      browser()
      get_vis_fields(vars,dat,edgeList,path_var,TRUE)
    }
    vis_fields<-dplyr::filter(vis_fields,!grepl("_gevitID",name))
    chart_specs<-get_charts_specs(vis_fields,required_tmp,dat_type,dat)
    all_specs[[dat]]<-chart_specs
    
    # if(dat == "table"){
    #   #chart_specs<-get_charts_specs(vis_fields,required_tmp,dat_type,dat_env_name)
    #   chart_specs<-get_charts_specs(vis_fields,required_tmp,dat_type,dat)
    #   all_specs[[dat]]<-chart_specs
    # }else{
    #   #remove ID objects
    #   vis_fields<-dplyr::filter(vis_fields,!grepl("_gevitID",name))
    #   #chart_specs<-get_charts_specs(vis_fields,required_tmp,dat_type,dat_env_name)
    #   chart_specs<-get_charts_specs(vis_fields,required_tmp,dat_type,dat)
    #   all_specs[[dat]]<-chart_specs
    # }
  }
  
  #return all specifications
  return(all_specs)
}

clean_up_spec<-function(all_specs = NULL,entity_graph_table=NULL,max_comp_spec=20){
  print("Cleaning specifications up")
  spec_names<-names(all_specs)
  comp_only<-sapply(spec_names,function(x){strsplit(x,"_")[[1]][1]})

  total_vis<-c()

  for(comp in unique(comp_only)){
    idx_comp<-which(grepl(comp,spec_names))
    comp_vis<-all_specs[idx_comp]
    comp_names<-names(all_specs)
    path_idx<-1:length(comp_vis)

    #The visualizations in EACH path are co-ordinated together
    #Because there is some linking variable between them
    for(idx in path_idx){
      
      charts<-comp_vis[[idx]]$spec
      #these charts can have 
  
      path_vis<-c()
      for(i in 1:length(charts)){
        for(j in 1:length(charts[[i]])){
          path_vis[[paste(i,j,sep="-")]]<-charts[[i]][[j]]
        }
      }
      
      #hard build in - show how five charts per path
      #these charts will be shown (for now) in the many
      #types linked configuration
      
      priorities<-sapply(path_vis,function(x){
        x1<-ifelse(is.na(x$relvance),0,x$relvance)
        x2<-ifelse(is.na(x$fields_total) | x$fields_total ==0,0,x$fields_total)
        
        #weird .. that I had to use sum and that + didn't work
        return(sum(as.numeric(x1),as.numeric(x2)))
        
      }) %>% sort(.,decreasing=TRUE)
      
      if(length(priorities) > 5){
        keep_list<-match(names(priorities[1:5]),names(path_vis))
        path_vis<-path_vis[keep_list]
        priorities<-priorities[1:5]
      }
      
      path_vis_single_specs<-lapply(path_vis,function(x){
        base_spec<-convert_to_mincombinr(x$spec,entity_graph_table)
      })
      
      #store the results, for now, just make a many types general
      #combo until do a slightly more sophisticated filtering
      total_vis[[comp_names[idx]]][["single_charts"]]<-path_vis_single_specs
      total_vis[[comp_names[idx]]][["view_score"]]<-sum(priorities)
    }
  }
  
  return(total_vis)
}

convert_to_mincombinr<-function(rough_spec=NULL,entity_graph_table=NULL){
  #create a mincombir compatible specification
  print("Generating mincombinr spec")
  
  datSrcs<-lapply(rough_spec,function(chan){
    if(class(chan) == "data_info"){
      return(chan$datSrc)
    }
    if(class(chan) == "channel_info"){
        return(chan$dataSource )
    }
  }) %>% unlist() %>% unname() %>% unique()
  
  #print(datSrcs)
  datSrcs<-datSrcs[!is.na(datSrcs)]
  meta_all<-c()
  
  if(length(datSrcs)>1){
    envObj<-dplyr::filter(entity_graph_table,name %in% datSrcs)$dataEnvName
    
    for(envItem in envObj){
      #set and initital value
      #for the metadata
      tmp<-get(envItem,envir = globalenv())
      if(is.null(meta_all)){
        if(tmp@type == "table"){
          meta_all<-tmp@data[[1]]
        }
        if(!is.null(tmp@data$metadata)){
          meta_all<-tmp@data$metadata
        }
      }else{
        if(tmp@type == "table"){
          meta_all<-dplyr::full_join(meta_all,tmp@data[[1]])
        }else{
          if(!is.null(tmp@data$metadata)){
            meta_all<-dplyr::full_join(meta_all,tmp@data$metadata)
          }
        }
      }
    }
  }
  
  #get more detailed data source information
  datSrc<-rough_spec$data$datSrc
  envObj<-dplyr::filter(entity_graph_table,name == datSrc)
  
  #modify environment object with data
  if(!is.null(meta_all)){
    if(class(envObj$dataType) !="table"){
      envItem<-get(envObj$dataEnvName,envir=globalenv())
      envItem@data$metadata<-meta_all
      
      #currently overwriting the global env metadata
      #but something that I might want to change
      #if I find a more efficient data structure
      assign(as.character(envObj$dataEnvName),envItem,envir = globalenv())
    }
  }
  
  #find out if there is more than one data source
  #if there is (meaning that there is an exact match)
  #then, modify the metadata or data as necessary
  
  #for some reason, minCombinR has an empty slot
  #TO DO: fix that minCombinR bug
  spec_tmp<-list("",
                 chart_type = rough_spec$chart_type,
                 data = envObj$dataEnvName)
  chan_tmp<-lapply(rough_spec,function(chan){
    if(class(chan) == "channel_info"){
      return(chan$field )
    }
  })
  
  chan_tmp<-base::Filter(Negate(is.null), chan_tmp)
  
  base_spec<-append(spec_tmp,chan_tmp)
  class(base_spec)<-c("list","gevitSpec","baseSpecs")
  
  return(base_spec)
  
}
