#Code I that I won't delete just yet.. but that also is not longer needed

get_spec_list_old<-function(harmon_obj=NULL,usrChoices = NULL){
  
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
  entity_graph_table$feild_detail<-apply(entity_graph_table[,1:5],1,function(x,meta,obj){
    if(!is.na(x[5]))
      return(x[5])
    
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
  path_list<-c()
  
  for(comp in component_info$component){
    comp_info<-dplyr::filter(component_info,component == comp)
    
    #for each component, try to produce at most two
    #visualizations
    max_vis<-2
    if(comp_info$n>1){
      #the hard coded numbers here are abitrary
      max_vis<-ifelse(comp_info$n*2<10,10,comp_info$n*2)
    }
    
    #get the relevant data for each component
    comp_var<-dplyr::filter(entity_graph_table,dataEntity=="feild") %>%
      dplyr::filter(component == comp)
    
    comp_data<-dplyr::filter(entity_graph_table,dataEntity=="dataType") %>%
      dplyr::filter(component == comp)
    
    #Find paths between variables as these are essential
    #seeds for the specifications
    dats<-as.numeric(V(entity_graph)[as.character(comp_data$name)])
    dat_paths<-igraph::all_shortest_paths(entity_graph,from=dats,to=dats)$res
    
    # --- Rank paths in graph ----
    #rank paths according to the number of visualizations linked
    #and the strength of the path - favour strongly connected paths
    pathRank<-sapply(dat_paths,function(path_val,scoring_table){
      VP = as.numeric(path_val)
      EP = rep(VP, each=2)[-1]
      EP = EP[-length(EP)]
      
      #a perfectly aligned path has a weight of one, because everything is a strong connection
      strength_var<-sum(E(entity_graph)$weight[get.edge.ids(entity_graph, EP)])/(length(VP)-1)
      
      if(is.nan(strength_var)){ strength_var <- 0} #try to avoid single charts
      
      #rate the path by the high relevance chart types
      datTypes<-dplyr::filter(datOnly,dataID %in% as_ids(path_val)) %>%
        dplyr::inner_join(chart_scores,by=c("dataType" = "dataSource")) %>%
        group_by(dataID) %>%
        do(summarise(.,max(rescale)))
      
      diversity<-nrow(datTypes)
      relevance<-sum(datTypes$`max(rescale)`)
      score = (diversity*relevance)*strength_var
      
      return(c(score,strength_var,diversity,relevance))
      
    },scoring_table = chart_scores) %>% t() %>% data.frame(.,stringsAsFactors=FALSE)
    
    colnames(pathRank)<-c("score","strength","diversity","relevance")
    pathRank$pathIndex<-1:length(dat_paths)
    
    #for now - choose the top path
    #top-two paths for each component
    #consider making this more variable if path lengths are store ect..
    pathRank<- pathRank %>% arrange(desc(strength),desc(score)) %>% head(4)
    
    #convert the path into a specification for a chart or two
    for(idx in pathRank$pathIndex){
      path_var<-as_ids(dat_paths[[idx]])
      
      dats<-dplyr::filter(datOnly,dataID %in% gsub("_gevitID","",path_var))
      
      vars<-dplyr::filter(entity_graph_table,dataSource %in% as.character(dats$dataID)) %>% 
        dplyr::filter(dataEntity == "feild")
      
      required<-path_var[path_var %in% vars$name]
      
      if(!is.null(usrChoices)){
        required<-c(required,usrChoices)
      }
      
      # generate charts by data type
      # if that data type is directly connected to other
      # charts also include their variables as potential linkers
      if(nrow(vars) == 1){
        #There is just one thing, there is no metadata at all
        #Then you just return whatever data object is
        chartType<-dplyr::filter(objMeta, dataID == as.character(vars$dataSource[1]))
        spec_list<-c(comp,NA,as.character(chartType$dataType),as.character(chartType$dataID),rep(NA,5),NA)
      }
      
      #create data specifications for individual chart types
      for(datSrc in dats$dataID){
        
        req_tmp<-required
        chartType<-dplyr::filter(objMeta, dataID == as.character(datSrc))
        
        full_link<-NA
        partial_link<-NA
        
        if(as.character(chartType$dataType) !="table"){
          link_strength<-dplyr::filter(edgeList,grepl(as.character(datSrc),dataset)) %>%
            dplyr::filter(var %in% path_var)
        }else{
          tmp_var<-dplyr::filter(vars,dataSource == datSrc)
          link_strength<-dplyr::filter(edgeList,dataset %in% tmp_var$name) %>%
            dplyr::filter(var %in% path_var)
        }
        
        if(nrow(link_strength)>0){
          #when there is a prefect match free up the objects quant variables for others
          #since other positional variables will already be occupied
          full_link<-unique(as.character(dplyr::filter(link_strength,weights == 0)$var))
          partial_link<-unique(as.character(dplyr::filter(link_strength,weights != 0)$var))
          
          full_link<-ifelse(length(full_link)>0,full_link,NA)
          partial_link<-ifelse(length(partial_link)>0,partial_link,NA)
        }
        
        tmp<-dplyr::filter(vars,!grepl("gevitID",name))
        #tmp<-vars
        #if there are no variables to link on, then
        if(nrow(tmp)==0){
          spec_list<-c(comp,
                       idx,
                       as.character(chartType$dataType),
                       as.character(chartType$dataID),
                       c(NA,NA,NA,NA,NA),
                       full_link,
                       partial_link)
        }else{
          
          all_links<-unique(c(partial_link,full_link,req_tmp))
          all_links<-all_links[!is.na(all_links)]
          
          n_quant<-dplyr::filter(tmp,feild_detail %in% c("quant","qual-many")) %>% filter(name %in% all_links)
          n_qual<-dplyr::filter(tmp,!feild_detail %in% c("quant","qual-many")) %>% filter(name %in% all_links)
          
          # if(all(!is.na(full_link)) & as.character(chartType$dataType) !="table"){
          #   tmp<-dplyr::filter(tmp,!(name %in% full_link))
          # }
          # 
          # if(all(!is.na(partial_link))){
          #   #prioritize highly connected nodes
          #   #even if they are not perfect matches
          #   all_links<-unique(c(partial_link,full_link,req_tmp))
          # 
          #   n_quant<-dplyr::filter(tmp,feild_detail %in% c("quant","qual-many")) %>% filter(name %in% all_links)
          #   n_qual<-dplyr::filter(tmp,!feild_detail %in% c("quant","qual-many")) %>% filter(name %in% all_links)
          
          if(nrow(n_quant)<3 & nrow(n_quant)>0){
            n_quant_extra<-dplyr::filter(tmp,feild_detail %in% c("quant","qual-many")) %>%
              anti_join(n_quant)
            if(nrow(n_quant_extra)>(3-nrow(n_quant))){
              n_quant_extra<-n_quant_extra %>% dplyr::sample_n(3-nrow(n_quant))
              n_quant<-rbind(n_quant,n_quant_extra)
            }
          }
          
          if(nrow(n_qual)<2 & nrow(n_qual)>0){
            n_qual_extra<-dplyr::filter(tmp,!feild_detail %in% c("quant","qual-many")) %>%
              anti_join(n_qual)
            if(nrow(n_qual_extra)>(2-nrow(n_qual))){
              n_qual_extra<-n_qual_extra %>%dplyr::sample_n(2-nrow(n_qual))
              n_qual<-rbind(n_qual,n_qual_extra)
            }
          }
          
          tmp<-rbind(n_quant,n_qual)
          #}
          
          total_quant =  tmp %>% dplyr::filter(feild_detail %in% c("quant","qual-many")) %>% count()
          total_qual = tmp %>% dplyr::filter(!feild_detail %in% c("quant","qual-many")) %>% count()
          
          #Finally, generate a more complex set of specifications
          if(total_quant$n > 0 | total_qual$n>0){
            spec_list<-assign_vars(datFeilds = tmp,
                                   require_var = req_tmp,
                                   n_quant=ifelse(total_quant$n>3,3,total_quant$n),
                                   n_qual=ifelse(total_qual$n>2,2,total_qual$n))
            
            rep_len<-ifelse(is.null(dim(spec_list)),1,nrow(spec_list))
            full_linkage<-ifelse(all(!is.na(full_link)),paste(full_link,collapse=","),NA)
            partial_linkage<-ifelse(all(!is.na(full_link)),paste(partial_link,collapse=","),NA)
            
            
            if(rep_len == 1){
              if(!is.null(spec_list)){
                spec_list<-c(comp,
                             idx,
                             as.character(chartType$dataType),
                             as.character(chartType$dataID),
                             spec_list,
                             full_linkage,
                             partial_linkage)
              }
            }else{
              spec_list<-cbind(rep(comp,rep_len),
                               rep(idx,rep_len),
                               rep(as.character(chartType$dataType),rep_len),
                               rep(as.character(chartType$dataID,rep_len)),
                               spec_list,
                               rep(as.character(full_linkage,rep_len)),
                               rep(as.character(partial_linkage,rep_len)))
            }
          }
        }
        
        spec_list_all<-rbind(spec_list_all,spec_list)
      }
      
      tmp_list<-data.frame(graph_component = comp,
                           graph_path = idx,
                           path = paste(path_var,collapse = ","),
                           pathRank[idx,],
                           stringAsFactors = FALSE)
      
      path_list<-rbind(path_list,tmp_list)
    }
  }
  spec_list_all<-data.frame(graph_component = spec_list_all[,1],
                            graph_path = spec_list_all[,2],
                            dataType = spec_list_all[,3],
                            dataSource = spec_list_all[,4],
                            quant_1 =spec_list_all[,5],
                            quant_2=spec_list_all[,6],
                            quant_3=spec_list_all[,7],
                            qual_1 =spec_list_all[,8],
                            qual_2 = spec_list_all[,9],
                            full_linkage = spec_list_all[,10],
                            partial_linkage = spec_list_all[,11],
                            stringsAsFactors = FALSE)
  
  spec_list_all$graph_path<-sapply(spec_list_all$graph_path,function(x){
    if(is.na(x)){
      return(1)
    }else{
      return(x)
    }})
  
  return(list(paths = path_list,specs = spec_list_all))
}

clean_up_spec_old<-function(path_specs = NULL,top_n = 10){
  spec_list<-path_specs$specs
  path_info<-path_specs$paths
  meta<-harmon_obj[["dataMeta"]]
  
  #small clean up
  path_info$graph_component<-as.character(path_info$graph_component)
  path_info$graph_path<-as.character(path_info$graph_path)
  
  if(top_n > 20){
    print("You don't need more than 20 charts. I'm cutting you off at 20")
    top_n<-20
  }
  #sometimes, the spec list is itself so massive that
  #it still must be reduced to a few more reasonable options
  #overall, if there are ten visualizations to produced for each component
  #that is the most important
  # count_charts<-spec_list %>%
  #   group_by(graph_component,graph_path) %>%
  #   count() %>%
  #   ungroup() %>%
  #   inner_join(path_info[,c("graph_component","graph_path","diversity","score")])
  # 
  # n_charts<-sum(count_charts$n)
  
  #there are still way too many charts
  #further summarize ny giving the user the top 10
  allSpecs<-c()
  spec_idx<-1
  for(comp in unique(spec_list$graph_component)){
    for(pathVal in unique(spec_list$graph_path)){
      chart_specs<- dplyr::filter(spec_list,graph_component==comp & graph_path == pathVal)
      if(nrow(chart_specs) == 0){
        next
      }
      single_specs<-c()
      for(i in 1:nrow(chart_specs)){
        single_chart<-chart_specs[i,]
        aes_vars<-chart_specs[i,5:9]
        
        var_info<-match(aes_vars,meta$dataID)
        var_info<-meta[var_info,]
        
        #ordering variables
        var_info<-var_info %>%
          mutate(var_value = ifelse(!is.na(as.numeric(gsub("qual-","",feild_detail))),
                                    as.numeric(gsub("qual-","",feild_detail)),
                                    13))
        #see if there's anything that's in the combo_variables and put that first
        if(!is.na(chart_specs$full_linkage)){
          link_pos<-which(var_info$dataID %in% chart_specs$full_linkage)
          var_info[link_pos,]$var_value<-14
        }
        
        var_info<-arrange(var_info,desc(var_value)) #sort in order
        
        #assign thiese to postion, size, shape, and colour
        quant<-c("pos1","pos2","size")
        qual<-c("color","shape")
        tmp2<-c()
        
        for(j in 1:nrow(var_info)){
          if(is.na(var_info[j,]$feild_detail)){
            tmp2<-tmp2<-c(tmp2,NA)
          }else if(var_info[j,]$feild_detail %in% c("quant","qual-many")){
            if(length(quant)>0){
              tmp2<-c(tmp2,quant[1])
              quant<-setdiff(quant,quant[1])
            }
          }else{
            if(length(qual)>0){
              assign_qual<-qual[1]
              if(assign_qual == "shape"){
                if(as.numeric(gsub("qual-","",var_info[j,]$feild_detail))<6){
                  tmp2<-c(tmp2,assign_qual)
                  qual<-setdiff(qual,qual[1])
                } 
              }else{
                tmp2<-c(tmp2,assign_qual)
                qual<-setdiff(qual,qual[1])
              }
              
            }
          }
        } #end of j for loop
        
        var_info$aes_assign<-tmp2
        
        #single chart specs
        chart_spec<-list(data = single_chart$dataSource,
                         chartType = single_chart$dataType,
                         aes = var_info[,c("dataID","dataSource","aes_assign")])
        
        single_specs[[single_chart$dataSource]]<-chart_spec
      }
      
      allSpecs[[spec_idx]]<-list(single_chart_specs=single_specs,
                                 combo = list(
                                   data = names(single_specs),
                                   full_link = unique(chart_specs$full_linkage),
                                   partial_link = unique(chart_specs$partial_linkage)
                                 )
      )
      spec_idx <- spec_idx + 1
    }
  }
  
  return(allSpecs)
  
}


filter_combo_old<-function(combos,require_var=NULL){
  if(is.null(require_var))
    return(combos)
  
  req_present<-apply(combos,2,function(x,req_var){
    sum(req_var %in% x)
  },req_var = require_var)
  
  n_req<-nrow(combos) #default for combn output
  
  if(length(require_var)<n_req){ n_req <-length(require_var)}
  
  idx_keep<-req_present>=n_req
  return(combos[,idx_keep])
}

assign_vars_old<-function(data = NULL,datFeilds = NULL,require_var = NULL,n_quant=3,n_qual=2){
  #high_degree vars have priority, because they can
  #faciliate combinations with other data
  connect_node<-dplyr::filter(datFeilds, degree>1) %>%
    dplyr::arrange(desc(degree)) %>%
    head(5)  
  
  require_var<-unique(c(require_var,connect_node$name))
  
  quant<-dplyr::filter(datFeilds,feild_detail %in% c("quant","qual-many"))
  qual<-dplyr::filter(datFeilds,!(feild_detail %in% c("quant","qual-many")))
  
  quant_require<- if (!is.null(require_var)) intersect(quant$name,require_var) else NULL
  qual_require<- if (!is.null(require_var)) intersect(qual$name,require_var) else NULL
  
  #a bit exaggerated because it doesn't take require variales into account
  n_combos<- choose(length(qual),n_qual) * choose(length(qual),n_quant)
  
  #and goo..
  if(n_combos>1000){
    stop("Haven't implemented this yet")
  }
  
  combo_quant<-filter_combo(combn(quant$name,m=n_quant),require_var = quant_require)
  combo_qual<-filter_combo(combn(qual$name,m=n_qual),require_var = qual_require)
  
  #generate specifications
  use_quant<-check_status(combo_quant)
  use_qual<-check_status(combo_qual)
  
  
  spec_list<-NULL
  if(use_quant & use_qual){
    spec_list<-join_combos(combo_quant,combo_qual,n_quant,n_qual)
  }else if(use_quant){
    spec_list<-single_specs(combo_quant,"quant")
  }else if(use_qual){
    spec_list<-single_specs(combo_qual,"qual")
  }
  
  return(spec_list)
}

check_status_old<-function(combo_val){
  use_combo<-FALSE
  if(is.null(dim(combo_val))){
    if(length(combo_val)>0){
      use_combo<-TRUE
    }
  }else{
    use_combo<-TRUE
  }
  return(use_combo)
}

single_specs_old<-function(combo_one,combo_type=NULL){
  if(is.null(nrow(combo_one))){
    
    if(combo_type == "quant"){
      vec_val<-rep(NA,3)
      vec_val[1:length(combo_one)]<-combo_one
      vec_val<-c(vec_val,NA,NA)
    }else{
      vec_val<-rep(NA,2)
      vec_val[1:length(combo_one)]<-combo_one
      vec_val<-c(NA,NA,NA,vec_val)
    }
    return(vec_val)
  }else{
    if(combo_type == "quant"){
      spec_val<-rbind(combo_one,
                      rep(NA,ncol(combo_one)),
                      rep(NA,ncol(combo_one)))
    }else{
      spec_val<-rbind(rep(NA,ncol(combo_one)),
                      rep(NA,ncol(combo_one)),
                      rep(NA,ncol(combo_one)),
                      combo_one)
    }
    return(t(spec_val))
  }
  
}


join_combos_old<-function(combo_one,combo_two,n_combo_one,n_combo_two){
  #TO DO: Address the situation where  one combo is only a vector
  #tmp1<-combo_one
  #tmp2<-combo_two
  
  combo_one<-as.matrix(t(combo_one),ncol=3)
  combo_two<-as.matrix(t(combo_two),ncol=2)
  
  spec_list<-c()
  for(i in 1:nrow(combo_one)){
    x<-combo_one[i,]
    for(j in 1:nrow(combo_two)){
      y<-combo_two[j,]
      quant_vec<-rep(NA,3)
      qual_vec<-rep(NA,2)
      
      if(length(x)>0){quant_vec[1:length(x)]<-x}
      if(length(y)>0){qual_vec[1:length(y)]<-y}
      spec<-c(quant_vec,qual_vec)
      spec_list<-rbind(spec_list,spec)
    }
  }
  
  return(unname(spec_list))
  # Apply doesn't work great when combo_one or combo_two is literally just one
  # row, so this is a better solution
  # apply(combo_one,1,function(x,combo_two,n_combo_one,n_combo_two){
  #   print("yay!")
  #   spec<-apply(combo_two,1,function(y,x,n_combo_one,n_combo_two){
  #     quant_vec<-rep(NA,3)
  #     qual_vec<-rep(NA,2)
  # 
  #     if(length(x)>0){quant_vec[1:length(x)]<-x}
  #     if(length(y)>0){qual_vec[1:length(y)]<-y}
  # 
  #     spec<-c(quant_vec,qual_vec)
  #     list(spec)
  #   },x = x,n_combo_one= n_combo_one,n_combo_two = n_combo_two)
  #   spec<-unlist(spec,recursive = FALSE)
  # },combo_two=combo_two,n_combo_one= n_combo_one,n_combo_two = n_combo_two) %>%
  #   lapply(.,function(x){
  #     do.call(rbind,x)
  #   }) %>% do.call(rbind,.)
}
