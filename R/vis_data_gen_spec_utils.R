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


filter_combo<-function(combos,require_var=NULL){
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

assign_vars<-function(data = NULL,datFeilds = NULL,require_var = NULL,n_quant=3,n_qual=2){
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

check_status<-function(combo_val){
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

single_specs<-function(combo_one,combo_type=NULL){
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


join_combos<-function(combo_one,combo_two,n_combo_one,n_combo_two){
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
