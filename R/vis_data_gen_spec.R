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

get_feild_details<-function(feild = NULL,feild_type = NULL,datSource = NULL,obj=NULL,meta){
  if(any(c("double","integer") %in% feild_type)){return("quant")}
  
  dat_tmp<-NULL
  index<-which(objMeta$dataID == datSource)
  
  if(grepl("table",datSource)){
    dat_tmp<-obj[[index]]@data[[1]]
    dat_tmp<-dat_tmp[,feild]
  }else if (!is.null(obj[[index]]@data$metadata)){
    if(grepl("gevitID",feild)){
      dat_tmp<-returnItemData(index,obj,meta)
    }else{
      dat_tmp<-obj[[index]]@data$metadata
      dat_tmp<-dat_tmp[,feild]
    }
  }else{
    dat_tmp<-returnItemData(index,obj,meta)
  }
  
  if(is.null(dat_tmp)){return("ERROR!")}
  
  var<-unique(dat_tmp)
  if(length(var)>12){
    return("qual-pos")
  }else{
    return(paste("qual",length(var),sep="-"))
  }
}

assign_vars<-function(data = NULL,datFeilds = NULL,require_var = NULL,n_quant=3,n_qual=2){
  #high_degree vars have priority, because they can
  #faciliate combinations with other data
  connect_node<-dplyr::filter(datFeilds, degree>1) %>%
    dplyr::arrange(desc(degree)) %>%
    head(5)  
  
  require_var<-c(require_var,connect_node$name)
  
  quant<-dplyr::filter(datFeilds,feild_detail %in% c("quant","qual-pos"))
  qual<-dplyr::filter(datFeilds,!(feild_detail %in% c("quant","qual-pos")))
  
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
    quant_spec<-single_specs(combo_quant,"quant")
    spec_list<-c(quant_spec,NA,NA)
  }else if(use_qual){
    quant_spec<-single_specs(combo_quant,"qual")
    spec_list<-c(NA,NA,NA,qual_spec)
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
    }else{
      vec_val<-rep(NA,2)
      vec_val[1:length(combo_one)]<-combo_one
    }
    return(vec_val)
  }
  
  return(NULL)
}

join_combos<-function(combo_one,combo_two,n_combo_one,n_combo_two){
  #TO DO: Address the situation where  one combo is only a vector
  apply(combo_one,2,function(x,combo_two,n_combo_one,n_combo_two){
    spec<-apply(combo_two,2,function(y,x,n_combo_one,n_combo_two){
      quant_vec<-rep(NA,3)
      qual_vec<-rep(NA,2)
      
      if(length(x)>0){quant_vec[1:length(x)]<-x}
      if(length(y)>0){qual_vec[1:length(y)]<-y}
      
      spec<-c(quant_vec,qual_vec)
      list(spec)
    },x = x,n_combo_one= n_combo_one,n_combo_two = n_combo_two)
    spec<-unlist(spec,recursive = FALSE)
  },combo_two=combo_two,n_combo_one= n_combo_one,n_combo_two = n_combo_two) %>%
    lapply(.,function(x){
      do.call(rbind,x)
    }) %>% do.call(rbind,.)
}
