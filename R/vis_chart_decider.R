get_charts_specs<-function(vis_fields=NULL,required_var=NULL,dat_type=NULL,data_env_name=NULL){
  
  #add priority column to sort vis fields according to the required variables
  vis_fields<-mutate(vis_fields,priority  = as.numeric(name %in% required_var))
  
  #get list of valid specs
  all_chart_specs<-c()
  chart_count<-1

   if(dat_type == "phyloTree"){
   }
  
  for(chart_type in names(chart_template_specs)){
    chart_spec<-chart_template_specs[[chart_type]]
    
    
    #if it is not the approperiate data type, move one
    if(dat_type != chart_spec$data$datType) next
    
    
    ### DATA AND ASSOCIATED DATA SOURCES ###
    chart_spec$data$datSrc<-data_env_name
    if(dat_type !="table"){
      metadata_src<-if(nrow(dplyr::filter(vis_fields,dataSource == data_env_name)) !=0) data_env_name else NULL 
      
      if(!is.null(metadata_src)){
        chart_spec$metadata$dataSrc<-data_env_name
      }
      
    }
    
    ### FILLING IN REQUIRED FIELDS
   vis_tmp<-vis_fields
   for(i in 1:length(chart_spec)){
     
     if(class(chart_spec[[i]]) != "channel_info") next
     
     chan_info<-chart_spec[[i]]
     
     #required variable class for this encoding
     #field_req<-intersect(c("quant","qual"),unlist(strsplit(chan_info$field_type,'[\\|\\-]')))
     field_req<-unlist(strsplit(chan_info$field_type,'[\\|]'))
     
     if(nrow(vis_tmp) == 0) next #nothing left
     
     #----- v Kind of a super massive filter that could be done more cleverly v -----
    chan_fields<-dplyr::filter(vis_tmp,sapply(field_detail,function(x,field_req){
      #filter_pass<-FALSE
      
      qual_idx<-grep("qual",field_req)
      total_num<-NA
      if(length(qual_idx)!=0){
        total_num<-unlist(strsplit(field_req[qual_idx],'\\-'))[2]
        field_req[qual_idx]<-"qual"
      }
      
      split_var<-unlist(strsplit(x,"\\-"))
      field_typ<-split_var[1]
      field_num<- split_var[2] #NA if it doesn't exist
      
      if(any(field_typ %in% field_req)){
        if(field_typ=="qual" & !is.na(total_num)){
          
          if(grepl("[0-9]+",total_num)){
            
            if(grepl("[0-9]+",field_num)){
              return(as.numeric(field_num) <= as.numeric(total_num))
            }else{
              return(FALSE)
            }
            
          }else{
            return(TRUE)
          }
          
        }else{
          return(TRUE)
        }
      }else{
        return(FALSE)
      }
      
      return(FALSE)
     },field_req=field_req))
     #----- ^ Kind of a super massive filter that could be done more cleverly ^ -----
     
     if(nrow(chan_fields)==0) next #nothing suitable remains
     
     #order remaining fields by priority and degree
     chan_fields<-arrange(chan_fields,desc(priority))
     
     #assign top value to field
     chart_spec[[i]]$field<-chan_fields[1,]$name
     chart_spec[[i]]$dataSource<-chan_fields[1,]$dataSource
     
     #remove it from consideration
     vis_tmp<-dplyr::filter(vis_tmp,name !=chan_fields[1,]$name)
   }
    
    #add specification to growing list
    all_chart_specs[[chart_count]]<-list(spec = chart_spec)
    chart_count = chart_count+1
    
  }

  #return charts that have all requirements filled in
  all_chart_specs<-lapply(all_chart_specs,function(chart){
    req_check<-sapply(chart$spec,function(chan_check){
      if(class(chan_check) == "channel_info"){
        if(chan_check$req){
          if(!is.na(chan_check$field)){
            return(TRUE) #field has been assigned to require varaiable
          }else{
            return(FALSE) #field has not been assigned to required variable
          }
        }else{
          #not required, so just say you've got it
          return(TRUE) 
        }
      }else{
        return(TRUE) #only chart types with proper data fields were added
      }
    })
    
    if(all(req_check)){
      score_idx<- grep(chart$spec$chart_type,tolower(chart_scores$chartType))
      relevance<-ifelse((length(score_idx) ==0),0,chart_scores[score_idx, ]$rescale)
      return(list(spec=chart$spec,
                  fields_total = length(setdiff(names(req_check),c("chart_type","data","metadata"))),
                  relvance = relevance))
      
    }else{
      return(NULL)
    }
  })
  
  all_chart_specs<-Filter(Negate(is.null), all_chart_specs)
  
  return(all_chart_specs)
  
  # #check if all of the required parameters have been assigned
  # req_fields<-get_req_fields(chart_req)
  # if( all(req_fields %in% assigned_params)){
  #   #if yes - then YAY you can make this plot!
  #   #get the chart score from gevit
  #   #note: chart_scores comes from 
  #   score_idx<- grep(chart_type,tolower(chart_scores$chartType))
  #   
  #   relevance<-if(length(score_idx) ==0) NA else chart_scores[score_idx, ]$rescale
  #   
  #   all_chart_specs[[chart_count]]<-list(spec = chart_req,
  #                                        fields_total = length(assigned_params),
  #                                        relvance = relevance)
  #   chart_count<-chart_count + 1
  # }
  
}


#------ OLDER STUFF -----
get_charts_specs_old_two<-function(vis_fields=NULL,required_var=NULL,dat_type=NULL,data_env_name=NULL){
  
  
  quant_fields<-dplyr::filter(vis_fields,field_detail  == "quant") %>%
    dplyr::mutate(priority = ifelse(name %in% required_var,1,0))
  
  qual_fields<-dplyr::filter(vis_fields,grepl("qual",field_detail)) %>%
    dplyr::mutate(priority = ifelse(name %in% required_var,1,0))
  

  all_chart_specs<-c()
  chart_count<-1
  channel_fields<-c("x","y","size","color","shape")
  #Go through each of the possible charts
  #assess if it is possible to 
  
  #load templates of charts
  
  for(chart_type in names(chart_template_specs)){
    chart_req<-chart_template_specs[[chart_type]]
    chart_req$data$datSrc<-data_env_name
    
    #check that the data source of the of the fields
    #is actually compatible with that chart type
    if(dat_type != chart_req$data$datType) next
    
    #now fill in those potential charts
    #give required variables priority
    quant_possible<-dplyr::filter(quant_fields,priority==1)$name
    if(length(quant_possible)<3 & nrow(quant_fields)>3){
      tmp<-setdiff(quant_fields$name,quant_possible)
      quant_possible<-c(quant_possible,sample(tmp,size=3-length(quant_possible),replace=FALSE))
    }
    
    qual_possible<-dplyr::filter(qual_fields,priority==1)$name
    
    if(length(qual_possible)<2 & nrow(quant_fields)>2){
      tmp<-setdiff(qual_fields$name,qual_possible)
      qual_possible<-c(qual_possible,sample(tmp,size=2-length(qual_possible),replace=FALSE))
    }
    
    
    if(length(c(quant_possible,qual_possible)) == 0){
      if(dat_type != "table"){
        #non-tabular data will already have positional variables assigned
        #so go-ahead and use that chart!
        score_idx<- grep(chart_type,tolower(chart_scores$chartType))
        relevance<-if(length(score_idx) ==0) NA else chart_scores[score_idx, ]$rescale
        
        all_chart_specs[[chart_count]]<-list(spec = chart_req,
                                             fields_total = 0,
                                             relvance = relevance)
        next
      }
    } 
    
    #Finally, assign those variables to parameters inside charts
    param_idx<-which(names(chart_req) %in% channel_fields)
    param_names<-names(chart_req)
    
    assigned_params<-c()
    
    #Attempt to assign variables to the parameters
    for(idx in param_idx){
      param_req<-chart_req[[idx]]
      
      if(param_req$field_type == "quant"){
        # --- quantiative only fields ---
        if(length(quant_possible) == 0) next
        
        chart_req[[param_names[idx]]]$field<-quant_possible[1]
        chart_req[[param_names[idx]]]$dataSource<-dplyr::filter(vis_fields,name ==quant_possible[1])$dataSource
        
        quant_possible<-setdiff(quant_possible,quant_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
        
      }else if (param_req$field_type == "qual"){
        # --- qualitative only fields ---
        if(length(qual_possible) == 0) next
        
        chart_req[[param_names[idx]]]$field<-qual_possible[1]
        chart_req[[param_names[idx]]]$dataSource<-dplyr::filter(vis_fields,name ==qual_possible[1])$dataSource
        
        qual_possible<-setdiff(qual_possible,qual_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
        
      }else if(param_req$field_type=="quant|qual-many"){
        # --- qual or quant value goes ---
        tmp<-dplyr::filter(vis_fields,name %in% qual_possible) %>%
          dplyr::filter(grepl("qual-many",field_detail))
        
        all_possible<- if(nrow(tmp)>0) c(quant_possible,tmp$name) else quant_possible
        
        if(length(all_possible) == 0) next
        
        chart_req[[param_names[idx]]]$field<-all_possible[1]
        chart_req[[param_names[idx]]]$dataSource<-dplyr::filter(vis_fields,name ==all_possible[1])$dataSource
        
        qual_possible<-setdiff(qual_possible,all_possible[1])
        quant_possible<-setdiff(quant_possible,all_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
        
      }else if(grepl("qual-[0-9]+",param_req$field_type)){
        # --- qualitative variable with specific # of levels ---
        if(length(qual_possible) == 0) next
        lvl_max<-as.numeric(gsub("qual-","",param_req$field_type))
        
        tmp<-dplyr::filter(vis_fields,name %in% qual_possible) %>%
          dplyr::filter(grepl("qual-[0-9]+",field_detail))
        
        if(nrow(tmp)==0) next
          
        tmp<-dplyr::mutate(tmp,field_detail = as.numeric(gsub("qual-","",field_detail))) %>%
          dplyr::filter(field_detail < lvl_max)
        
        if(nrow(tmp)==0) next
        
        tmp_qual_possible<-tmp$name
        chart_req[[param_names[idx]]]$field<-tmp_qual_possible[1]
        chart_req[[param_names[idx]]]$dataSource<-dplyr::filter(vis_fields,name ==tmp_qual_possible[1])$dataSource
        
        qual_possible<-setdiff(qual_possible,tmp_qual_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
          
      }else{
        all_possible<-c(qual_possible,quant_possible)
        if(length(all_possible) == 0) next
        
        chart_req[[param_names[idx]]]$field<-all_possible[1]
        chart_req[[param_names[idx]]]$dataSource<-dplyr::filter(vis_fields,name ==all_possible[1])$dataSource
        
        qual_possible<-setdiff(qual_possible,all_possible[1])
        quant_possible<-setdiff(quant_possible,all_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
      }
    }
    
    #check if all of the required parameters have been assigned
    req_fields<-get_req_fields(chart_req)
    if( all(req_fields %in% assigned_params)){
      #if yes - then YAY you can make this plot!
      #get the chart score from gevit
      #note: chart_scores comes from 
      score_idx<- grep(chart_type,tolower(chart_scores$chartType))
      
      relevance<-if(length(score_idx) ==0) NA else chart_scores[score_idx, ]$rescale
      
      all_chart_specs[[chart_count]]<-list(spec = chart_req,
                                           fields_total = length(assigned_params),
                                           relvance = relevance)
      chart_count<-chart_count + 1
    }
  }
  return(all_chart_specs)
}