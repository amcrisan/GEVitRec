#tries a bunch of charts, uses those that are possible, removes those that are not
#also provides three scores : total fields assigned, relevance, and perceptual rank (from showME)
get_charts_specs<-function(vis_fields=NULL,required_var=NULL,dat_type=NULL,data_env_name=NULL){
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
  
  for(chart_type in names(chart_required_specs)){
    chart_req<-chart_required_specs[[chart_type]]
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