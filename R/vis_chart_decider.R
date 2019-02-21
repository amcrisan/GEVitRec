
#tries a bunch of charts, uses those that are possible, removes those that are not
#also provides three scores : total feilds assigned, relevance, and perceptual rank (from showME)
get_charts_specs<-function(vis_feilds=NULL,required_var=NULL){

  quant_feilds<-dplyr::filter(vis_feilds,feild_detail  == "quant")
  qual_feilds<-dplyr::filter(vis_feilds,grepl("qual",feild_detail))
  

  all_chart_specs<-c()
  chart_count<-1
  channel_feilds<-c("x","y","color","shape","alpha")
  
for(chart_type in names(chart_required_specs)){
    chart_req<-chart_required_specs[[chart_type]]
    
    quant_possible<-quant_feilds$name
    qual_possible<-qual_feilds$name
    
    param_idx<-which(names(chart_req) %in% channel_feilds)
    param_names<-names(chart_req)
    
    assigned_params<-c()
    
    #Attempt to assign variables to the parameters
    for(idx in param_idx){
      param_req<-chart_req[[idx]]
      
      if(param_req$feild_type == "quant"){
        # --- quantiative only feilds ---
        if(length(quant_possible) == 0) next
        
        chart_req[[param_names[idx]]]$feild<-quant_possible[1]
        quant_possible<-setdiff(quant_possible,quant_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
        
      }else if (param_req$feild_type == "qual"){
        # --- qualitative only feilds ---
        if(length(qual_possible) == 0) next
        
        chart_req[[param_names[idx]]]$feild<-qual_possible[1]
        qual_possible<-setdiff(qual_possible,qual_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
        
      }else if(param_req$feild_type=="quant|qual-many"){
        # --- qual or quant value goes ---
        tmp<-dplyr::filter(vis_feilds,name %in% qual_possible) %>%
          dplyr::filter(grepl("qual-many",feild_detail))
        
        all_possible<- if(nrow(tmp)>0) c(quant_possible,tmp$name) else quant_possible
        
        if(length(all_possible) == 0) next
        
        chart_req[[param_names[idx]]]$feild<-all_possible[1]
        qual_possible<-setdiff(qual_possible,all_possible[1])
        quant_possible<-setdiff(quant_possible,all_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
        
      }else if(grepl("qual-[0-9]+",param_req$feild_type)){
        # --- qualitative variable with specific # of levels ---
        if(length(qual_possible) == 0) next
        lvl_max<-as.numeric(gsub("qual-","",param_req$feild_type))
        
        tmp<-dplyr::filter(vis_feilds,name %in% qual_possible) %>%
          dplyr::filter(grepl("qual-[0-9]+",feild_detail))
        
        if(nrow(tmp)==0) next
          
        tmp<-dplyr::mutate(tmp,feild_detail = as.numeric(gsub("qual-","",feild_detail))) %>%
          dplyr::filter(feild_detail < lvl_max)
        
        if(nrow(tmp)==0) next
        
        tmp_qual_possible<-tmp$name
        chart_req[[param_names[idx]]]$feild<-tmp_qual_possible[1]
        qual_possible<-setdiff(qual_possible,tmp_qual_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
          
      }else{
        all_possible<-c(qual_possible,quant_possible)
        if(length(all_possible) == 0) next
        
        chart_req[[param_names[idx]]]$feild<-all_possible[1]
        qual_possible<-setdiff(qual_possible,all_possible[1])
        quant_possible<-setdiff(quant_possible,all_possible[1])
        assigned_params<-c(assigned_params,param_names[idx])
      }
    }
    
    #check if all of the required parameters have been assigned
    req_feilds<-get_req_feilds(chart_req)
    if(all(req_feilds %in% assigned_params)){
      #if yes - then YAY you can make this plot!
      #get the chart score from gevit
      #note: chart_scores comes from 
      score_idx<- grep(chart_type,tolower(chart_scores$chartType))
      
      relevance<-if(length(score_idx) ==0) NA else chart_scores[score_idx, ]$rescale
      
      all_chart_specs[[chart_count]]<-list(spec = chart_req,
                                           feilds_total = length(assigned_params),
                                           relvance = relevance)
      chart_count<-chart_count + 1
    }
  }
  
  return(all_chart_specs)
}