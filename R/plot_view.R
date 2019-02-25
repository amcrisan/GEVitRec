plot_view<-function(view_obj = NULL,view_num=NULL){
  
  if(is.null(view_num)){
    view_total<-length(length(view_obj))
    
    stop(sprintf("You must specify which view you want to visualize. There are a total of %d views produced by GEViTRec. Please provide a value to the view_num 1 up to %d",view_total,view_total))
  }
  
  view_obj<-view_obj[[view_num]]
  
  #for each spec in the single view, assign the variable to the environment
  var_tmp<-"chart_spec"
  
  vars<-c()
  for(i in 1:length(view_obj$single_charts)){
    chart_spec<-view_obj$single_charts[[i]]
    var_name<-paste(var_tmp,i,sep="_")
    assign(x = var_name,value = chart_spec,envir = parent.frame())
    vars<-c(vars,var_name)
  }
  
  combo_plot<-mincombinr::specify_combination(combo_type = "many_types_general",base_charts = vars)
  
  #plot a many types general combination
  plot(combo_plot)
  
}