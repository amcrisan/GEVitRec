dendroTree<-phyloTree<-function(){
  enhance<-list(
    layout = NULL,
    line_color = NULL,
    line_width = NULL,
    line_shape = NULL,
    line_alpha= NULL
  )
  
  specs<-list(data = NULL,
              enhance = enhance,
              chart_type = "Tree")
  
  return(specs)
}

geo_map_shape<-function(){
    enhance<-list(
      area_color = NULL,
      area_alpha = NULL)
    
    specs<-list(data = NULL,
                enhance = enhance,
                chart_type = "Geographic Map")
    
    return(specs)
}


bar_chart<-function(){
  enhance<-list(
    pos_x = NULL,
    pos_y = NULL,
    area_color = NULL,
    area_alpha = NULL)
  
  specs<-list(data = NULL,
              enhance = enhance,
              chart_type = "Bar Chart")
  
  return(specs)
}

density_1D<-hist_chart<-box_chart<-function(){
  enhance<-list(
    pos_x = NULL,
    pos_y = NULL,
    area_color = NULL,
    area_alpha = NULL)
  
  specs<-list(data = NULL,
              enhance = enhance,
              chart_type = "Distribution")
  
  return(specs)
}

scatter_chart<-geo_map_point<-function(){
  enhance<-list(
    pos_x = NULL,
    pos_y = NULL,
    point_color = NULL,
    point_size = NULL,
    point_shape = NULL,
    point_alpha = NULL)
  
  specs<-list(data = NULL,
              enhance = enhance,
              chart_type = "Scatter Chart")
  
  return(specs)
}

line_chart<-function(){
  enhance<-list(
    pos_x = NULL,
    pos_y = NULL,
    line_color = NULL,
    line_size = NULL,
    line_shape = NULL,
    line_alpha = NULL)
  
  specs<-list(data = NULL,
              enhance = enhance,
              chart_type = "Line Chart")
  
  return(specs)
}




