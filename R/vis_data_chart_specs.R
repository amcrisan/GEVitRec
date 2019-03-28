# -------- >> Chart and field objects --------------
# Information about a specific fields
var_obj <- function(field=NA,field_type = NA,dataSource=NA,req = FALSE) {
  info <- list(field = field, field_type = field_type, dataSource = dataSource,req = req)
  attr(info, "class") <- "channel_info"
  info
}

#Information about specific data
data_obj<-function(datSrc = NA, datType = NA){
  info<-list(datSrc = datSrc,datType = datType)
  attr(info, "class") <- "data_info"
  info
}

# Specification for a single chart
chart_spec<-function(chart_type=NA,data=NA,...){
  chart_spec<-list(chart_type = chart_type,
                   data = data)
  chan_param<-list(...)
  
  for(item in names(chan_param)){
    if(item == "") next
    chart_spec<-append(chart_spec,chan_param[item])
  }
  
  attr(chart_spec,"class")<-"chart_spec"
  chart_spec
}


#Helper function to return the required fields for a chart
get_req_fields<-function(spec_val){
  param_name<-names(spec_val)
  req<-c()
  for(item in param_name){
    tmp<-spec_val[[item]]
    if(class(tmp) == "channel_info"){
      if(tmp$req){
        req<-c(req,item)
      }
    }
  }
  return(req)
}

# -------- >> Chart Templates --------------
chart_template_specs<-list()

# ---- Histogram ----
#specifications for a histogram
chart_template_specs[["histogram"]]<-chart_spec(chart_type = "histogram",
                data = data_obj(NA,"table"),
                x = var_obj(NA,"quant",dataSource=NA,TRUE),
                color = var_obj(NA,"qual-12",dataSource=NA,FALSE))

# ---- Density Plot 1-D ----
#specifications for a density plot
chart_template_specs[["density"]]<-chart_spec(chart_type = "density",
                      data = data_obj(NA,"table"),
                      x = var_obj(NA,"quant",dataSource=NA,TRUE),
                      color = var_obj(NA,"qual-12",dataSource=NA,FALSE))

# ---- Bar Chart ----
#specifications for a bar chart
chart_template_specs[["bar"]]<-chart_spec(chart_type = "bar",
                     data = data_obj(NA,"table"),
                     x = var_obj(NA,"qual-12",dataSource=NA,TRUE),
                     color = var_obj(NA,"qual-12",dataSource=NA,FALSE))

# ---- Line Chart ----
#specifications of a line chart
# chart_template_specs[["line"]]<-chart_spec(chart_type = "line",
#                       data = data_obj(NA,"table"),
#                       x = var_obj(NA,"any",TRUE),
#                       y = var_obj(NA,"any",TRUE),
#                       color = var_obj(NA,"qual-12",FALSE),
#                       shape = var_obj(NA,"qual-6",FALSE))

# ---- Scatter Chart ----
#specifications for a scatter charts
chart_template_specs[["scatter"]]<-chart_spec(chart_type = "scatter",
                       data = data_obj(NA,"table"),
                       x=var_obj(NA,"quant|qual",dataSource=NA,TRUE),
                       y=var_obj(NA,"quant|qual",dataSource=NA,TRUE),
                       color = var_obj(NA,"qual-12",dataSource=NA,FALSE),
                       shape = var_obj(NA,"qual-6",dataSource=NA,FALSE))

# ---- Boxplot ----
#specifications for a box_plot
chart_template_specs[["boxplot"]]<-chart_spec(chart_type = "boxplot",
                 data = data_obj(NA,"table"),
                 x = var_obj(NA,"qual-12",dataSource=NA,TRUE),
                 y = var_obj(NA,"quant",dataSource=NA,TRUE),
                 color = var_obj(NA,"qual-12",dataSource=NA,FALSE))

# ---- Swarm ----
#specifications for a scatter charts
chart_template_specs[["swarm"]]<-chart_spec(chart_type = "scatter",
                                              data = data_obj(NA,"table"),
                                              x=var_obj(NA,"qual",dataSource=NA,TRUE),
                                              y=var_obj(NA,"quant",dataSource=NA,TRUE),
                                              color = var_obj(NA,"qual-12",dataSource=NA,FALSE),
                                              shape = var_obj(NA,"qual-6",dataSource=NA,FALSE))

# ---- Heatmap ----
#specifications for a heatmap
chart_template_specs[["heatmap"]]<-chart_spec(chart_type = "heatmap",
                 data = data_obj(NA,"table"),
                 x = var_obj(NA,"qual",dataSource=NA,TRUE),
                 y = var_obj(NA,"qual",dataSource=NA,TRUE),
                 color = var_obj(NA,"quant",dataSource=NA,TRUE))

# ---- Tile chart ----
#specifications for a tile chart
#an alternative to the heatmap that lets the colour variable
#be a qualtitative value
chart_template_specs[["tile"]]<-chart_spec(chart_type = "tile",
                                     data = data_obj(NA,"table"),
                                     x = var_obj(NA,"qual",dataSource=NA,TRUE),
                                     y = var_obj(NA,"qual",dataSource=NA,TRUE),
                                     color = var_obj(NA,"qual-12",dataSource=NA,TRUE))

# ---- Phylogenetic Tree ----
chart_template_specs[["phylogenetic tree"]]<-chart_spec(chart_type = "phylogenetic tree",
                                           data = data_obj(NA,"phyloTree"),
                                           metadata = data_obj(NA,"table"),
                                           color = var_obj(NA,"qual-12",dataSource=NA,FALSE),
                                           shape = var_obj(NA,"qual-6",dataSource=NA,FALSE))

# ---- Image ----
chart_template_specs[["image"]]<-chart_spec(chart_type = "phylogenetic tree",
                                           data = data_obj(NA,"image"),
                                           metadata = data_obj(NA,"table"),
                                           x = var_obj(NA,"quant",dataSource=NA,FALSE),
                                           y = var_obj(NA,"quant",dataSource=NA,FALSE),
                                           color = var_obj(NA,"qual-12",dataSource=NA,FALSE),
                                           shape = var_obj(NA,"qual-6",dataSource=NA,FALSE))

# ---- Alignment ----
chart_template_specs[["alignment"]]<-chart_spec(chart_type = "alignment",
                                            data = data_obj(NA,"dna"))

# ---- Choropleth ----
chart_template_specs[["choropleth"]]<-chart_spec(chart_type = "choropleth",
                                                data = data_obj(NA,"spatial"),
                                                metadata = data_obj(NA,"table"),
                                                color = var_obj(NA,"quant|qual-12",dataSource=NA,FALSE))



