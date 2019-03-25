
#' Data harmonization
#'
#' @param field 
#' @param field_type 
#' @param datSource 
#' @param obj 
#' @param meta 
#'
#' @return
#'
#' @examples
get_field_details<-function(field = NULL,field_type = NULL,datSource = NULL,obj=NULL,meta){
  
  if(any(c("numeric","integer","Date") %in% field_type)){return("quant")}
  dat_tmp<-NULL
  index<-which(meta$dataID == datSource)
  
  if(grepl("table",datSource)){
    dat_tmp<-obj[[index]]@data[[1]]
    dat_tmp<-dat_tmp[,field]
  }else if (!is.null(obj[[index]]@data$metadata)){
    if(grepl("gevitID",field)){
      dat_tmp<-returnItemData(index,obj,meta)
    }else{
      dat_tmp<-obj[[index]]@data$metadata
      dat_tmp<-dat_tmp[,field]
    }
  }else{
    dat_tmp<-returnItemData(index,obj,meta)
  }
  
  if(is.null(dat_tmp)){return("ERROR!")}
  
  var<-unique(dat_tmp)
  if(length(var)>12){
    return("qual-many")
  }else{
    return(paste("qual",length(var),sep="-"))
  }
}

#' Title
#'
#' @param ... 
#' @param dataDict 
#' @import igraph
#' @import dplyr
#' @import tidygraph
#' @return
#' @export
#'
#' @examples
data_harmonization<-function(...,dataDict=NULL){
  
  allObj<-list(...)
  
  objNames<-setdiff(as.character(match.call()),c("data_harmonization","dataDict"))
  #use internal data dictionary if none is provided
  # if(is.null(dataDict)){
  #  dataDict = readxl::read_xlsx(system.file("inst/epivis_shiny/data_dictionaries/", "universal_data_dictionary.xlsx", package = "epivis"))
  # }
  
  #name list object after the ids
  #TO DO: throw error if its not a gevitObject
  names(allObj)<-sapply(allObj,function(x){x@id})
  
  #create metadata table about the objects
  allObjMeta<-data.frame(dataID = sapply(allObj,function(x){x@id}),
                         dataType= sapply(allObj,function(x){x@type}),
                         dataSource = sapply(allObj,function(x){x@source}),
                         dataEntity = rep("dataSource",length(allObj)),
                         dataEnvName = objNames,
                         stringsAsFactors = FALSE)
  
  #-------- EXPLODE DATA ------------------
  # Explode fields from tables and data types store them in metadata table
  #load necessary data dictionary
  tabScanned<-scanTab(objData=allObj,objMeta=allObjMeta,dataDict=dataDict)
  #add exploded table fields to data type
  tabScanned<-data.frame(dataID=tabScanned$variable,
                         dataType= tabScanned$class,
                         dataSource = tabScanned$tableSource,
                         dataEntity = rep("field",nrow(tabScanned)),
                         dataEnvName = tabScanned$envName,
                         stringsAsFactors = FALSE)

  allObjMeta<-rbind(allObjMeta,tabScanned)
  # Add some more field details to the data
  detail_tmp<-apply(cbind(allObjMeta$dataID,allObjMeta$dataEnvName),1,function(x){paste(x[1],x[2],sep =";")})

  allObjMeta$field_detail<-sapply(detail_tmp,function(var,obj,objMeta){
    var<-strsplit(var,";")[[1]]
  
    tmp<-dplyr::filter(objMeta,dataID == var[1]) %>%
      dplyr::filter(dataEnvName == var[2])
    
    if(tmp$dataEntity == "dataSource"){
      return(NA)
    }
    
    tmp<-get_field_details(field = tmp$dataID,
                           field_type = tmp$dataType,
                           datSource = tmp$dataSource,
                           obj = obj,
                           meta = objMeta)  
  },obj = allObj, objMeta =  allObjMeta) %>% unname()
  
  
  #-------- FIND DATA LINKAGES ------------------
  exploded_dataset<-findLink(allObj=allObj,allObjMeta=allObjMeta)
  dataset_links<-dplyr::filter(exploded_dataset,jaccard_dist<1)
  
  #-------- GENERATE ENTITY GRAPH ------------------
  # A sloppy way to make an edge list
  #a slight rearrange to make an edge_list
  tmp<-exploded_dataset[,c(1:2)]
  colnames(tmp)<-c("dataset","var")
  
  tmp2<-exploded_dataset[,c(3:4)]
  colnames(tmp2)<-c("dataset","var")
  
  exploded_edges<-rbind(tmp,tmp2) %>% distinct()
  exploded_edges$weights<-rep(0,nrow(exploded_edges))
  exploded_edges$link_type<-rep("data-var",nrow(exploded_edges))
  
  
  #now add links for derived connections
  #what happens if columns have the same name? Address this
  for(i in 1:nrow(dataset_links)){
    item_one<-dataset_links[i,]$item_one_var
    item_two<-dataset_links[i,]$item_two_var
    weight<-dataset_links[i,]$jaccard_dist
    link_type<-"var-var"
    
    exploded_edges<-rbind(exploded_edges,c(item_one,item_two,weight,link_type))
  }
  
  #create a graph
  exploded_graph<-igraph::graph_from_data_frame(exploded_edges,directed = FALSE)
  exploded_graph<-tidygraph::as_tbl_graph(exploded_graph)
  
  #adding metadata
  #note as_tiddle lets you see node data for testing
  exploded_graph<- exploded_graph %>%
    activate(edges) %>%
    mutate(jaccard_distance = 1-as.numeric(weights)) %>%
    mutate(edge_type =  ifelse((jaccard_distance == 1), "exact","inexact")) %>%
    activate(nodes) %>%
    full_join(allObjMeta,by=c("name" ="dataID")) %>%
    mutate(dataSource = ifelse(file.exists(as.character(dataSource)),name,as.character(dataSource))) %>%
    mutate(dataType  = ifelse(is.na(dataType),"character",as.character(dataType))) %>%
    mutate(dataSource = ifelse(is.na(dataSource), as.character(name),as.character(dataSource))) %>%
    mutate(dataSource = ifelse(grepl("gevitID",dataSource),
                               gsub("_gevitID","",dataSource),
                               as.character(dataSource)))%>%
    mutate(dataEntity  = ifelse(is.na(dataEntity),"field",as.character(dataEntity))) %>%
    mutate(dataEnvName = check_name(.))%>%
    mutate(field_detail = check_field(.,allObjMeta,allObj)) %>%
    mutate(degree = centrality_degree())
  
  graph_components<-components(exploded_graph)$membership %>% stack()
  colnames(graph_components)<-c("component","name")
  
  exploded_graph<-exploded_graph%>% activate(nodes)%>%
    inner_join(graph_components,by="name")
  
  
  # #fill in those missing variables for the ID
  # entity_graph_table$field_detail<-apply(entity_graph_table[,1:6],1,function(x,meta,obj){
  #   if(!is.na(x[6]))
  #     return(x[6])
  #   
  #   if(x[4] == "dataSource")
  #     return(NA)
  #   
  #   get_field_details(field = x[1],
  #                     field_type = x[2],
  #                     datSource = x[3],
  #                     obj = obj,
  #                     meta = meta)
  # },meta = objMeta,obj=obj)
  
  #-------- RETURN HARMONIZED DATA OBJECT ------------------
  harmonized<-list("dataObj" = allObj,
       "dataMeta" = allObjMeta,
       "entityGraph" = exploded_graph,
       "edgeList" = exploded_edges)
  
  class(harmonized)<-c("list","GEVITRec")
  
  return(harmonized)
}

#' Subset Graph
#'
#' @param g 
#' @param cutoff 
#' @import igraph
#' @import dplyr
#' @import tidygraph
#' @return
#' @export
#'
#' @examples
subset_graph<-function(g=NULL,cutoff=0){

  g<-g %>%
    activate(edges)%>%
    filter(jaccard_distance >= cutoff) %>%
    activate(nodes)%>%
    mutate(degree = centrality_degree())
  
  
  graph_components<-components(g)$membership %>% stack()
  colnames(graph_components)<-c("component","name")
  
  g<-g %>% activate(nodes) %>%
    inner_join(graph_components,by="name") %>% 
    mutate(component = component.y)%>%
    dplyr::select(-contains('component.x')) %>%
    dplyr::select(-contains('component.y'))

  
  return(g)
}

#' View entity graph
#'
#' @param exploded_graph 
#' @param cutoff 
#' @import ggplot2
#' @import ggraph
#' 
#' @return
#' @export
#'
#' @examples
view_entity_graph<-function(exploded_graph = NULL,cutoff=0){
  if(!"igraph" %in% class(exploded_graph))
    return("need igraph object")

  #Creating the graph objet to view
  p<-ggraph(exploded_graph) +
    geom_edge_link(aes(linetype = edge_type,alpha = jaccard_distance))+
    geom_node_point(aes(shape = dataEntity,color=dataEnvName,size=dataEntity))+
    scale_shape_manual(values = c(15,20),name = "Entity")+
    scale_size_manual(values=c(6,4),name="Entity")+
    scale_edge_alpha_continuous(range = c(0.2,1),name="Jaccard Index")+
    guides(color = guide_legend(title="Data Source"),
           linetype = guide_legend(title="Linkage Type"))+
    theme_graph()
  
  plot(p)
}
