
data_harmonization<-function(...,dataDict=NULL){
  allObj<-list(...)
  
  #use internal data dictionary if none is provided
  if(is.null(dataDict)){
   dataDict = readxl::read_xlsx(system.file("inst/epivis_shiny/data_dictionaries/", "universal_data_dictionary.xlsx", package = "epivis"))
  }
  
  #name list object after the ids
  #TO DO: throw error if its not a gevitObject
  names(allObj)<-sapply(allObj,function(x){x@id})
  
  #create metadata table about the objects
  allObjMeta<-data.frame(dataID = sapply(allObj,function(x){x@id}),
                         dataType= sapply(allObj,function(x){x@type}),
                         dataSource = sapply(allObj,function(x){x@source}),
                         dataEntity = rep("dataType",length(allObj)))
  
  #-------- EXPLODE DATA ------------------
  # Explode feilds from tables and data types store them in metadata table
  #load necessary data dictionary
  tabScanned<-scanTab(objData=allObj,objMeta=allObjMeta,dataDict=dataDict)
  
  #add exploded feilds to data type
  tabScanned<-data.frame(dataID=tabScanned$variable,
                         dataType= tabScanned$class,
                         dataSource = tabScanned$tableSource,
                         dataEntity = rep("feild",nrow(tabScanned)))
  
  allObjMeta<-rbind(allObjMeta,tabScanned)
  
  
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
  
  #-------- RETURN HARMONIZED DATA OBJECT ------------------
  harmonized<-list("dataObj" = allObj,
       "dataMeta" = allObjMeta,
       "entityGraph" = exploded_graph,
       "edgeList" = exploded_edges)
  
  class(harmonized)<-c("list","GEVITRec")
  
  return(harmonized)
}

view_entity_graph<-function(harmon_obj = NULL){
  if(!"GEVITRec" %in% class(harmon_obj))
    return("This method only works for gevitREC objects")
  
  exploded_graph<-harmon_obj[["entityGraph"]]
  objMeta<-harmon_obj[["dataMeta"]]


  exploded_graph<-tidygraph::as_tbl_graph(exploded_graph)
  
  #adding metadata
  #note as_tiddle lets you see node data for testing
  exploded_graph<- exploded_graph %>%
    activate(edges) %>%
    mutate(jaccard_distance = 1-as.numeric(weights)) %>%
    mutate(edge_type =  ifelse((jaccard_distance == 1), "direct","inferred")) %>%
    activate(nodes) %>%
    full_join(objMeta,by=c("name" ="dataID")) %>%
    mutate(dataSource = ifelse(file.exists(as.character(dataSource)),name,as.character(dataSource))) %>%
    mutate(dataType  = ifelse(is.na(dataEntity),"character",as.character(dataEntity))) %>%
    mutate(dataSource = ifelse(is.na(dataSource), as.character(name),as.character(dataSource))) %>%
    mutate(dataSource = ifelse(grepl("gevitID",dataSource),
                               gsub("_gevitID","",dataSource),
                               as.character(dataSource)))%>%
    mutate(dataEntity  = ifelse(is.na(dataEntity),"feild",as.character(dataEntity)))
  
  #Creating the graph objet to view
  p<-ggraph(exploded_graph) +
    geom_edge_link(aes(linetype = edge_type,alpha = jaccard_distance))+
    geom_node_point(aes(shape = dataEntity,color=dataSource),size=4)+
    scale_shape_manual(values = c(19,20))+
    scale_edge_alpha_continuous(range = c(0.2,1))+
    theme_graph()
  
  plot(p)
}
