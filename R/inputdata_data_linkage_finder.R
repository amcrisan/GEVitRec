#This is a bunch of code that will crunch and sort the input data into useable metadata

#Function to Scan tables columns and identify what they are, and if they are special data types
scanTab<-function(tabIndex=NA,objData = NULL,objMeta=NULL,dataDict=NULL){
  #no table index has been provided, so get one yourself
  
  if(is.na(tabIndex)){
    #check which objects are tables
    tabIndex<-which(objMeta$dataType == "table")
    #check which objects have tablular metadata
    metatabs<-which(sapply(objMeta$dataID, function(x,obj){
      x<-obj[[as.character(x)]]
      if(is.null(x@data$metadata) || is.na(x@data$metadata))
        return(FALSE)
      
      if(is.data.frame(x@data$metadata))
        return(TRUE)
    },obj = objData))
    
    tabIndex<-unique(c(tabIndex,metatabs))
  }
  
  #if there is no table
  if(length(tabIndex) == 0){
    warning("There are no table data objects")
    return(NA)
  }
  
  #if there are multiple tables
  if(length(tabIndex)>1){
    #call yo-self!
    #scanTab(3,objData,objMeta,dataDict)
    tmp<-lapply(tabIndex,function(x){scanTab(x,objData,objMeta,dataDict)})
    tmp<-dplyr::bind_rows(tmp) #bind the rows of that recursion!
    return(tmp)
  }
  

  #now let's get to business. Scan those columns!
  if(objData[[tabIndex]]@type == "table"){
    itemData<-objData[[tabIndex]]@data[[1]]
  }else{
    itemData<-objData[[tabIndex]]@data$metadata
  }
  
  itemName<-as.character(objMeta[tabIndex,"dataID"])
  itemEnvName<-as.character(objMeta[tabIndex,"dataEnvName"])
  
  #scan for categories:
  #then, wait for user input to decide whether to show one variable, or multiple variables
  tableInfo<-data.frame(tableSource = rep(itemName,ncol(itemData)),
                        variable = colnames(itemData),
                        class = sapply(itemData, class),
                        envName = rep(itemEnvName,ncol(itemData)),
                        stringsAsFactors = FALSE)
  
  #scan for special column types (spatial, genomic, dates)
  #universal data dictionary a standard, users can add other data dictionaries
  
  # #scan and apply to special table
  # tableInfo$specialClass<-apply(tableInfo,1,function(x,dict){
  #   #exact match so can have more variability here
  #   data_term <- trimws(x[["variable"]])
  #   
  #   idxDict<-which(dict$term %in% data_term)
  #   category <- NA
  #   
  #   #If there's no match...
  #   if(length(idxDict)<1)
  #     return(category)
  #   
  #   #If there's a match....
  #   if(length(idxDict)>1){
  #     tmp<-table(dict[idxDict,]$category)
  #     tmp<-tmp/sum(tmp)
  #     
  #     # if there are multiple matches and
  #     # if any category breaks 50% return that
  #     # note: conservative route is to return NA
  #     tmp<-sort(tmp,decreasing = TRUE)
  #     
  #     if(tmp[1]>0.5){
  #       category<-names(tmp)[1]
  #     }
  #   }else{
  #     category<-dict[idxDict,]$category
  #   }
  #   return(category)
  # },
  # dict=dataDict) #Note to self: I like passing then second variable rather than just hoping its correctly scoped.
  
  return(tableInfo)
}

#HELPER FUNCTION: STANDARDIZING DATA INPUT
returnItemData<-function(index,obj,meta,returnMeta=FALSE){
  itemData<-obj[[index]]@data
  itemName<-as.character(meta[index,"dataID"])
  itemType<-as.character(meta[index,"dataType"])
  
  #extract the data or metadata into a table / character vector
  dataOut<-switch(itemType,
                  "phyloTree" = if(!returnMeta) as.character(itemData[[1]]$tip.label) else itemData$metadata,
                  "dna" = if(!returnMeta) as.character(names(itemData[[1]])) else itemData$metadata,
                  "spatial" = if(!returnMeta) rownames(itemData$geometry) else itemData$metadata,
                  "table" = itemData[[1]])
  
  return(dataOut)
  #return(list(itemData = itemData,itemName=itemName,itemType = itemType,data=dataOut))
}

#HELPER FUNCTION: QUICK JACCARD DISTANCE
jaccard_dist<-function(x,y){
  #intersection
  nIntersect<-length(intersect(x,y))
  #union
  nUnion<-length(union(x,y))
  dist_metric<-(nUnion - nIntersect)/nUnion
  
  #return(1 - (nIntersect/total))
}


#amethod to check links between two charts
check_link<-function(item_one = NA,item_two=NA,item_one_name=NA,item_two_name=NA){
  compRes<-NULL
  
  if(is.null(ncol(item_one)) & is.null(ncol(item_two))){
    #two vectors
    class_one<-class(item_one)
    class_two<-class(item_two)
    
    item_one<-if(class_one=="factor") as.character(item_one) else item_one
    item_two<-if(class_two=="factor") as.character(item_two) else item_two
    
    if(class_one == class_two){
      if(class_one %in% c("factor","character")){
        if(dplyr::setequal(item_one,item_two)){
          return(0)
        }else{
          return(jaccard_dist(item_one,item_two))
        }
      }else{
        #don't match on numerical axes yet, it could be a co-incidence
        #mainly looking to match ids to some ids column
        compRes<-NULL
      }
    }
    
  }else if(is.null(ncol(item_one)) | is.null(ncol(item_two))){
    #one vector and one data frame
    df_item<-if(is.data.frame(item_one)) item_one else item_two
    vec_item<-if (!is.data.frame(item_one)) item_one else item_two
    
    #names of items
    df_item_name<-if(is.data.frame(item_one)) item_one_name else item_two_name
    vec_item_name<-if (!is.data.frame(item_one)) item_one_name else item_two_name
    
    #get similarity
    compRes<-apply(df_item,2,function(x,comp_item){
      check_link(x,comp_item)
    },comp_item = vec_item)
    
    
    compRes<-cbind(rep(as.character(vec_item_name),length(compRes)),
                   rep(as.character(df_item_name),length(compRes)),
                   names(compRes),
                   unname(compRes))
    
  }else{
    #comparing and linking two data frames
    compRes<-do.call("rbind",apply(item_one,2,function(x,comp_item,name_one,name_two){
      data.frame(check_link(item_one = x,item_two = comp_item,name_one,name_two))
    },comp_item = item_two,name_one = item_one_name, name_two = item_two_name))
    
    
    compRes[sapply(compRes,function(x){length(x)==0})] <- NULL
    
    
  }
  
  return(compRes)
}


#FUNCTION : FIND LINKS BETWEEN DIFFERENT DATA TYPES
#' Title
#'
#' @param allObj 
#' @param allObjMeta 
#' @param cutoff 
#'
#' @importFrom dplyr %>%
#' @return

findLink<-function(allObj=NA,allObjMeta = NA,cutoff=1){
  
  if(length(allObj) < 2)
    return(NULL)
  
  noQuantMeta<-dplyr::filter(allObjMeta,is.na(feild_detail) | grepl("qual",feild_detail))
  
  #list all pairwise combinations
  combos<-combn(1:length(allObj),m=2)
  
  #scan all pairwise data combinations to find links between data items
  varComp<-c()
  for(i in 1:ncol(combos)){
    combo<-combos[,i]
    
    #get standard information out
    item_one<-returnItemData(combo[1],allObj,noQuantMeta)
    item_two<-returnItemData(combo[2],allObj,noQuantMeta)
    
    #get names to be passed to other functions
    item_one_name = as.character(noQuantMeta[combo[1],"dataID"])
    item_two_name = as.character(noQuantMeta[combo[2],"dataID"])
    
    #check if non-tabular objects "exploded feilds" better to link on that
    if(item_one_name %in% noQuantMeta$dataSource & as.character(noQuantMeta[combo[1],"dataType"])!="table"){
      returnItemData(combo[1],allObj,noQuantMeta,TRUE)
    }
    
    if(item_two_name %in% noQuantMeta$dataSource & as.character(noQuantMeta[combo[2],"dataType"])!="table"){
      item_two<-returnItemData(combo[2],allObj,noQuantMeta,TRUE)
    }
  
    #check whether there are any linkages between these data items
    
    links<-check_link(item_one,item_two,item_one_name,item_two_name)

  if(length(links) == 1){
    varComp<-rbind(varComp,c(item_one_name,item_two_name,paste(item_two_name,"gevitID",sep="_"),links))
  }else{
    links<-as.matrix(links) #easier to rbind this way
    varComp<-rbind(varComp,links)
  }
    
  }
  
  
  #get the comparing variable for the data
  compvar<-apply(cbind(rownames(varComp),varComp[,1]),1, function(x,allObjMeta){
    if(x[1]=="")
        return(paste(x[2],"gevitID",sep ="_"))
      
      return(gsub("\\.[0-9]+$","",x[1]))
    
  },allObjMeta) %>% unlist() %>%unname()
  
  
  varComp<-data.frame(item_one = varComp[,1],
                      item_one_var = compvar,
                      item_two = varComp[,2],
                      item_two_var = varComp[,3],
                      jaccard_dist = as.numeric(varComp[,4]),
                      stringsAsFactors = FALSE)
  
  return(varComp)
  #return(dplyr::filter(varComp,jaccard_dist<cutoff))

  
}

