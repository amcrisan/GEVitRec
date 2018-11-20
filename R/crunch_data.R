#This is a bunch of code that will crunch and sort the input data into useable metadata

#Function to Scan tables columns and identify what they are, and if they are special data types
scanTab<-function(tabIndex=NA,objData = NULL,objMeta=NULL,dataDict=NULL){
  #no table index has been provided, so get one yourself
  if(is.na(tabIndex)){
    tabIndex<-which(objMeta$dataType == "table")
  }
  
  #if there is no table
  if(length(tabIndex) == 0){
    warning("There are no table data objects")
    return(NA)
  }
  
  #if there are multiple tables
  if(length(tabIndex)>1){
    #call yo-self!
    tmp<-sapply(tabIndex,function(x){scanTab(x,objData,objMeta,dataDict)})
    return(tmp)
  }
  

  #now let's get to business. Scan those columns!
  itemData<-allObj[[tabIndex]]@data[[1]]
  itemName<-as.character(objMeta[tabIndex,"dataID"])
  
  #scan for categories: continuous, categorgical, interval
  #if there's no special data in the column (i.e. lat/long cord)
  #then, wait for user input to decide whether to show one variable, or multiple variables
  tableInfo<-data.frame(tableSource = rep(itemName,ncol(itemData)),
                        variable = colnames(itemData),
                        class = sapply(itemData, typeof))
  
  #scan for special column types (spatial, genomic, dates)
  #universal data dictionary a standard, users can add other data dictionaries
  
  #scan and apply to special table
  tableInfo$specialClass<-apply(tableInfo,1,function(x,dict){
    #exact match so can have more variability here
    data_term <- trimws(tolower(x[["variable"]]))
    
    idxDict<-which(dict$term %in% data_term)
    category <- NA
    
    #If there's no match...
    if(length(idxDict)<1)
      return(category)
    
    #If there's a match....
    if(length(idxDict)>1){
      tmp<-table(dict[idxDict,]$category)
      tmp<-tmp/sum(tmp)
      
      # if there are multiple matches and
      # if any category breaks 50% return that
      # note: conservative route is to return NA
      tmp<-sort(tmp,decreasing = TRUE)
      
      if(tmp[1]>0.5){
        category<-names(tmp)[1]
      }
    }else{
      category<-dict[idxDict,]$category
    }
    return(category)
  },
  dict=dataDict) #Note to self: I like passing then second variable rather than just hoping its correctly scoped.
  
  return(tableInfo)
}

#HELPER FUNCTION: STANDARDIZING DATA INPTU
returnItemData<-function(index,obj,meta){
  itemData<-obj[[index]]@data[[1]]
  itemName<-as.character(meta[index,"dataID"])
  itemType<-as.character(meta[index,"dataType"])
  
  #extract the data or metadata into a table / character vector
  dataOut<-switch(itemType,
                  "phyloTree" = as.character(itemData$tip.label),
                  "spatial" = itemData,
                  "table" = itemData)
  
  return(list(itemData = itemData,itemName=itemName,itemType = itemType,data=dataOut))
}

#HELPER FUNCTION: QUICK JACCARD DISTANCE
jaccard_dist<-function(x,y){
  #intersection
  nIntersect<-length(intersect(x,y))
  #union
  total<-length(union(x,y))
  
  return(1 - (nIntersect/total))
}


#FUNCTION : FIND LINKS BETWEEN DIFFERENT DATA TYPES
findLink<-function(allObj=NA,allObjMeta = NA,cutoff=1){
  #list all pairwise combinations
  combos<-combn(1:length(allObj),m=2)
  
  #scann all pairwise data combinations 
  varComp<-c()
  for(i in 1:ncol(combos)){
    combo<-combos[,i]
    item_one<-returnItemData(combo[1],allObj,allObjMeta)
    item_two<-returnItemData(combo[2],allObj,allObjMeta)
    
    #compare vector to data.frame
    if("phyloTree" %in% c(item_one$itemType,item_two$itemType)){
      if(item_one$itemType == "phyloTree"){
        nonTab<-item_one
        tab<-item_two
      }else{
        nonTab<-item_two
        tab<-item_one
      }
      
      compRes<-apply(tab$data,2,function(tabCol,nonTabItem){
        #see how similar two vectors are
        #even numbers are treated as strings, for exact values (not distributions)
        jaccard_dist(unique(tabCol),unique(nonTabItem))
      },nonTabItem = nonTab$data)
      
      compRes<-data.frame(item_one= rep(nonTab$itemName,length(compRes)),
                          item_two= paste(tab$itemName,names(compRes),sep="-"),
                          jaccard_dist= unname(compRes),
                          stringsAsFactors = FALSE)
      varComp<-rbind(varComp,compRes)
    }else{
      #compare data.frame to data.frame
      compRes<-lapply(colnames(item_one$data),function(colName,item_one,item_two,item_one_name,item_two_name){
        tabCol<-item_one$data[,colName]
        compRes<-apply(item_two$data,2,function(tabCol_two,tabCol){
          jaccard_dist(unique(tabCol_two),unique(tabCol))
        },tabCol = tabCol)
        data.frame(item_one= rep(paste(item_one_name,colName,sep="-"),length(compRes)),
                   item_two= paste(item_two_name,names(compRes),sep="-"),
                   jaccard_dist= unname(compRes),
                   stringsAsFactors = FALSE)
      },item_one = item_one,item_two = item_two,item_one_name = item_one$itemName,item_two_name=item_two$itemName)
      
      compRes<-dplyr::bind_rows(compRes)
      varComp<-rbind(varComp,compRes)
    }
  }
  
  return(dplyr::filter(varComp,jaccard_dist<cutoff))
  
}

