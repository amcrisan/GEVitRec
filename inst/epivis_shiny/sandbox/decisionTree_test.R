devtools::load_all()
library(dplyr)
library(ape)


#***************************************************************
#
# 0. Load in all of the test data, store it in a data structure
#
#***************************************************************

#----------------------
# TABLE FILE FORMATS
#-----------------------
tableFile<-"/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/table_data/microreact-project-west-african-ebola-epidemic-data.csv"

tableOBJ<-input_data(file=tableFile,dataType="table")

#----------------------
# TREE FILE FORMATS
#-----------------------
#loading tree data - DONE
treeFile<-"/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/tree_data/microreact-project-west-african-ebola-epidemic-tree.nwk"
treeEBOV<-input_data(file = treeFile,dataType="tree")
# 
# #load tree File with metadata - DONE
# treeFile<-"/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/tree_data/microreact-project-west-african-ebola-epidemic-tree.nwk"
# treeMeta<-"/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/table_data/microreact-project-west-african-ebola-epidemic-data.csv"
# treeEBOV<-input_data(file = treeFile,dataType="tree",metadata=treeMeta)


#----------------------
# SPATIAL FILE FORMATS
#-----------------------
#load spatial / shape data - DONE
#To do: add metadata? I.e. additional information related to spatial coordinates
shapeFile = "/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/spatial_data/GIN_adm1_1m_ocha.shp"
shapeObj<-input_data(file = shapeFile,dataType = "spatial")

#----------------------
# DNA FILE FORMATS
#-----------------------
# CURRENTLY SAVING EVERYTHING AS A DNABIN OBJECT FOR EVERYTHING
# COULD BREAK WITH LARGE FILES, SO NEED TO FIX THAT PROPERLY.. BUT THAT'S LONGER TERM
# THINKG ABOUT STORING GFF, AND REFERENCE SEQUENCE.. LAST USEFUL THING, TO DO SOON

#---- FASTA ---------
# directory of fasta files - DONE
#fastaDir<-"/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/dna_data/fasta_files/"
#dnaFASTADIRObj<-input_data(file=fastaDir,dataType = "dna")

# inidividual fasta file - DONE
fastaFile<-"/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/testdata/dna_data/fasta_files/Guinea-surveillance-20150805.good.fasta"
dnaFASTAObj<-input_data(file=fastaFile,dataType = "dna")

#----------------------------------
# DATA STRUCTURE TO RULE THEM ALL
#----------------------------------

#there has to be a 1:1 exact mapping between allObj and allObjMeta
allObj<-c(treeEBOV,tableOBJ,shapeObj,dnaFASTAObj)

allObjMeta<-data.frame(dataID = sapply(allObj,function(x){x@id}),
                   dataType= sapply(allObj,function(x){x@type}),
                   dataSource = sapply(allObj,function(x){x@source}))

#***************************************************************
#
# 1. Decision tree of how to visualize the data
#
#***************************************************************
#DECISION 1: IS IT A TABLE OR NOT?
tabIndex<-which(allObjMeta$dataType == "table")

tabScanned<-scanTab(tabIndex,allObj,allObjMeta)

#DECISION 1A: IF IT IS A TABLE SCAN ITS COLUMNS FIGURE OUT WHAT'S INSDIE
scanTab<-function(tabIndex=NA,objData = NULL,objMeta=NULL){
  #if there is no table
  if(length(tabIndex) == 0)
    stop("There are no table data objects")
  
  #if there are multiple tables
  if(length(tabIndex)>1){
    #call yo-self!
    tmp<-sapply(tabIndex,function(x){scanTab(x,objData,objMeta)})
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
  dataDict<-readxl::read_xlsx(path="inst/epivis_shiny/data_dictionaries/universal_data_dictionary.xlsx")
  
  #TO DO - when there are multiple data dictionaries, put them all together
  
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


#DECISION 1B: FIND LINKS ACROSS ALL OF THE DATA
#tabScanned<-data.frame(dataID = tabScanned$variable,
#                           dataType = tabScanned$specialClass,
#                           dataSource = tabScanned$tableSource)

#allObjMeta<-rbind(allObjMeta,specialTabData)

# DECISION 2 : CHECK IF THERE IS AN LINKING VARIABLE BETWEEN ALL THE DATA OBJECTS
# NOTE: THINGS MAY LINK IN MULTIPLE DIFFERENT WAYS

# tables, again, are the hardest because there's so much data in them, other data objects are more straight forward..
# probably best to store this as a network, because relationships between distant objects could be imputed

# for each object's data and metadata, compare to other objects

#HELPER FUNCTIONS
#structured summary of data type
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

#quick jaccard distance calcuations
jaccard_dist<-function(x,y){
  #intersection
  nIntersect<-length(intersect(x,y))
  #union
  total<-length(union(x,y))
  
  return(1 - (nIntersect/total))
}

#get jaccard index to start
#get all unique pairwise comparisons between datasets

#currently, assuming only one phylogenetic tree is loaded
combos<-combn(1:3,m=2)

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

#remove any comparisons that have literally ZERO in common
varComp<-dplyr::filter(varComp,jaccard_dist<1)


# DECISION SUMMARY : PUTTING EVERYTHING TOGETHER

# object data and metadata are allObj and allObjMeta
# table scanns are in tabScanned and will now be added to allObjMeta
tabScanned<-data.frame(dataID=tabScanned$variable,
                       dataType=apply(tabScanned[,c("class","specialClass")],1,function(x){ifelse(is.na(x[2]),x[1],paste(x[1],x[2],sep=";"))}),
                       dataSource = tabScanned$tableSource)

allObjMeta<-rbind(allObjMeta,tabScanned)

# Table with links and relationships between all data types is varComp

#These three datasets basically control the decision enginge about what gets shown and how

