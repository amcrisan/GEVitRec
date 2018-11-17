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

#DECISION 1A: IF IT IS A TABLE SCAN ITS COLUMNS
scanTab<-function(tabIndex=NA,objData = NULL,objMeta=NULL){
  #if there is no table
  if(length(tabIndex) == 0)
    stop()
  
  #if there are multiple tables
  if(length(tabIndex)>1){
    #call yo-self!
    tmp<-sapply(tabIndex,function(x){scanTab(x,objData)})
    return(tmp)
  }
  
  #now let's get to business. Scan those columns!
  itemData<-allObj[[tabIndex]]@data[[1]]
  
  #scan for categories: continuous, categorgical, interval
  #if there's no special data in the column (i.e. lat/long cord)
  #then, wait for user input to decide whether to show one variable, or multiple variables
  tableInfo<-data.frame(variable = colnames(itemData),
             class = sapply(itemData, typeof))
  
  #scan for special column types (spatial, genomic, dates)
}


#DECION 1B : IF IT IS NOT TABLE, THE VISUALIZATION CHOICE IS STRAIGHT FORWARD
