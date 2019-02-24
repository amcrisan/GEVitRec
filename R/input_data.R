#Function that creates a unique ID for each data item
#code from: https://stackoverflow.com/questions/42734547/generating-random-strings
randID <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}


#' Input Data
#'
#' Generic input function for inputing data, helper functions are in the back
#'
#' @param file
#' @param dataType
#' @param asObj
#' @param desc
#' @param ...
#'
#' @import dplyr
#' @export
#' @return
input_data<-function(file  = NA, dataType = NA, asObj=TRUE,desc = NA,...){
  #Only supports specific data types
  #TODO: data for sequence alignment
  if(!(dataType %in% c("tree","table","dna","spatial","image")))
    stop("Data type is not supported. Please choose one of : tree, table, dna, spatial, image. Use ?input_data to learn more about the different input types.")
  
  #print(as.list(match.call())) #for testing only
  
  #make a unique ID for each object
  dataID<-randID()
  
  switch(dataType,
         "table" = input_table(file,asObj,dataID,desc,...),
         "tree" = input_phyloTree(file,asObj,dataID,desc,...),
         "dna" = input_dna(file,asObj,dataID,desc,...),
         "spatial" = input_spatial(file,asObj,dataID,desc,proj4String,...),
         "image" = input_image(file,asObj,dataID,desc,...),
         "rdata" = input_rdata(file, asObj, dataID,decs,...))
}

#***************************************************************
# INPUT TABLE DATA
#***************************************************************
input_table<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,stringsAsFactors = FALSE,...){
  #autodetect file type
  if(stringr::str_detect(file,"xls$|xlsx$")){
    dat<-readxl::read_excel(path=file,...)
  }else if(stringr::str_detect(file,"csv$")){
    dat<-read.csv(file=file,stringsAsFactors = stringsAsFactors,header=TRUE,...)
  }else if(stringr::str_detect(file,"tsv$")){
    dat<-read.table(file=file,stringsAsFactors = stringsAsFactors,header=TRUE,sep="\t",...)
  }else{
    stop("GEViT currently only supports xls,xlsx,csv,and tsv table input formats.")
  }
  
  #little bit of column name normalization
  colnames(dat)<-tolower(colnames(dat))
  
  if(asObj){
    objDat<-new("gevitDataObj",
                id  = paste("table",dataID,sep="_"),
                type = "table",
                source = file,
                data = list(table = dat))
    return(objDat)
  }else{
    return(dat)
  }
}
#***************************************************************
# INPUT DNA DATA
#***************************************************************
input_dna<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,fileType=NA,...){
  #right now, user must specify a file, and not a directory
  if(dir.exists(file)){
    #actions for a directory
    fileList<-list.files(file,full.names=TRUE)
    
    #identify file formats
    fileType<-sapply(fileList,function(x){
      dna_detectFileType(x)
    })
    
    if(sum(is.na(fileType)) == length(fileType)){
      stop("There are no vcf or fasta files to upload in this directory.")
    }
    
    #quick check on file num and size - warn user if too many files or too large
    fileInfo<-as.data.frame(t(sapply(fileList,file.info)),stringsAsFactors = FALSE)
    
    #warn if it looks like all files will take up 4 gigs of RAM or more
    totalFileSizeEst<-sum(as.numeric(fileInfo$size))/(1024^3) #convert bytes to gigs
    scalingFactor <- 2 #seems reasonable, but sometimes it's as much as 10!
    
    if(totalFileSizeEst*scalingFactor>4){
      warning("Current input files likely to exceed 4 gigs of RAM.")
    }
    
    #now call yo-self!
    #put all individual files into a single DNA bin object
    outInfo<-apply(cbind(fileList,fileType),1,function(x){
      input_dna(file=x[1],fileType=x[2],asObj=FALSE)
    }) %>%
      data.table::rbindlist() %>%
      toDNABIN()
    
    objDat<-new("gevitDataObj",
                id  = paste("dna",dataID,sep="_"),
                type = "dna",
                source = fileList,
                data = list(dnaBin=outInfo))
    
    return(objDat)
  }else{
    #actions for a single file
    if(is.na(fileType)){
      fileType<-dna_detectFileType(file)
    }
    
    if(!(fileType %in% c("vcf","fasta")))
      stop("Only VCF and FASTA files are supported at this time")
    
    #run approperiate input function given the file type
    output<-switch(fileType,
                   "vcf" = input_vcf(file,format=NA,...),
                   "fasta" = input_fasta(file,format=NA,...))
    
    
    if(asObj){
      objDat<-new("gevitDataObj",
                  id  = paste("dna",dataID,sep="_"),
                  type = "dna",
                  source = file,
                  data = list(dnaBin=output))
    }else{
      return(output)
    }
  }
}

#Helper function to detect file types for DNA input
dna_detectFileType<-function(file){
  #autodetect file type
  if(stringr::str_detect(file,".vcf$|.vcf.gz$")){
    return("vcf")
  }else if(stringr::str_detect(file,".fasta$|.fasta.gz$|.fa|.fa.gz$")){
    return("fasta")
  }else{
    return(NA)
  }
}

#***********************************************
# input_vcf
# This function is very tailored to epiDRIVE, so it won't work
# well generically. But what it does is assumes a concatenated vcf
# file instead of directory. ShinyFiles does allow directory files
# to be loaded (yay!) however, the problem automatically
# add the UI and the server observer function, which is just
# tricky to get right. Using the general shiny fileINPUT
# it becomes necessary to do this step
#
input_vcf<-function(file=NA,asObj=TRUE,filter=NA,...){
  #Currently will not work with header, assume multiple files stuck together
  #Only really works where all the files at concatenated together
  if(stringr::str_detect(file,".vcf.gz$")){
    tmp<-data.table::fread(paste0('zcat < ',file))
  }else{
    tmp<-data.table::fread(file)
  }
  #for now, just take the first five columns
  tmp<-tmp[,1:5]
  colnames(tmp)<-c("SAMPID","CHROM","POS","REF","ALT")
  
  #rename the sampleID
  splitChar<-ifelse(stringr::str_detect(file,".vcf.gz"),"\\.vcf\\.gz","\\.vcf")
  tmp<-tmp %>%
    group_by(SAMPID) %>%
    mutate(SAMPMOD=unlist(str_split(SAMPID,splitChar))[[1]]) %>%
    mutate(CHROMPOS = paste(CHROM,POS,sep=":")) %>%
    ungroup()
  
  #reference sequence for each position
  refSeq<-dplyr::select(tmp,CHROMPOS,REF) %>% distinct()
  
  #convert DNA object
  seqList<-list()
  for(sampVal in basename(unique(tmp$SAMPMOD))){
    subSamp<-dplyr::filter(tmp,SAMPMOD == sampVal)
    idx<-match(subSamp$CHROMPOS,refSeq$CHROMPOS)
    subSeq<-refSeq
    subSeq[idx,"REF"]<-subSamp$ALT
    seqList[[sampVal]]<-subSeq$REF
  }
  
  remove(refSeq,tmp)
  gc()
  
  return(as.DNAbin(seqList))
}

#helper function for VCF that preps a directory for loading
#quite a few things to add to make it work for fasta and gz files
#currently only works on linux files
prepGenomics<-function(fileDIR=NULL,filetype = NULL){
  #currently only works for VCFS, need to extend to other file types
  if(dir.exists(fileDIR)){
    if(file.exists(sprintf("%s/epiDRIVE_allTogether.vcf",fileDIR))){
      file.remove(sprintf("%s/epiDRIVE_allTogether.vcf",fileDIR))
    }
    system(sprintf("awk '{print FILENAME,$0}' %s/*.vcf | grep -v '#' | cat > %s/epiDRIVE_allTogether.vcf",fileDIR,fileDIR))
  }
}

#***********************************************
# Input fasta files that are zipped or otherwise
#
input_fasta<-function(file=NA,...){
  if(stringr::str_detect(file,".fasta.gz$")){
    return(ape::read.FASTA(file = gzfile(file), type="DNA"))
  }else{
    return(ape::read.FASTA(file = file, type="DNA"))
  }
}

#***************************************************************
# INPUT SHAPE FILE DATA
#***************************************************************
input_spatial<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,...){
  
  if(!stringr::str_detect(file,"shp$")){
    stop("Currently, the input file only loads shapefiles. Is your map an image file? Please choose set dataType to 'image' in order to load it properly")
  }
  
  #check in the directory to make sure supporting files are there
  #check dbf or csv file for metadata
  nc <- sf::st_read(file, quiet = TRUE)
  
  #create an object
  if(asObj){
    sfc_meta<-NA
    sfc_info<-nc
    
    #if there is any metadata, split it out into a data frame
    if(ncol(nc)>1){
      sfc_idx<-sapply(sapply(nc,class),function(x){"sfc" %in% x})
      sfc_info<-nc[,which(sfc_idx)]
      nc<-as.data.frame(nc)
      sfc_meta<-nc[,which(!sfc_idx)]
    }
    
    objDat<-new("gevitDataObj",
                id  = paste("spatial",dataID,sep="_"),
                type = "spatial",
                source = file,
                data = list(geometry=sfc_info,
                            metadata = sfc_meta))
    return(objDat)
  }else{
    return(nc)
  }
}

#Helper function to join spatial file

join_spatial_data<-function(...,obj_names = NULL){
  
  spatial_obj<-list(...)
  
  #spatial object variables
  dataID<-paste("spatial",randID(),sep="_")
  
  geo_data<-c()
  geo_metadata<-c()
  
  for(idx in 1:length(spatial_obj)){
    
    obj<-spatial_obj[[idx]]
    
    if(obj@type !="spatial") next
    
    geo_tmp<-obj@data$geometry
    geo_meta_tmp<-if(!is.null(obj@data$metadata)) obj@data$metadata else NULL
    
    item_id<- ifelse(is.null(obj_names),obj@id,obj_names[idx])
    
    ##DEV NOTE: Assumes shape file only has geometry column
    # When read in as a shape file
    #adding to the geometry item
    geo_tmp_col<-colnames(geo_tmp)
    
    if(length(geo_tmp_col)>1){
      warning("This method assumes that your shape file data only contains geometry information. Strange sideffects may ensue.")
    }
    
    geo_tmp<-cbind(rep(item_id,times = nrow(geo_tmp)),geo_tmp)
    colnames(geo_tmp)<- c("minID",geo_tmp_col)
    
    geo_data<-rbind(geo_data,geo_tmp)
    
    #adding to metadata item
    if(is.null(geo_meta_tmp) | all(is.na(geo_meta_tmp))) next
    
    geo_meta_col<-colnames(geo_meta_tmp)
    geo_meta_tmp<-cbind(rep(item_id,times = nrow(geo_tmp)),geo_meta_tmp)
    
    #TO DO: This is not a safe action...
    #But if everything is different, the user needs to suss it out
    #Build a warning message in
    geo_metadata<-rbind(geo_metadata,geo_meta_tmp)
    colnames(geo_metadata)<-c("minID",geo_meta_col)
  }
  
  source_info<-sapply(spatial_obj,function(x){x@id})
  objDat<-new("gevitDataObj",
              id  = dataID,
              type = "spatial",
              source = paste(source_info,collapse =";"),
              data = list(geometry=geo_data,
                          metadata = geo_metadata))
  return(objDat)
}


#***************************************************************
# INPUT PHYLOGENETIC TREE DATA
#***************************************************************
input_phyloTree<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,sepLabel = NA,metadataFile=NULL,...){
  #Make sure that tree has the right format to load
  if(!stringr::str_detect(file,"tree$|nwk$|tre$|newick$|nexus$")){
    stop("Phylogenetic tree file cannot be loaded. Please ensure that your tree has a .tree, .tre, .nwk, or .nexus format.")
  }
  
  #Try to load the tree
  tree<-treeio::read.tree(file=file)
  #not sure why the tryCatch below is not working
  #tree<-tryCatch(treeio::read.tree(file=file,...),
  #               error = function(e) stop("Could not load tree."))
  
  
  #Create a separate data frame the contains the tipData
  #If there is a bunch of information stored in the tip data
  #use the sep-label command to split it into a table
  if(!is.na(sepLabel)){
    #if it is a special character add escape
    if(grepl('[[:punct:]]', sepLabel))
      sepLabel<-paste0("\\",sepLabel)
    
    #Think more about how you want to handle the error messages
    tipDat<-tryCatch(do.call(rbind,strsplit(tree$tip.label,sepLabel)),
                     error = function(e) return(NULL))
  }else{
    tipDat<-tree$tip.label
  }
  
  #if the user added metadata load too
  if(!is.null(metadataFile)){
    metadata<-input_table(file = metadataFile,asObj=FALSE)
    
    #quick scan for column that matches node labels to make it easier to link data later
    tipLab<-tree$tip.label
    containsLabs<-apply(metadata,2,function(x){sum(tipLab %in% x)})
    if(max(containsLabs) == 0){
      warning("Current metadata file does not contain column matching tree labels")
      linkVar <- NULL
    }else{
      #Best guess of which columns contain the label information
      linkVar<-names(which(containsLabs == max(containsLabs)))
    }
  }
  #return the tree and parsed tip data (if that's what the user wants)
  #Is there more I should get out of a tree?
  
  if(asObj){
    objDat<-new("gevitDataObj",
                id  = paste("phyloTree",dataID,sep="_"),
                type = "phyloTree",
                source = file,
                data = list(tree=tree)
    )
    
    if(!is.null(tipDat))
      objDat@data$tipData<-tipDat
    
    if(!is.null(metadataFile)){
      objDat@source<-c(objDat@source,metadataFile)
      objDat@data$metadata<-metadata
      
      objDat@data$linkVar<-linkVar
    }
    
    
    return(objDat)
  }else{
    return(list(tree=tree,nodeDat = tipDat,metadata = metadata))
  }
}

#***************************************************************
# INPUT IMAGE DATA
#***************************************************************
input_image<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,...){
  img<-magick::image_read(path=file)
  
  #all images get resized so that they are mangeable to work with
  #arbitrarily set this 1000 pixels for max height width. Note that
  #rescaling will preserve the aspect ratio
  #smaller size also lets it load faster
  img<-magick::image_resize(img, "1000x1000")
  
  #get details for later
  imgDetails<-magick::image_info(img)
  
  #just return the image
  warning("To use this image, please be sure to have separate file that links the image to data in pixel space. If you would like to CREATE an annotation file, run the 'annotate_image' command.")
  
  if(asObj){
    objDat<-new("gevitDataObj",
                id  = paste("image",dataID,sep="_"),
                type = "image",
                source = file,
                data = list(data=img,imgDetails = imgDetails,metadata = NULL)
    )
    
    return(objDat)
  }else{
    return(list(data=img,imgDetails = imgDetails,metadata=NULL))
  }
}

#helper method to annotate FEATURES within an image
#'@export
annotate_image<-function(img = NULL,imgDetails=NULL,outfile = NULL,overwrite_meta=FALSE){
  # If user does not provide a file name, make one up
  if(is.null(outfile)){
    outfile="annotated_image_file.csv"
  }
  
  if(class(img) == "gevitDataObj"){
    annote_dat<-runApp(annotate_app(img@data$data,img@data$imgDetails))
    #
  }else{
    annote_dat<-shiny::runApp(annotate_image_app(img,imgDetails))
  }
  #cleaning up the annotatation data
  annote_dat<-data.frame(elemID =annote_dat[,1],
                         x = as.numeric(annote_dat[,2]),
                         y = as.numeric(annote_dat[,3]),
                         xmax = as.numeric(annote_dat[,4]),
                         ymax = as.numeric(annote_dat[,5]),
                         element_name = annote_dat[,6],
                         type = annote_dat[,7],
                         stringsAsFactors = FALSE)
  
  if(class(img) == "gevitDataObj"){
    if(!is.null(img@data$metadata) & !overwrite_meta){
      # ------- TO DO: IF IT CAN'T RBIND ..then ... DIE GRACEFULLY # -------
      #if there's existing metadata, add to it, don't overwrite it
      annote_dat<-rbind(img@data$metadata,annote_dat)
    }
    
    img@data$metadata<-annote_dat
    return(img)
  }else{
    return(annote_dat)
  }
}

#***************************************************************
# INPUT R DATA
# Essentially, input any data that is outputted by R.
# There are specific object files that are loaded
#***************************************************************
input_rdata<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,...){
  #Load data from R
  if(stringr::str_detect(file,"rdata$ | rda")){
    dat<-try(load(file))
  }else if(stringr::str_detect(file,"rds$")){
    dat<-try(readRDS(file))
  }else{
    stop("Right now, epiDRIVE can only read data that has been saved by the save() function (file extension rdata, or rda) or by the saveRDS() function (file extension, rds)")
  }
  
  objName<-dat
  
  #check how many entitities are being loaded and create gevitR objects for use
  #use get and assign to read and set new objec
  for(i in 1:length(dat)){
    tmp<-get(dat[[i]])
    tmpClass<-class(tmp)
    
    #give it a unique dataID
    #support specific types - not everything, otherwise that's a nightmare
    tmpClass<-switch(tmpClass,
                     "phylo" = "tree",
                     "data.frame" = "table",
                     "igraph" = "nodeLink",
                     "hclust" = "tree",
                     NULL)
    
    if(is.null(tmpClass)){
      warning(sprintf("'%s' not loaded - the '%s' data type is currently not supported by epiDRIVE",class(tmp),objName[i]))
    }
    
    
    #converting objects into the gevitObject so that it can be used
    #with the rest of the analysis
    
    tmp<-new("gevitDataObj",
             id  = paste(tmpClass,dataID,sep="_"),
             type = tmpClass,
             source = file,
             data = list(data = tmp)
    )
    
    #reassign the environment variable to the gevit object
    #does not return anything
    assign(objName[i],tmp)
  }
}

