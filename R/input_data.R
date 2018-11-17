#Function that creates a unique ID for each data item
#code from: https://stackoverflow.com/questions/42734547/generating-random-strings
randID <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}


#Generic input function for inputing data, helper functions are in the back
#' @export
input_data<-function(file  = NA, dataType = NA, asObj=TRUE,desc = NA,...){
  #Only supports specific data types
  #TODO: data for sequence alignment
  if(!(dataType %in% c("tree","table","dna","spatial","image")))
    stop("Data type is not supported. Please choose one of : tree, table, dna, spatial, image. Use ?input_data to learn more about the different input types.")
  
  print(as.list(match.call()))
  
  #make a unique ID for each object
  dataID<-randID()
  
  switch(dataType,
         "table" = input_table(file,asObj,dataID,desc,...),
         "tree" = input_phyloTree(file,asObj,dataID,desc,...),
         "dna" = input_dna(file,asObj,dataID,desc,...),
         "spatial" = input_spatial(file,asObj,dataID,desc,proj4String,...),
         "image" = input_image(file,asObj,dataID,desc,...))
}


#Helper function: inputs tables
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

#Helper function : input dna
input_dna<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,fileType=NA,...){
  #check to see what kinds of files the user has
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
      output<-ifelse(fileType == "fasta",
                     toDNABIN(output,"fasta"),
                     toDNABIN(output,"vcf")
             )

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
  }else if(stringr::str_detect(file,".fasta$")){
    return("fasta")
  }else{
    return(NA)
  }
}

#helper function to create a DNA bin object
toDNABIN<-function(dat = NULL,datType=NA){
  #Likely a matrix
  if(datType=="vcf"){
    #generate alignment
    refSeq<-dplyr::distinct(dat,CHROM,POS,REF)
    grp<-unique(dat$SAMPID)
    mat<-matrix(".",length(grp),nrow(refSeq))
  
    for(i in 1:length(grp)){
      seq<-refSeq$REF
      tmp<-dplyr::filter(dat,SAMPID == grp[i])
      idx<-match(tmp$POS,refSeq$POS)
      
      seq[idx]<-tmp$ALT
      mat[i,]<-seq
    }
    rownames(mat)<-grp
    colnames(mat)<-paste(refSeq$CHROM,refSeq$POS,sep=":")
    
    #return DNA bin object
    return(ape::as.DNAbin(mat))
  }else if(datType == "fasta"){
    #likely fasta string
    samps<-as.character(dat$seq)
    names(samps)<-dat$SAMPID
    return(as.DNAbin.DNAStringSet(samps))
  }
  
}

#Helper function to input dna, which loads VCF data
#Initially, used VCFR because it's got a lot going on
#and data table was more general and versitile
#NOTE: ASSUMES ONE SAMPLE PER VCF
input_vcf<-function(file=NA,asObj=TRUE,filter=NA,...){
  #Read with data.table, rather than vcfR
  #vcfFile<-vcfR::read.vcfR(vcfFile)
  if(stringr::str_detect(file,".vcf.gz$")){
    tmp<-data.table::fread(paste0('zcat < ',file),skip="#")
  }else{
    tmp<-data.table::fread(file,skip="#")
  }
  colnames(tmp)<-gsub("#","",colnames(tmp))
  tmp$SAMPID<-rep(basename(file),nrow(tmp))
  
  #Filter the sequences if needed or if very large file (last part is TO DO)
  if(!is.na(filter)){
    classFilter<-class(filter)
    if(classFilter == "character"){
      #if character, filter all items that contain the word (i.e. PASS)
      print("Implement me")
    }else if(classFilter == "logical"){
      #if boolean, deploy our own basic one, then filter with word PASS
      print("Implemented me")
    }else{
      warning("Input to filter is not currently supported! Data is not filtered.")
    }
  }
  
  return(tmp[,c("SAMPID","CHROM","POS","REF","ALT"),on=c("ID")])
}

input_fasta<-function(file=NA,...){
  if(stringr::str_detect(file,".fasta.gz$")){
    tmp<-data.table::fread(paste0('zcat < ',file),skip="#")
  }else{
    tmp<-data.table::fread(file,stringsAsFactors = FALSE,header = FALSE)
  }
  #tmp<-ape::read.FASTA(file=file,type="DNA")
  idxSamp<-which(sapply(tmp,function(x){grepl(">",x)}))
  samps<-c()
  for(i in 1:length(idxSamp)){
    start = idxSamp[i]+1 #start *after* the sample ID
    end<-ifelse(i == length(idxSamp),nrow(tmp),idxSamp[i+1]-1)
    samps<-rbind(samps,paste0(sapply(tmp[start:end,"V1"],as.character),collapse = ""))
  }
  #rownames(samps)<-gsub(">","",tmp[idxSamp,"V1"])
  #tst<-as.DNAbin.DNAStringSet(samps)
  return(data.frame(seq=samps,SAMPID=gsub(">","",tmp[idxSamp,"V1"])))
}

#Helpfer function : input spatial data
input_spatial<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,...){
  
  if(!stringr::str_detect(file,"shp$")){
    stop("Currently, the input file only loads shapefiles. Is your map an image file? Please choose set dataType to 'image' in order to load it properly")
  }
  
  nc <- sf::st_read(file, quiet = TRUE)
  
  #convert everythign to same layer in event for multiple maps
  #doesn't alway work well
  nc<- sf::st_transform(nc, "+init=epsg:3857")
  
  if(asObj){
    objDat<-new("gevitDataObj",
                id  = paste("spatial",dataID,sep="_"),
                type = "spatial",
                source = file,
                data = list(geometry=nc))
    return(objDat)
  }else{
    return(nc)
  }
}

#Helpfer function: input tree
#' A helper function that reads in tree file data. Expecting either Newick or Nexus format
#'
#' @param file
#' @param desc
#' @param ...
#'
#' @return a phylo tree object
#'
#' @examples
input_phyloTree<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,sepLabel = NA,metadataFile=NULL,...){
  #Make sure that tree has the right format to load
  if(!stringr::str_detect(file,"tree$|nwk$|tre$|newick$|nexus$")){
    stop("Phylogenetic tree file cannot be loaded. Please ensure that your tree has a .tree, .tre, .nwk, or .nexus format.")
  }
  
  #Try to load the tree, if for whatever reason it can't be loaded, throw error
  tree<-treeio::read.tree(file=file)
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

#Helpder function : input_image
#' Input Image
#' A helper function for input_data
#' @param file
#' @param desc
#' @param ...
#'
#' @return
#' @examples
input_image<-function(file=NA,asObj=TRUE,dataID=NA,desc=NA,...){
  img<-magick::image_read(path=file)
  
  #all images get resized so that they are mangeable to work with
  #arbitrarily set this 1000 pixels for max height width. Note that
  #rescaling will preserve the aspect ratio
  #smaller size also lets it load faster
  img<-magick::image_resize(img, "1000x1000")
  
  #get details for later
  imgDetails<-capture.output(img)
  
  #cleaning up the image details
  imgDetails<-data.frame(headerInfo= factor(c("img",unlist(strsplit(trimws(imgDetails[1]),"\\s+")))),
                         values = c(file,unlist(strsplit(trimws(imgDetails[2]),"\\s+"))[-1]))
  
  imgDetails<-tidyr::spread(imgDetails,headerInfo,values)
  imgDetails$width<-as.numeric(as.character(imgDetails$width))
  imgDetails$height<-as.numeric(as.character(imgDetails$height))
  
  #just return the image
  warning("To use this image, please be sure to have separate file that links the image to data in pixel space. If you would like to CREATE an annotation file, run the 'annotate_image' command.")
  
  if(asObj){
    objDat<-new("gevitDataObj",
                id  = paste("image",dataID,sep="_"),
                type = "image",
                source = file,
                data = list(img=img,imgDetails = imgDetails)
    )
    
    return(objDat)
  }else{
    return(list(img=img,imgDetails = imgDetails))
  }
}

#helper method to annotate FEATURES within an image
#'@export
annotate_image<-function(img = NULL,imgDetails=NULL,outfile = NULL){
  # If user does not provide a file name, make one up
  if(is.null(outfile)){
    outfile="annotated_image_file.csv"
  }
  
  if(class(img) == "gevitDataObj"){
    annote_dat<-runApp(annotate_app(img@data$img,img@data$imgDetails))
    #
  }else{
    annote_dat<-runApp(annotate_image_app(img,imgDetails))
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
    img@data$annotate<-annote_dat
    return(img)
  }else{
    return(annote_dat)
  }
  
}
