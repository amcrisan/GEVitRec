#----------------------------------------------------
# EPIVIS SHINY APP DATA INPUT *SUPPORT* METHODS
#
# Helpder function that support loading data. 
#----------------------------------------------------

#this function will add user interface elements based upon the type of data
addUI<- function(datType=NULL,datNum = NULL,datName = ""){
  uiOut<-div(id = paste0("dataSource",datNum),class="dataInput",
    fluidRow(
      column(9,
             if(datType == "Phylogenetic Tree"){
               ####### INPUT IS GENOMIC DATA
               # Upload Phylogenetic Tree and optional metadata
               tagList(h4(tagList(img(src="img/phylogeny.png",height="20px"), getName(datType,datName))),
                       shiny::fileInput(paste0("dataSource",datNum,"_Files"),
                                width = '100%', 
                                label = "Choose phylogenetic tree file to load (.nwk, .newick, .tree, .tre formats accepted)",
                                accept = c(".nwk",".tree",".newick",".tre")))
               
             }else if(datType %in% c("FASTA","VCF")){
               ####### INPUT IS GENOMIC DATA
               ## FOR THIS - CONSIDER HOW TO SWAP BETWEEN DIRECTORY TO UPLOAD AND FILE TO UPLOAD
               tagList(h4(tagList(img(src="img/genome.png",height="20px"), datType)),
                       shiny::fileInput(paste0("dataSource",datNum,"_Files"),
                                width = '100%', 
                                label = "Choose genomic file OR directory to upload (.vcf ,.vcf.gz, .fasta, and .fasta.gz format accepted)",
                                accept = c(".vcf",".vcf.gz",".fasta",".fasta.gz"),
                                multiple = TRUE)
               )
              }else if(datType == "Shape File"){
                ####### INPUT IS SPATIAL DATA
                tagList(h4(tagList(img(src="img/spatial.png",height="25px"), datType)),
                        shiny::fileInput(paste0("dataSource",datNum,"_Files"),
                                 width = '100%', 
                                 label = "Choose spatial file to upload (.sp format accepted)",
                                 accept = c(".sp"))
                )
              }else if(datType %in% c("Interior Map Image", "Image","Gel Image")){
                ####### INPUT IS IMAGE DATA
                tagList(switch(datType,
                               "Interior Map Image" = h4(tagList(img(src="img/interior_map.png",height="50px"), datType)),
                               "Image" = h4(tagList(img(src="img/image.png",height="20px"), datType)),
                               "Gel Image" = h4(tagList(img(src="img/genome.png",height="20px"), datType))
                               ),
                        shiny::fileInput(paste0("dataSource",datNum,"_Files"),
                                 width = '100%', 
                                 label = "Choose spatial file to upload (.jpeg,.jpg, .tiff, .png, and .pdf format accepted)",
                                 accept = c(".jpeg",".jpg",".tiff",".png",".pdf"))
                )
              }else{
              ####### INPUT IS LINE LIST / METADATA
                tagList(h4(tagList(img(src="img/spreadsheet.png",height="20px"), datType)),
                        shiny::fileInput(paste0("dataSource",datNum,"_Files"),
                                width = '100%', 
                                label = "Choose metadata file to load (.csv, . xls, .xlsx, .tsv accepted)",
                                accept = c(".csv",".xls",".xlsx",".tsv"))
                )
             }
             ),
      column(3,
             ####### BUTTON TO REMOVE DATA SOURCE
             style = "text-align: right;",
             shiny::actionButton(paste0("removeDataSource",datNum),label=tagList(shiny::icon("times"), "Remove Data Source")))
    ))

  return(uiOut)
}

getName<-function(datType = NULL,datName = ""){
  return(paste0(datType,ifelse(datName=="","",paste0(" - ",datName))))
}

#add all of the epivis dataSec files into a nice gevitr data object

makeGEVITRobj<-function(dataSrc=NA,liveStatus=NA){
  tmp<-apply(dataSrc,1,function(dat,live){
    datType<-as.character(dat[["dataType"]])
    if(live){
      datSource<-as.character(dat[["datapath"]]) #this is the code that needs to run in live
    }else{
      #####  v TMP TEST SOLN v ############
      source<-switch(datType,
                     "Phylogenetic Tree" = "/tree_data/",
                     "Line List" = "/table_data/" )
      #temporary soln
      datSource<-paste0(dataPath,source,as.character(dat[["name"]]))
      #####  ^ TMP TEST SOLN ^ ############
    }
    
    #convert data type from friendly external to more efficient internal
    datType<-switch(datType,
                    "Phylogenetic Tree" = "tree",
                    "Line List" = "table")
    
    #produces a gevitR data object!!!
    input_data(file=datSource,dataType=datType)
  },live=liveStatus)
  
  return(tmp)
}

