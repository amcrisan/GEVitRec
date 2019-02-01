#Generic object that contains GEViT specific data
#make list a little easier  when passing information around
setClass(
  "gevitDataObj",
  slots = c(
    id = "character",
    type = "character",
    source = "character",
    data = "list"
  )
)

#Method that tells you what items are packed into the data slot
setGeneric(
  "peekDataSources",
  function(object) {
    standardGeneric("peekDataSources")
  }
)

setMethod("peekDataSources",
          signature("gevitDataObj"),
          function(object){
            return(names(object@data))
          }
)

#Method that adds a new data
setGeneric(
  "addDataSource",
  function(object,newData,newDataName){
    standardGeneric("addDataSource")
  }
)

setMethod("addDataSource",
          signature("gevitDataObj","data.frame","character"),
          function(object,newData = NULL,newDataName = NULL){
            if(is.null(newDataName)){
              stop("Please provide a name for the new data")
            }
            object@data[[type]]<-newData
          })

#Method that extracts data list
setGeneric(
  "getAllData",
  function(object){
    standardGeneric("getAllData")
  }
)

setMethod("getAllData",
          signature("gevitDataObj"),
          function(object){
            return(object@data)
          }
)
