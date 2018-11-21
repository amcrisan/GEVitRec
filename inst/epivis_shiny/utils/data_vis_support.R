#----------------------------------------------------
# EPIVIS SHINY APP DATA VIS *SUPPORT* METHODS
#
# Helpder function that support visualizing the data
#----------------------------------------------------

plot_decider<-function(dataVis = NA,allDataVis=NA){
  #Get the data type of the individual data source
  itemVis<-dplyr::filter(allDataVis$allObjMeta,dataID == dataVis)
  
  #If it's a table - do this
  if(itemVis$dataType == "table"){
    return(p("I AM A TABLE"))
  }else{
    
    #If it's not a table - do everything else
    mainComponent<-p(as.character(itemVis$dataType))
    
    #Is there a way to connect this data source to others?
    #If yes, provide combination options
    conGraphNodes<-unique(c(allDataVis$varComp$item_one,allDataVis$varComp$item_two))
    if(as.character(itemVis$dataID) %in% conGraphNodes ){
      accessoryComponents<-p("SIDE HUSTLE")
    }
  
    #Lay this bad-boy out as two columns
    uiOut<-div(
      column(8,
             mainComponent),
      column(4,
             accessoryComponents)
    )
    
    return(uiOut)
  }
  
  #Output the resulting data visualization
}