#----------------------------------------------------
# EPIVIS SHINY APP DATA VIS *SUPPORT* METHODS
#
# Helpder function that support visualizing the data
#----------------------------------------------------

#main function that decides what kind of data visualization to produce
plot_decider<-function(dataVis = NA,data=NA,dataMeta=NA,visChoices=NULL){
  #Get the data type of the individual data source
  itemVis<-dplyr::filter(dataMeta,dataID == dataVis)
  
  mainComponent<-NULL
  accessoryComponents<-NULL
  
  #If it's a table - do this
  if(itemVis$dataType == "table"){
    mainComponent<-tabVis(as.character(itemVis$dataID),data,dataMeta,visChoices)[[1]]$plotItem
    #mainComponent<-p("AM A THING")
    accessoryComponents<-p("SIDE HUSTLE")
  }else{
    #If it's not a table - do everything else
    mainComponent<-p(as.character(itemVis$dataType))
    
    #Is there a way to connect this data source to others?
    #If yes, provide combination options
    conGraphNodes<-unique(c(allDataVis$varComp$item_one,allDataVis$varComp$item_two))
    if(as.character(itemVis$dataID) %in% conGraphNodes ){
      accessoryComponents<-p("SIDE HUSTLE")
    }
  }
  
  #uiOut<-div(
  #  column(8,
  #         mainComponent),
  #  column(4,
  #         accessoryComponents)
  #)
  
  #Lay this bad-boy out as two columns
  #return(uiOut)
  return(mainComponent)
  #Output the resulting data visualization
}
