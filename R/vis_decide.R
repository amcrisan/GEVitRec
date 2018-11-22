#***************************************************************
#
# Process what to visualize
#
#***************************************************************
#a helper function that adds items to a plot list
add_to_plot_list<-function(plotList,plotName=NA,plotItem=NA){

  plotList[[plotName]]<-list(plotName = plotName,
                             plotItem = plotItem,
                             accessoryInfo = NA)
 
 return(plotList)
}

# visualization that handles table data
# can return one or multiple charts, depending upon user specifications
# or lack there of
tabVis<-function(tab=NA,data=NA,dataMeta=NA,visChoices = NA){
  plotList<-NULL
  
  tabData<-dplyr::filter(dataMeta,dataSource == tab)
  
  #How many variables will be visualized
  if(is.na(visChoices)){
    #if you don't choose, everything is up for grabs!
    vizChoices = as.character(tabData$dataID)
  }
  num_var<-length(visChoices)
  
  #Now let's inspect what variables we have, and what should be visualized
  tabDataViz<-dplyr::filter(tabData,dataID %in% visChoices) %>%
    tidyr::separate(dataType,c("dataType","specialDataType"),sep=";",fill="right")
  
  #what to visualize... what to visualize...
  if(num_var == 1){
    #if you just have one variable, there's only a few things you can visualized
    #there is only one special datatype to care about here: date; because it should be an epicurve
    if(tabDataViz$specialDataType == "temporal"){
      plotList<-NULL
      plotList<-add_to_plot_list(plotList,"epicurve_1",plot_epicurve())
    }else{
      if(tabDataViz$specialDataType == "character"){
        #categorical variables get a bar chart
        plotList<-add_to_plot_list(plotList,"barchart_1",plot_barchart())
      }else{
        #continous variables get a histogram or a pdf
        plotList<-add_to_plot_list(plotList,"histogram_1",plot_histogram())
      }
    }
  }else if(num_var == 2){
    #if you just have two variables, again, you are similarly limited
    print("I am still working on this")
  }else{
    #now things get interesting
    print("I am still working on this")
  }
  
  return(plotList)
}

#***************************************************************
#
# Basic code for data visualizations
#
# EVENTUALLY, THIS WILL JUST PASS SPECIFICATIONS TO GEVITR 
#
#***************************************************************

plot_epicurve<-function(){
  dat<-data.frame(year= 2009:2018,
                  count = sample(0:100,10))
  p<-ggplot(dat,aes(x=year,y=count))+
    geom_bar(stat="identity")+
    theme_bw()
    
  return(p)
}

plot_barchart<-function(){
  dat<-data.frame(year= sample(letters,10),
                  count = sample(0:100,10))
  p<-ggplot(dat,aes(x=year,y=count))+
    geom_bar(stat="identity")+
    theme_bw()
  
  return(p)
}


plot_geomap<-function(){
  return(p("I am a fancy map - beep boop"))
}

plot_histogram<-function(){
  dat<-data.frame(contData<-runif(100))
  p<-ggplot(dat,aes(x=contData))+
    geom_histogram()+
    theme_bw()
  
  return(p)
}

plot_pdf<-function(){
  dat<-data.frame(contData<-runif(100))
  p<-ggplot(dat,aes(x=contData))+
    geom_density(fill="black")+
    theme_bw()
  
  return(p)
}