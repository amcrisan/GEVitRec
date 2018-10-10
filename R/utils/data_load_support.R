#----------------------------------------------------
# EPIVIS SHINY APP DATA INPUT *SUPPORT* METHODS
#
# Helpder function that support loading data
#----------------------------------------------------

addUI<- function(datType=NULL,datNum = NULL){
  uiOut<-div(id = paste0("dataSource",datNum),class="dataInput",
    h5(datType),
    fluidRow(
      column(8,
             shiny::fileInput(paste0("datType",inputDataValues), width = '100%', label = "choose file to load")),
      column(4,
             style = "margin-top: 25px;",
             shiny::actionButton(paste0("removeDataSource",inputDataValues$numDataSources),label=tagList(shiny::icon("times"), "Remove Data Source")))
    ))
}