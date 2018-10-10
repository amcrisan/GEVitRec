library(shiny)
library(shinydashboard)
library(leaflet)
library(epivis)


#-------------------------------------------------------------------------
# DASHBOARD HEADER
#-------------------------------------------------------------------------
header<-dashboardHeader(title="epivis @ BCCDC")

#-------------------------------------------------------------------------
# MAIN DASHBOARD BODY
#-------------------------------------------------------------------------
body<-dashboardBody(
  includeCSS("www/style.css"),
  includeScript("www/app.js"),
  #Tab Item correspond to sidebar menu items
  tabItems(
  tabItem("input_data",
         h2("Data Input"),
         p("Load data sources to visualize. First choose the type of data that you intend to upload, then click on the 'upload' button. Next, choose the file to upload and (if it is available) any additional metadata files associated with that data type."),
         # Note to self: Made this a DIV to make it easier to automatically add UI elements on the fly underneath it
         div(id = "dataInputOptions",
         fluidRow(
           column(width = 3,
                  selectInput("datType",
                              label = "Choose a data type",
                              selected=NULL,
                              multiple=FALSE,
                              choices = c("Phylogenetic Tree",
                                          "Line List",
                                          "Interior Map Image",
                                          "Gel Image",
                                          "Image",
                                          "Spatial",
                                          "Genomic"))),
           column(3,
                  textInput("datTypeName",
                            label = "Provide a dataset name (Optional):",
                            value  = "")),
           column(6,
                  style = "margin-top: 25px;",
                  actionButton("addDataSource","Add Data Source"))
           ))),
         actionButton("loadData","Load Data")),
  tabItem("data_vis",
          p("To do - visualization"),
          plotOutput("distPlot")),
  tabItem("data_analytics",
          p("To do - analytics")),
  tabItem("output_reports",
          p("To do - reports"))
  )
)

#-------------------------------------------------------------------------
# SIDEBAR ELEMENTS
#-------------------------------------------------------------------------
sidebar<-dashboardSidebar(
 #sidebar menu
 sidebarMenu(id = "sideBarMenuOptions",
             menuItem("Input Data", tabName = "input_data"),
             menuItem("Visualization", tabName = "data_vis"),
             menuItem("Analytics", tabName = "data_analytics"),
             menuItem("Reports", tabName = "output_reports")),
 #dummy filtration menu for initial building only
 conditionalPanel("input.sidebarMenuOptions == 'data_vis'",
   sliderInput("bins",
             "Number of bins:",
             min = 1,
             max = 50,
             value = 30))
)



#-------------------------------------------------------------------------
# PUTTING THE DASHABORD ALL TOGETHER
#-------------------------------------------------------------------------

dashboardPage(
  header,
  sidebar,
  body,
  skin="black"
)