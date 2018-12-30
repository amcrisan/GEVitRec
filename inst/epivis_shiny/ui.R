library(shiny)
library(shinydashboard)
library(leaflet)
library(epivis)

#-------------------------------------------------------------------------
# DASHBOARD HEADER
#-------------------------------------------------------------------------
header<-dashboardHeader(title="genepiDRIVE @ BCCDC")

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
                                          "Image",
                                          "Shape File",
                                          "FASTA",
                                          "VCF"))),
           column(3,
                  textInput("datTypeName",
                            label = "Provide a dataset name (Optional):",
                            value  = "")),
           column(6,
                  style = "margin-top: 25px;",
                  actionButton("addDataSource","Add Data Source"))
           )),
         br(),
         actionButton("loadData","Load Data")),
  tabItem("input_data_summary",
          h2("Data Input Summary"),
          p("Mostly to be used for testing purposes, it provides a summary of all the data read in, how it is categorized by the system, and finally, what data linkages the system has discovered"),
          tableOutput("dataSummaryTable"),
          tableOutput("dataLinkageTable")
          ),
  tabItem("data_vis",
          h2("Data Input"),
          p("Visualize data - currently a work in progress"),
          tabsetPanel(id = "visPanel",
                      type="pill",
                      tabPanel("Summary Overview",
                               p("Summary data visualization here")),
                               #plotOutput("summaryVisualization")),
                      tabPanel("Individual",
                               p("Individual data visualizations here"),
                               uiOutput("dataOptions"),
                               uiOutput("tableOptions"),
                               br(),
                               plotOutput("indVis"))
                      )
          ),
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
             menuItem("Input Data Summary",tabName = "input_data_summary"),
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