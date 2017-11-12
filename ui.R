library(shinydashboard)
library(ggplot2)
library(plotly)
library(leaflet)
library(treemap)
library(d3treeR)
library(shinycssloaders)
library(shinyjs)





  header <- dashboardHeader(title = "Paris Wifi")

  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Raw data", tabName = "Raw_data", icon = icon("database")),
      menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/amirbenmahjoub/wifiparisapp")
      
    ),

    uiOutput("Date scope"),
    uiOutput("Duration scope"),
    uiOutput("Borought"),
    uiOutput("Category site"),
    uiOutput("Site"),
    uiOutput("Country"),
    uiOutput("Category device"),
    uiOutput("Analysis axis"),
    actionButton(inputId = "go", label = "plot")
    
    

  )


  body <- dashboardBody(
    
    

    tabItems(
      
      tabItem("dashboard", 
              
              
              fluidRow(
                
                
                tabBox(
                  
                  title = "Paris map",
                  id = "tabset2", height = "450px", width = 12,
                  tabPanel("Wifi sites map", withSpinner(leafletOutput("mymap")))
                )
                
                
                
              ),
              
              
              
              fluidRow(
                
                
                       tabBox(
                         
                         title = "Number of connexions trend",
                         id = "tabset1", height = "250px", width = 6,
                         tabPanel("Without smoothering", withSpinner(plotlyOutput('Viz2'))),
                       tabPanel("With smoothering", withSpinner(plotlyOutput('Viz1')))
                ),
                
                
                tabBox(title = "Number of connexions treemap",
                       id = "tabset3", height = "250px", width = 6,
                       tabPanel("Without hierarchy", withSpinner(d3tree3Output('Viz3'))) ,
                       tabPanel("With hierarchy", withSpinner(d3tree3Output('Viz4'))))
                       
                       
                        
                       )
                       
                       
                       
                
                
                
      ),
      

      tabItem("Raw_data", withSpinner(DT::dataTableOutput("data_table")), downloadButton("downloadCsv", "Download as CSV"))
      
    )
    )



 dashboardPage(header,sidebar,body)





