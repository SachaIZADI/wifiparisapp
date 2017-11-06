
library(shiny)
library(dplyr)
library(wifiparis)
library(plotly)
library(lubridate)
library(leaflet)
library(treemap)
library(d3treeR)
library(shinyjs)




shinyServer(function(input, output) {
  
  
  
  output$`Date scope` <- renderUI({dateRangeInput(inputId = "Date scope", label = "Date scope", language = "fr", start = "2016-01-01", end  = "2016-01-02")})
  output$`Duration scope` <- renderUI({sliderInput("Duration scope", "Duration scope", min = 0, max = 7200, value = c(0, 7200))})
  
  
  output$Borought <- renderUI({
    selectizeInput(inputId = "Borought", label = "Borought",  multiple = TRUE, selected = "All", choices = c("All", "1","2","3","4","5","6","7","8","9","10","11","12","13","14",
                                                                                                             "15","16","17","18","19","20"), options = list(plugins =  list('remove_button', 
                                                                                                                                                                            'restore_on_backspace',
                                                                                                                                                                            'drag_drop'),  
                                                                                                                                                            placeholder = 'select a Borought') )
  })    
  
  
  
  output$`Category site` <- renderUI({
    
    
    
    if(is.null(input$Borought))
      return()
    
    else if ("All" %in% input$Borought)
      selectizeInput(inputId= "Category site", label = "Category site",  multiple = TRUE, selected = "All", choices = c("All", "Bibliothèque","Espace ouvert/vert","Maison d'accompagnement","Centre de sport et bien être","Centre d'animation et culturel","Mairie"), options = list(plugins = 
                                                                                                                                                                                                                                                                                         list('remove_button', 'restore_on_backspace',
                                                                                                                                                                                                                                                                                              'drag_drop'), placeholder = 'select a site category'))
    else
      
      
      selectizeInput(inputId= "Category site", label = "Category site",  multiple = TRUE, selected = "All", choices = c("All", as.list(unique(Data_Viz_1 %>%  filter (Ardt %in% input$Borought) %>% select(category_site)))), options = list(plugins = 
                                                                                                                                                                                                                                               list('remove_button', 'restore_on_backspace',
                                                                                                                                                                                                                                                    'drag_drop'),placeholder = 'select a site category'))
  })
  
  
  output$Site <- renderUI({
    
    if(is.null(input$Borought) || is.null(input$`Category site`))
      return()
    
    new_list_2 <- Data_Viz_1
    
    if (!"All" %in% input$Borought)
      new_list_2 <- new_list_2 %>% filter(Ardt %in% input$Borought)
    if (!"All" %in% input$`Category site`)
      new_list_2 <- new_list_2 %>% filter(category_site %in% input$`Category site`)
    
    selectizeInput("Site", "Site", multiple = TRUE, selected = "All",  choices = c("All", unique(new_list_2$site)), options = list(plugins = 
                                                                                                                                     list('remove_button', 'restore_on_backspace',
                                                                                                                                          'drag_drop'),placeholder = 'select a site'))
    
    
  })
  
  output$Country <- renderUI({
    
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) )
      return()
    
    new_list_3 <- Data_Viz_1
    
    if (!"All" %in% input$Borought)
      new_list_3 <- new_list_3 %>% filter(Ardt %in% input$Borought)
    if (!"All" %in% input$`Category site`)
      new_list_3 <- new_list_3 %>% filter(category_site %in% input$`Category site`)
    if (!"All" %in% input$Site)
      new_list_3 <- new_list_3 %>% filter(site %in% input$Site)
    
    
    selectizeInput("Country", "Country", multiple = TRUE, selected = "All",choices = c("All", unique(new_list_3$Country)), options = list(plugins = 
                                                                                                                                            list('remove_button', 'restore_on_backspace',
                                                                                                                                                 'drag_drop'),placeholder = 'select a country'))
    
    
  })
  
  output$`Category device` <- renderUI({selectizeInput("Category device", "Category device", selected = "All", multiple = TRUE, choices = c("All", "smartphone", "tablet", "computer"), options = list(plugins = 
                                                                                                                                                                                                         list('remove_button', 'restore_on_backspace',
                                                                                                                                                                                                              'drag_drop'), placeholder = 'select a device category'))})
  
  output$`Analysis axis` <- renderUI({selectizeInput("Analysis axis", "Analysis axis", multiple = FALSE, choices = c("None", "Ardt","site", "category_site", "category_device", "Country"), options = list(plugins = 
                                                                                                                                                                                                             list('remove_button', 'restore_on_backspace',
                                                                                                                                                                                                                  'drag_drop'), placeholder = 'select an analysis axis'))})
  
  
  Datatable <- reactive({
    
    start = input$`Date scope`[1]
    end =  input$`Date scope`[2]
    start_dur = input$`Duration scope`[1]
    end_dur = input$`Duration scope`[2]
    
    Data_Viz_1_Filter<- Data_Viz_1 %>%  filter(as.Date(start_time) >=start & as.Date(start_time)<=end) %>% filter(duration < end_dur) %>% 
      filter(duration > start_dur)
    
    if (!"All" %in% input$Borought ){
      Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(Ardt %in% input$Borought )
    }
    if (!"All" %in% input$`Category site`){
      Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(category_site %in% input$`Category site`)
    }
    if (!"All" %in% input$Site){
      Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(site %in% input$Site)
    }
    if (!"All" %in% input$Country){
      Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(Country %in% input$Country)
    }
    if (!"All" %in% input$`Category device`){
      Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(category_device %in% input$`Category device`)
    }
    
    Data_Viz_1_Filter$start_time <- as.character(as.Date(Data_Viz_1_Filter$start_time))
    Data_Viz_1_Filter$duration <- as.character(Data_Viz_1_Filter$duration)
    Data_Viz_1_Filter$Ardt <- as.character(Data_Viz_1_Filter$Ardt)
    
    return(Data_Viz_1_Filter)
    
    
    
    
  })
  
  
  
  output$data_table <- DT::renderDataTable(DT::datatable({
    
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) || is.null(input$`Category device`)
       || is.null(input$Country))
      return()
    
    Datatable()
    
  }))
  
  output$downloadCsv <- downloadHandler(
    filename = "wifi_paris_data.csv",
    content = function(file) {
      write.csv(Datatable(), file)
    },
    contentType = "text/csv"
  )
  
  start <- reactive ({ lubridate::ymd(input$`Date scope`[1])})
  end <- reactive ({ lubridate::ymd(input$`Date scope`[2])})
  duration_min<-  reactive({input$`Duration scope`[1]})  
  duration_max<-  reactive({input$`Duration scope`[2]}) 
  districts<- reactive({input$Borought}) 
  cat_sites<- reactive({input$`Category site`}) 
  sites<- reactive({input$Site}) 
  countries<- reactive({input$Country}) 
  devices<- reactive({input$`Category device`}) 
  analysis_axis<- reactive({input$`Analysis axis`})  
  
  
  
  output$Viz1 <- renderPlotly({
    
   
    
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) || is.null(input$`Category device`)
       || is.null(input$Country) || is.null(input$`Analysis axis`))
      return()
    
    
    
    return(Viz1_plot(start(), end(), duration_min(), duration_max(), districts(), cat_sites(), sites(), countries(), 
                     devices(), analysis_axis(), FALSE))
    
    
  })
  
  output$Viz2 <- renderPlotly({
    
    
    
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) || is.null(input$`Category device`)
       || is.null(input$Country) || is.null(input$`Analysis axis`))
      return()
    
 
    
    return(Viz1_plot(start(), end(), duration_min(), duration_max(), districts(), cat_sites(), sites(), countries(), 
                     devices(), analysis_axis(), TRUE))
    
  })
  
  output$mymap <- renderLeaflet({
    
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) || is.null(input$`Category device`)
       || is.null(input$Country) )
      return()
    
   
    
    return(Map_plot(start(), end(), duration_min(), duration_max(), districts(), cat_sites(), sites(), countries(), devices()))
    
    
  })
  
  
  
  output$Viz3 <- renderD3tree3({
    
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) || is.null(input$`Category device`)
       || is.null(input$Country) || is.null(input$`Analysis axis`))
      return()
    
  
    
    
    
    return (Viz3_plot(start(), end(), duration_min(), duration_max(), districts(), cat_sites(), sites(), countries(), devices(),
                      analysis_axis(),FALSE))
    
    
    
  })
  
  output$Viz4 <- renderD3tree3({
    
  
    if(is.null(input$Borought) || is.null(input$`Category site`) || is.null(input$Site) || is.null(input$`Category device`)
       || is.null(input$Country) || is.null(input$`Analysis axis`))
      return()
    

    
    return (Viz3_plot(start(), end(), duration_min(), duration_max(), districts(), cat_sites(), sites(), countries(), devices(),
                      analysis_axis(),TRUE))
    
    
    
  })
  
  
})


# Ajouter un bouton reset filters
# Ajouter un bouton pour generer les visualisations une fois le filtre choisi
# Regler probleme date lien avec les visu
# faire des sous fenetre avec les images agrandies
# probleme avec viz 2



