#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(stringr)
library(ggplot2)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(sp)
library(maptools)
library(dplyr)

# Data

evt_warn <- read.csv('Data/EvtWarn.csv')
rv_bsm <- read.csv('Data/RvBsm.csv')

host_devices <- list(
  "2004" = 2004,
  "2008" = 2008,
  "2017" = 2017,
  "2107" = 2107,
  "2147" = 2147,
  "2218" = 2218,
  "2233" = 2233,
  "2331" = 2331,
  "2348" = 2348,
  "2494" = 2494,
  "2496" = 2496,
  "2527" = 2527,
  "2533" = 2533,
  "2559" = 2559,
  "2584" = 2584,
  "2588" = 2588,
  "2627" = 2627,
  "2720" = 2720,
  "2804" = 2804,
  "2858" = 2858,
  "2936" = 2936,
  "2941" = 2941,
  "2969" = 2969,
  "2998" = 2998,
  "2999" = 2999)

events = list(
  'EEBL' = 0,
  'FCW (Forward Collision Warning)' = 1,
  'IMA (Intersection Movement Assist)' = 2,
  'BSW/LCW' = 3,
  'DNPW' = 4,
  'CLW' = 5,
  'N/A' = 6,
  'RSZW' = 7,
  'CSW' = 8,
  'RLVW' = 9,
  'PDA' = 10,
  'LTA' = 11,
  'EVA' = 12,
  'LSM' = 13
)

# Get host data per device
get_host <- function(device) {
  filepath <- paste0('Gen_Data/host_', toString(device), '.csv')
  return(read.csv(filepath))
}

# Helper Functions

# Filter the data on deviceID
filter_device <- function(df=NULL, device, host = FALSE) {
  if(host == TRUE) {
    df <- get_host(device)
    return(df[, c('Trip','Time', 'Latitude', 'Longitude', 'Speed', 'BrakeStatus', 'TurnSignal')])
  }
  return(df[df["Device"] == device,])
}

# Filter the data on Trip(s)
filter_trips <- function(df, trips) {
  return(df[df$Trip %in% trips,])
}

# Filter the data on Time(s)
filter_times <- function(df, times) {
  return(df[(df$Time >= times[1]) & (df$Time <= times[2]),])
}


# Color Functions

lines_pal <- colorFactor(
  palette = c('blue', 'green'),
  domain = c(0,1)
)

evt_pal <- colorFactor(
  palette = c("#a51d1d","#fc9300", "#ffbf6d", "#27a337", "purple", "#ffaff0","#4f4f4f", "#49acd1"),
  domain = c(0,1,2,3,9,10,11,12)
)

getColor <- function(df) {
  sapply(df$EventAppId, function(EventAppId) {
    if(EventAppId == 0) {
      "red"
    } else if(EventAppId == 1) {
      "orange"
    } else if(EventAppId == 2) {
      "beige"
    } else if(EventAppId == 3) {
      "green"
    } else if(EventAppId == 9) {
      "purple"
    } else if(EventAppId == 10) {
      "pink"
    } else if(EventAppId == 11) {
      "gray"
    } else if(EventAppId == 12) {
      "blue"
    } })
}

# Points To Lines (Not Original Function)
# Source Name: Rpubs
# Author: Kyle Walker
# Date: Feburary 27, 2015
# URL: https://rpubs.com/walkerke/points_to_line
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Honda Team 3"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      tabBox(title = "Trip View", 
             width = 8,
             tabPanel(title = 'Location',
                      leafletOutput("geoplot")),
             tabPanel(title = 'Speed',
                      plotOutput('speed')),
             tabPanel(title = 'Acceleration',
                      plotOutput('acceleration'))
      ),
      box(title = "Controls", 
          width = 4,
          uiOutput("choices"),
          fluidRow(
            box(width = 6,
                selectizeInput("deviceID",
                                  label = "Device ID",
                                  choices = host_devices,
                                  multiple=FALSE, 
                                  # options = list(
                                  #   placeholder = 'Select Device',
                                  #   onInitialize = I('function() { this.setValue(""); }'))
                                  selected = 2004)),
            box(width = 6,
              uiOutput("trip_choices"))),
          uiOutput("time_slider"),
          textOutput('number_of_events'),
          actionBttn('reset',
                     label = 'Reset',
                     block = TRUE,
                     style = 'material-flat',
                     size = 'sm')
      )
    ),
    fluidRow(
      tabBox(title = "Data", width = 12,
             tabPanel(title = "EvtWarn",
                      DTOutput({id = 'evt_warn_out'})),
             tabPanel(title = "Host",
                      DTOutput({id = 'host_out'})),
             tabPanel(title = "RvBsm",
                      DTOutput({id = 'rv_bsm_out'})))
    )
  )
)


server <- function(input, output) {
  ########### FUNCTIONS ##########################################
  # Load initial data
  initial_data <- reactive({
    if (input$deviceID %in% host_devices) {
      host_init <- get_host(input$deviceID)[c('Trip','Time', 'Latitude', 'Longitude', 'Speed', 'BrakeStatus', 'TurnSignal', 'LongAccel')]
      rv_init <- rv_bsm[rv_bsm["Device"] == input$deviceID,]
      evt_init <- evt_warn[evt_warn["Device"] == input$deviceID,]
      return(list(
        "host" = host_init,
        "rv" = rv_init,
        "evt" = evt_init
      ))
    }
  })
  
  # Filter data on Trip input
  data_f <- reactive({
#    if('go' %in% input$button) {
      host <- initial_data()$host
      rv <- initial_data()$rv
      evt <- initial_data()$evt
      
      host_f <- host[(host$Trip %in% input$trips),]
      rv_f <- rv[(rv$Trip %in% input$trips),]
      evt_f <- evt[(evt$Trip %in% input$trips),]
      return(list(
        "host" = host_f,
        "rv" = rv_f,
        "evt" = evt_f
      ))
#    }
  })
  
  # Filter data on trip and time
  data_ft <- reactive({
#    if('go' %in% input$button) {
      host <- data_f()$host
      rv <- data_f()$rv
      evt <- data_f()$evt
      
      host_ft <- host[(host$Time >= input$times[1]) & (host$Time <= input$times[2]),]
      rv_ft <- rv[(rv$Time >= input$times[1]) & (rv$Time <= input$times[2]),]
      evt_ft <- evt[(evt$Time >= input$times[1]) & (evt$Time <= input$times[2]),]
      return(list(
        "host" = host_ft,
        "rv" = rv_ft,
        "evt" = evt_ft
      ))
#    }
  })
  
  # Get first event in a trip with specified appID and alertlevel
  evt_warn_first <- reactive({
#    if('go' %in% input$button) {
      df <- data_ft()$evt %>%
        group_by(Trip, EventAppId, AlertLevel) %>%
        filter(Time == min(Time)) 
      return(df)
#    }
    
  })
  
  # get list of trips that are in all three data sources
  available_trips <- reactive({
      unique_trips <- intersect(unique(initial_data()$host$Trip), unique(initial_data()$rv$Trip))
      unique_trips <- intersect(unique_trips, unique(initial_data()$evt$Trip))
      return(unique_trips)
  })
  
  # turn points to lines for host
  make_host_lines <- reactive({
      cur_host <- data_ft()$host
      if (length(unique(cur_host$Trip)) == 1) {
        points_to_line(cur_host, 
                       long = "Longitude",
                       lat = "Latitude")
      }
      else if ((length(unique(cur_host$Trip)) > 1) & (length(cur_host) > 4)) {
        points_to_line(cur_host, 
                       long = "Longitude",
                       lat = "Latitude",
                       id_field = "Trip",
                       sort_field = "Time")
      } else {
        NULL
      }
  })
  
  # turn points to lines for rv_bsm
  make_rv_bsm_lines <- reactive({
      cur_rv <- data_ft()$rv
      if (length(unique(cur_rv$Trip)) == 1) {
        points_to_line(cur_rv, 
                       long = "Longitude",
                       lat = "Latitude")
      }
      else if ((length(unique(cur_rv$Trip)) > 1) & (length(cur_rv) > 4)) {
        points_to_line(cur_rv, 
                       long = "Longitude",
                       lat = "Latitude",
                       id_field = "Trip",
                       sort_field = "Time")
      } else {
        NULL
      }
  })
  
  ############ OUTPUTS ############################
  
  # Dataframe outputs
  output$evt_warn_out <- renderDT(data_ft()$evt[,c('Time', 'HvLatitude', 'HvLongitude', 'RvLatitude', 'RvLongitude', 'EventAppId', 'AlertLevel')])
  output$host_out <- renderDT(data_ft()$host)
  output$rv_bsm_out <- renderDT(data_ft()$rv)
  
  # list number of events in filtered data
  output$number_of_events <- renderText({paste0('Number of Events: ', length(data_ft()$evt$Time))})
  
  # Plot Outputs
  output$geoplot <- renderLeaflet({
    leaflet() %>% 
      setView(
        lng = -83.373, 
        lat = 40.232, 
        zoom = 10) %>%
      addTiles() %>%
      addLegend(
        colors = c('blue', 'green'),
        labels = c('Host', 'RvBsm'),
        position = "bottomleft",
        title = 'Vehicle Path') %>%        
      addLegend(
        colors = c("#a51d1d","#fc9300", "#ffbf6d", "#27a337", "purple", "#ffaff0","#4f4f4f", "#49acd1"),
        labels = c('EEBL', '<b>FCW</b>', '<b>IMA</b>', 'BSW/LCW', 
                   'RLVW', 'PDA', 'LTA', 'EVA'),
        position = "bottomright",
        title = 'Event Type')
  })
  output$speed <- renderPlot({
    plot(data_ft()$host$Time, data_ft()$host$Speed, type = "l", col = 'blue', xlab = 'Time (in centiseconds)', ylab = 'Speed')
    abline(v = data_ft()$evt$Time, col = 'red')
    title('Host Speed')
  })
  output$acceleration <- renderPlot({
    plot(data_ft()$host$Time, data_ft()$host$LongAccel, type = "l", col = 'blue', xlab = 'Time (in centiseconds)', ylab = 'Acceleration')
    abline(v = data_ft()$evt$Time, col = 'red')
    title('Host Acceleration')
  })
  
  ######## RENDERED IO ######################
  
  # Trip choices
  output$trip_choices <- renderUI({
    selectInput("trips", 
                label = "Trip Number", 
                choices = available_trips(), 
                multiple = TRUE,
                selectize = TRUE,
                selected = available_trips()[1]
    )
  })
  
  # data choices
  output$choices <- renderUI({
    checkboxGroupButtons('df_show', 
                         label = 'Show on Plot', 
                         choices = list(
                           'Host Device' = 'host_show',
                           'Remote Vehicle' = 'rv_bsm_show',
                           'Event Warnings' = 'evt_warn_show'),
                         selected = 'evt_warn_show',
                         justified = TRUE)
  })
  
  # time slider
  output$time_slider <- renderUI({
    sliderInput('times',
                label = 'Time Slider',
                min = min(initial_data()$host$Time),
                max = max(initial_data()$host$Time),
                value = c(min(initial_data()$evt$Time),
                          max(initial_data()$evt$Time)))
  })
  
  ########## EVENTS #####################################
  
  # Reset Button Action
  observeEvent(input$reset,{
    output$time_slider <- renderUI({
      sliderInput('times',
                  label = 'Time Slider',
                  min = min(initial_data()$host$Time),
                  max = max(initial_data()$host$Time),
                  value = c(min(initial_data()$evt$Time),
                            max(initial_data()$evt$Time)))
    })
    
    output$choices <- renderUI({
      checkboxGroupButtons('df_show', 
                           label = 'Show on Plot', 
                           choices = list(
                             'Host Device' = 'host_show',
                             'Remote Vehicle' = 'rv_bsm_show',
                             'Event Warnings' = 'evt_warn_show'),
                           selected = 'evt_warn_show',
                           justified = TRUE)
    })
    
    output$geoplot <- renderLeaflet({
      leaflet() %>% 
        setView(
          lng = -83.373, 
          lat = 40.232, 
          zoom = 10) %>%
        addTiles() %>%
        addLegend(
          colors = c('blue', 'green'),
          labels = c('Host', 'RvBsm'),
          position = "bottomleft",
          title = 'Vehicle Path') %>%        
        addLegend(
          colors = c("#a51d1d","#fc9300", "#ffbf6d", "#27a337", "purple", "#ffaff0","#4f4f4f", "#49acd1"),
          labels = c('EEBL', '<b>FCW</b>', '<b>IMA</b>', 'BSW/LCW', 
                     'RLVW', 'PDA', 'LTA', 'EVA'),
          position = "bottomright",
          title = 'Event Type')
    })
  })
  
  # Make Location plot reactive
  observe({
#    if(('go' %in% input$button)) {
      proxy <- leafletProxy('geoplot') 
      proxy %>% clearMarkers() %>% clearShapes()
      if (('evt_warn_show' %in% input$df_show)& !is.null(evt_warn_first())) {
        icons1 <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = getColor(evt_warn_first())
        )
        icons2 <- awesomeIcons(
          icon = 'close',
          iconColor = 'black',
          library = 'ion',
          markerColor = getColor(evt_warn_first())
        )
        proxy %>%
          addAwesomeMarkers(data = evt_warn_first(),
                            lng=~RvLongitude,
                            lat=~RvLatitude, 
                            icon = icons1,
                            popup = ~as.character(AlertLevel), 
                            labelOptions = labelOptions(
                              noHide = TRUE,
                              direction = "right"
                            )) %>%
          addAwesomeMarkers(data = evt_warn_first(),
                            lng=~HvLongitude,
                            lat=~HvLatitude, 
                            icon = icons2,
                            popup = ~as.character(AlertLevel), 
                            labelOptions = labelOptions(
                              noHide = TRUE,
                              direction = "left"
                            )) %>%
          addCircleMarkers(data = data_ft()$evt,
                           lng=~HvLongitude,
                           lat=~HvLatitude,
                           radius = 5,
                           stroke = FALSE,
                           fillOpacity = 1,
                           color = ~evt_pal(EventAppId),
                           popup = ~as.character(AlertLevel)) %>%
          addCircleMarkers(data = data_ft()$evt,
                           lng=~RvLongitude,
                           lat=~RvLatitude,
                           radius = 5,
                           stroke = FALSE,
                           fillOpacity = 1,
                           color = ~evt_pal(EventAppId),
                           popup = ~as.character(AlertLevel))
        if (('host_show' %in% input$df_show) & (!is.null(make_host_lines()))) {
          proxy %>% 
            addPolylines(data = make_host_lines(), color = "blue", opacity =0.75)
        } 
        if (('rv_bsm_show' %in% input$df_show) & !is.null(make_rv_bsm_lines())) {
          proxy  %>%
            addPolylines(data = make_rv_bsm_lines(), color = "red", opacity = 0.75)
        }
      } else {
        proxy %>% clearMarkers() %>% clearShapes()
      }
  #    }
    })
  
}
shinyApp(ui = ui, server = server)