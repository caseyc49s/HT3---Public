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

lines_pal <- colorFactor(
  palette = c('blue', 'green'),
  domain = c(0,1)
)

evt_pal <- colorFactor(
  palette = c("#a51d1d","#fc9300", "#ffbf6d", "#27a337", "purple", "#ffaff0","#4f4f4f", "#49acd1"),
  domain = c(0,1,2,3,9,10,11,12)
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Honda Team 3"),
  
  # Sidebar to select device
  dashboardSidebar(
    h3('Sourcing Options'),
    p('Please select device and trips for dashboard to display.'),
    hr(),
    selectizeInput("deviceID",
                   label = "Device ID",
                   choices = host_devices,
                   multiple=FALSE, 
                   # options = list(
                   #   placeholder = 'Select Device',
                   #   onInitialize = I('function() { this.setValue(""); }'))
                   selected = 2004
    ),
    uiOutput("trip_choices"),
    hr()
  ),
  # Show a plot of the generated distribution
  dashboardBody(
    fluidRow(
      tabBox(title = "Trip View", 
             width = 8,
             tabPanel(title = 'Location',
                      leafletOutput("geoplot")),
             tabPanel(title = 'Speed',
                      renderPlot('speed')),
             tabPanel(title = 'Acceleration',
                      renderPlot('acceleration')),
             tabPanel(title = 'Yaw Rate',
                      renderPlot('yaw'))
      ),
      box(title = "Controls", 
          width = 4,
          checkboxGroupButtons('df_show', 
                               label = 'Show on Plot', 
                               choices = list(
                                 'Host Device' = 'host_show',
                                 'Remote Vehicle' = 'rv_bsm_show',
                                 'Event Warnings' = 'evt_warn_show'),
                               selected = 'evt_warn_show',
                               justified = TRUE),
          textOutput('number_of_events')
      )
    ),
    fluidRow(
      box(title = 'Time Slider',
          width = 12,
          uiOutput("time_slider"))
    ),
    fluidRow(
      tabBox(title = "Data", width = 12,
             tabPanel(title = "EvtWarn",
                      DTOutput({id = 'evt_warn_out'})),
             tabPanel(title = "Host",
                      DTOutput({id = 'host_out'})),
             tabPanel(title = "RvBsm",
                      DTOutput({id = 'rv_bsm_out'})))
    ),
    fluidRow(
      box(title = "More Inputs 1", width = 4),
      box(title = "More Inputs 2", width = 4),
      box(title = "More Inputs 3", width = 4)
    ))
)

server <- function(input, output) {
  initial_data <- reactive({
    if (input$deviceID %in% host_devices) {
      host_init <- get_host(input$deviceID)[c('Trip','Time', 'Latitude', 'Longitude', 'Speed', 'BrakeStatus', 'TurnSignal')]
      rv_init <- rv_bsm[rv_bsm["Device"] == input$deviceID,]
      evt_init <- evt_warn[evt_warn["Device"] == input$deviceID]
      return(list(
        "host" = host_init,
        "rv" = rv_init,
        "evt" = evt_init
      ))
    }
  })
  return(df[(df$Time >= times[1]) & (df$Time <= times[2]),])
  
  data_f <- reactive({
    host <- get_host(input$deviceID)[c('Trip','Time', 'Latitude', 'Longitude', 'Speed', 'BrakeStatus', 'TurnSignal')]
    rv <- rv_bsm[rv_bsm["Device"] == input$deviceID,]
    evt <- evt_warn[evt_warn["Device"] == input$deviceID]
    
    host_f <- host[, host$Trip %in% input$trips]
    rv_f <- rv[, rv$Trip %in% input$trips]
    evt_f <- evt[, evt$Trip %in% input$trips]
    return(list(
      "host" = host_f,
      "rv" = rv_f,
      "evt" = evt_f
    ))
  })
  
  data_ft <- reactive({
    host <- get_host(input$deviceID)[c('Trip','Time', 'Latitude', 'Longitude', 'Speed', 'BrakeStatus', 'TurnSignal')]
    rv <- rv_bsm[rv_bsm["Device"] == input$deviceID,]
    evt <- evt_warn[evt_warn["Device"] == input$deviceID]
    
    host_ft <- host[(host$Time >= input$times[1]) & (host$Time <= input$times[2]), host$Trip %in% input$trips]
    rv_ft <- rv[(rv$Time >= input$times[1]) & (rv$Time <= input$times[2]), rv$Trip %in% input$trips]
    evt_ft <- evt[(evt$Time >= input$times[1]) & (evt$Time <= input$times[2]), evt$Trip %in% input$trips]
    return(list(
      "host" = host_ft,
      "rv" = rv_ft,
      "evt" = evt_ft
    ))
  })
  
  evt_warn_first <- reactive({
    df <- data_ft()$first %>%
      group_by(EventAppId, AlertLevel) %>%
      filter(Time == min(Time)) 
    return(df)
  })
  
  available_trips <- reactive({
    unique_trips <- intersect(unique(initial_data()$host$Trip), unique(initial_data()$rv$Trip))
    unique_trips <- intersect(unique_trips, unique(initial_data()$evt[,"Trip"]))
    return(unique_trips)
  })
  
  make_host_lines <- reactive({
    try({
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
  })
  
  make_rv_bsm_lines <- reactive({
    try({
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
  })
  
  output$evt_warn_out <- renderDT(data_ft()$evt[,c('Time', 'HvLatitude', 'HvLongitude', 'RvLatitude', 'RvLongitude', 'EventAppId', 'AlertLevel')])
  output$host_out <- renderDT(data_ft()$host)
  output$rv_bsm_out <- renderDT(data_ft()$rv)
  
  output$number_of_events <- renderText({paste0('Number of Events: ', length(data_ft()$host))})
  
  # Default Times
  # default_times <- reactive({
  #   if (length(evt_warn_f()$Time) < 2) {
  #     return(c(
  #       min(initial_data()$evt$Time),
  #       max(data$evt_warn$Time)))
  #   } else {
  #     return(c(
  #       min(evt_warn_f()$Time),
  #       max(evt_warn_f()$Time)))
  #   }
  # })
  
  output$trip_choices <- renderUI({
    selectInput("trips", 
                label = "Trip Number", 
                choices = available_trips(), 
                multiple = TRUE,
                selectize = FALSE,
                size = 10
    )
  })
  
  output$time_slider <- renderUI({
    sliderInput('times',
                label = 'Time Slider',
                min = min(data_f()$host$Time),
                max = max(data_f()$host$Time),
                value = c(min(data_f()$evt$Time),
                          max(data_f()$evt$Time)))
  })
  map <- leaflet() %>% 
    setView(
      lng = -83.373, 
      lat = 40.232, 
      zoom = 12) %>%
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
  
  output$geoplot <- renderLeaflet({
    map
  })
  
  observe({
    host_data <- data_ft()$host
    if ('evt_warn_show' %in% input$df_show) {
      leafletProxy('geoplot', data = host_data) %>%
        addCircleMarkers(layerId = 'hv_circles',
                         lng=~HvLongitude,
                         lat=~HvLatitude,
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 1,
                         color = ~evt_pal(EventAppId),
                         popup = ~as.character(AlertLevel)) %>%
        addCircleMarkers(layerId = 'rv_circles',
                         lng=~RvLongitude,
                         lat=~RvLatitude,
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 1,
                         color = ~evt_pal(EventAppId),
                         popup = ~as.character(AlertLevel))
    }
  })
  
  observe({
    if(!is.null(make_host_lines())) {
      if ('host_show' %in% input$df_show) {
        leafletProxy('geoplot', data = make_host_lines())  %>% 
          addPolylines(color = "blue", opacity =0.75, layerId = 'host_lines')
      }
    }
  })
  
  observe({
    if(!is.null(make_rv_bsm_lines())) {
      if ('rv_bsm_show' %in% input$df_show) {
        leafletProxy('geoplot', data = make_rv_bsm_lines())  %>%
          addPolylines(color = "red", opacity = 0.75, layerId = 'rv_bsm_lines')
      }
    }
  })
  
  observe({
    if(!is.null(evt_warn_first())) {
      evt_warn_ff <- evt_warn_first()
      
      icons1 <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(evt_warn_ff)
      )
      icons2 <- awesomeIcons(
        icon = 'close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(evt_warn_ff)
      )
      if ('evt_warn_show' %in% input$df_show) {
        leafletProxy('geoplot', data = evt_warn_first()) %>% 
          addAwesomeMarkers(
            layerId = 'rv_mark',
            lng=~RvLongitude,
            lat=~RvLatitude, 
            icon = icons1,
            popup = ~as.character(AlertLevel), 
            label = "Remote",
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "right"
            )) %>%
          addAwesomeMarkers(
            layerId = 'hv_mark',
            lng=~HvLongitude,
            lat=~HvLatitude, 
            icon = icons2,
            popup = ~as.character(AlertLevel), 
            label = "Host",
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "left"
            ))
      }
    }
  })
  
  
}
shinyApp(ui = ui, server = server)