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

rg_pal <- colorFactor(
  palette = c('green', 'red'),
  domain = c(0,1)
)

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
    p('Please select file sourcing options for dashboard display. Note that this is just for sourcing so that load times can be reduced when selecting views. Display options are on the right.'),
    hr(),
    checkboxGroupInput("df_choices", 
                       label = "Load Data", 
                       choices = list(
                         "Host" = 'host_selected', 
                         "RvBSM" = 'rv_bsm_selected', 
                         "EvtWarn" = 'evt_warn_selected'),
                       selected = c('host_selected', 'rv_bsm_selected', 'evt_warn_selected')),
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
          awesomeRadio('facet',
                       label = 'Facet',
                       choices = list(
                         'Brake Status' = 'BrakeStatus',
                         'Turn Signal' = 'TurnSignal'
                       ),
                       selected = 'BrakeStatus',
                       width = "100%"),
          checkboxGroupButtons('event_type',
                               label = 'Event Type',
                               choices = list(
                                 'Forward Collision Warning (FCW)' = 1,
                                 'Intersection Movement Assist (IMA)' = 2,
                                 'Other' = 20),
                               selected = c(1, 2, 20),
                               justified = TRUE,
                               direction = 'vertical'
          ),
          textOutput('number_of_events'),
          verbatimTextOutput('data_names'),
          verbatimTextOutput('device_name'),
          verbatimTextOutput('trip_names')
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
  # Reactive data by device
  data <- reactiveValues(
    "host" = filter_device(device = 2004, host = TRUE),
    "rv_bsm" = filter_device(rv_bsm, device = 2004),
    "evt_warn" = filter_device(evt_warn, device = 2004)
  )
  data_trips <- reactiveValues(
    "host" = NULL,
    "rv_bsm" = NULL,
    "evt_warn" = NULL
  )
  data_times<- reactiveValues(
    "host" = NULL,
    "rv_bsm" = NULL,
    "evt_warn" = NULL
  )
  
  # Updates the data for specific device id
  update_data <- reactive({
    if (input$deviceID %in% host_devices) {
      if ("host_selected" %in% input$df_choices) {
        data$host <- filter_device(device = input$deviceID, host = TRUE)
      }
      if ("rv_bsm_selected" %in% input$df_choices) {
        data$rv_bsm <- filter_device(rv_bsm, device = input$deviceID)
      }
      if ("evt_warn_selected" %in% input$df_choices) {
        data$evt_warn <- filter_device(evt_warn, device = input$deviceID)
      }
    }
  })
  
  update_data_trips <- reactive({
    if (input$deviceID %in% host_devices) {
      if ("host_selected" %in% input$df_choices) {
        data_trips$host <- filter_trips(data$host, input$trips)
      }
      if ("rv_bsm_selected" %in% input$df_choices) {
        data_trips$rv_bsm <- filter_trips(data$rv_bsm, input$trips)
      }
      if ("evt_warn_selected" %in% input$df_choices) {
        data_trips$evt_warn <- filter_trips(data$evt_warn, input$trips)
      }
    }
  })
  
  update_data_times <- reactive({
    if (input$deviceID %in% host_devices) {
      if ("host_selected" %in% input$df_choices) {
        data_times$host <- filter_times(data$host, input$times)
      }
      if ("rv_bsm_selected" %in% input$df_choices) {
        data_times$rv_bsm <- filter_times(data$rv_bsm, input$times)
      }
      if ("evt_warn_selected" %in% input$df_choices) {
        data_times$evt_warn <- filter_times(data$evt_warn, input$times)
      }
    }
  })
  
  
  # Filter also for specified Trip
  host_f <- reactive({
    filter_trips(data$host, input$trips)
  })
  rv_bsm_f <- reactive({
    filter_trips(data$rv_bsm, input$trips)
  })
  evt_warn_f <- reactive({
    filter_trips(data$evt_warn, input$trips)
  })
  
  # Filter for specified time
  host_ft <- reactive({
    df <- filter_times(host_f(), input$times)
    return(df)
  })
  rv_bsm_ft <- reactive({
    df <- filter_times(rv_bsm_f(), input$times)
    return(df)
  })
  evt_warn_ft <- reactive({
    df <- filter_times(evt_warn_f(), input$times)
    return(df)
  })
  
  # get start of event 
  evt_warn_first <- reactive({
    evt_warn_ft() %>%
      group_by(EventAppId, AlertLevel) %>%
      filter(Time == min(Time)) 
  })
  
  # Get list of available trips for selected deviceID
  available_trips <- reactive({
    unique_trips <- NULL
    if (input$deviceID %in% host_devices) {
      if ('host_selected' %in% input$df_choices) {
        unique_trips <- unique(data$host[,"Trip"])
      }
      if ('rv_bsm_selected' %in% input$df_choices) {
        rv_bsm_trips <- unique(data$rv_bsm[,"Trip"])
        if (is.null(unique_trips)) {
          unique_trips <- rv_bsm_trips
        }
        else {
          unique_trips <- intersect(unique_trips, rv_bsm_trips)
        }
      }
      if ('evt_warn_selected' %in% input$df_choices) {
        evt_warn_trips <- unique(data$evt_warn[,"Trip"])
        if (is.null(unique_trips)) {
          unique_trips <- evt_warn_trips
        }
        else {
          unique_trips <- intersect(unique_trips, evt_warn_trips)
        }
      }
    }
    return(unique_trips)
  })
  
  # Default Times
  default_times <- reactive({
    if (length(evt_warn_f()$Time) < 2) {
      return(c(
        min(data$evt_warn$Time), 
        max(data$evt_warn$Time)))
    } else {
      return(c(
        min(evt_warn_f()$Time), 
        max(evt_warn_f()$Time)))
    }
  })
  # Points to Lines
  make_host_lines <- reactive({
    cur_host <- host_ft()
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
  
  make_rv_bsm_lines <- reactive({
    cur_rv <- rv_bsm_ft()
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
  
  observeEvent(input$df_choices, {
    update_data()
  })
  
  observeEvent (input$deviceID, {
    update_data()
  })
  
  observeEvent (input$trips, {
    update_data_trips()
  })
  
  observeEvent (input$times, {
    update_data_trips()
    update_data_times()
  })
  
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
                min = min(data$host[,'Time']),
                max = max(data$host[,'Time']),
                value = default_times())
  })
  
  output$evt_warn_out <- renderDT(evt_warn_ft()[,c('Time', 'HvLatitude', 'HvLongitude', 'RvLatitude', 'RvLongitude', 'EventAppId', 'AlertLevel')])
  output$host_out <- renderDT(host_ft())
  output$rv_bsm_out <- renderDT(rv_bsm_ft())
  
  output$number_of_events <- renderText({paste0('Number of Events: ', length(evt_warn_ft()))})
  
  output$data_names <- renderText({input$df_choices})
  output$device_name <- renderText({input$deviceID})
  output$trip_names <- renderText({input$trips})
  
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
  
  # observe({
  #   proxy_legend <- leafletProxy('geoplot') 
  #   if ('host_show' %in% input$df_show) {
  #     if (input$facet == 'BrakeStatus') {
  #       proxy_legend <- proxy_legend %>%
  #         addLegend(pal = rg_pal,
  #                   values = c(0, 1),
  #                   labels = c('red -> 0', 'green -> 1'))
  #     }
  #   }
  # })
  
  observe({
    proxy_evt_warn <- leafletProxy('geoplot')
    if ('evt_warn_show' %in% input$df_show) {
      proxy_evt_warn <- proxy_evt_warn %>%
        addCircleMarkers(data = evt_warn_ft(),
                         layerId = 'hv_circles',
                         lng=~HvLongitude,
                         lat=~HvLatitude,
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 1,
                         color = ~evt_pal(EventAppId),
                         popup = ~as.character(AlertLevel)) %>%
        addCircleMarkers(data = evt_warn_ft(),
                         layerId = 'rv_circles',
                         lng=~RvLongitude,
                         lat=~RvLatitude,
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 1,
                         color = ~evt_pal(EventAppId),
                         popup = ~as.character(AlertLevel))
      proxy_evt_warn
    } else {
      proxy_evt_warn <- proxy_evt_warn %>% 
        removeMarker('hv_circles') %>%
        removeMarker('rv_circles')
      proxy_evt_warn
    }
  })
  observe({
    proxy_host <- leafletProxy('geoplot')
    if(!is.null(make_host_lines())) {
      if ('host_show' %in% input$df_show) {
        proxy_host <- proxy_host %>% 
          addPolylines(data = make_host_lines(), color = "blue", opacity =0.75, layerId = 'host_lines')
        proxy_host
      } else {
        proxy_host
      }
    }
  })
  
  observe({
    proxy_rv_bsm <- leafletProxy('geoplot')
    if(!is.null(make_rv_bsm_lines())) {
      if ('rv_bsm_show' %in% input$df_show) {
        proxy_rv_bsm %>%
          addPolylines(data = make_rv_bsm_lines(), color = "red", opacity = 0.75, layerId = 'rv_bsm_lines')
        proxy_rv_bsm
      } else{
        proxy_rv_bsm
      }
    }
  })
  
  observe({
    proxy_markers <- leafletProxy('geoplot')
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
        proxy_markers <- proxy_markers %>% 
          addAwesomeMarkers(data = evt_warn_first(),
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
          addAwesomeMarkers(data = evt_warn_first(),
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
        proxy_markers
      }
    }
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)