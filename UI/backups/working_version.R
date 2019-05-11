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

# Get host data per device
get_host <- function(device) {
  filepath <- paste0('Gen_Data/host_', toString(device), '.csv')
  return(read.csv(filepath))
}

# Helper Functions
filter_device <- function(df=NULL, device, host = FALSE) {
  if(host == TRUE) {
    df <- get_host(device)
    return(df[, c('Trip','Latitude', 'Longitude', 'Speed')])
  }
  return(df[df["Device"] == device,])
}

filter_trips <- function(df, trips) {
  return(df[df$Trip %in% trips,])
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Honda Team 3"),
  
  # Sidebar to select device
  dashboardSidebar(
    checkboxGroupInput("df_choices", 
                       label = "Data Source", 
                       choices = list(
                         "Host" = 'host_selected', 
                         "RvBSM" = 'rv_bsm_selected', 
                         "EvtWarn" = 'evt_warn_selected'
                       )
    ),
    hr(),
    uiOutput("device_choices"),
    uiOutput("trip_choices"),
    br(),
    br(),
    br(),
    br(),
    br(),
    hr(),
    actionButton("change_plot",
                 label="Plot",
                 width = "80%")
    
  ),
  # Show a plot of the generated distribution
  dashboardBody(
    fluidRow(
      box(title = "Location", width = 8, leafletOutput("geoplot")),
      tabBox(title = "someplot", width = 4,
             tabPanel(title = "Panel1",
                      verbatimTextOutput('errors')),
             tabPanel(title = "Panel2"))
      
    ),
    fluidRow(
      box(title = "Time Slider", width = 12)
    ),
    fluidRow(
      box(title = "More Inputs 1", width = 4),
      box(title = "More Inputs 2", width = 4),
      box(title = "More Inputs 3", width = 4)
    )
    
    
  )
)

server <- function(input, output) {
  # Reactive Value for user state
  state <- reactiveValues(
    "host" = FALSE,
    "rv_bsm" = FALSE,
    "evt_warn" = FALSE
  )
  data <- reactiveValues(
    "host" = NULL,
    "rv_bsm" = NULL,
    "evt_warn" = NULL
  )
  
  # Filter data by selected deviceID
  host_f <- reactive({
    filter_device(device = input$deviceID, host = TRUE)
  })
  evt_warn_f <- reactive({
    state$evt_warn <- FALSE
    filter_device(evt_warn, device = input$deviceID)
  })
  rv_bsm_f <- reactive({
    state$rv_bsm <- FALSE
    filter_device(rv_bsm, device = input$deviceID)
  })
  
  # Filter also for specified Trip
  host_ff <- reactive({
    df <- filter_trips(host_f(), input$trips)
    return(df)
  })
  evt_warn_ff <- reactive({
    df <- filter_trips(evt_warn_f(), input$trips)
    state$evt_warn <- TRUE
    return(df)
  })
  rv_bsm_ff <- reactive({
    df <- filter_trips(rv_bsm_f(), input$trips)
    state$rv_bsm <- TRUE
    return(df)
  })
  
  # Get list of available trips for selected deviceID
  available_trips <- reactive({
    unique_trips <- NULL
    if (input$deviceID %in% host_devices) {
      if ('host_selected' %in% input$df_choices) {
        unique_trips <- unique(host_f()$Trip)
      }
      if ('rv_bsm_selected' %in% input$df_choices) {
        rv_bsm_trips <- unique(rv_bsm_f()$Trip)
        if (is.null(unique_trips)) {
          unique_trips <- rv_bsm_trips
        }
        else {
          unique_trips <- intersect(unique_trips, rv_bsm_trips)
        }
      }
      if ('evt_warn_selected' %in% input$df_choices) {
        evt_warn_trips <- unique(evt_warn_f()$Trip)
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
  # Output Available Trips as checkbox input
  
  
  observeEvent(input$df_choices, {
    output$device_choices <- renderUI({
      selectizeInput("deviceID",
                     label = "Device ID",
                     choices = list(
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
                       "2999" = 2999),
                     multiple=FALSE, 
                     options = list(
                       placeholder = 'Select Device',
                       onInitialize = I('function() { this.setValue(""); }'))
      )
    })
  })
  observeEvent(input$deviceID, {
    output$trip_choices <- renderUI({
      selectizeInput("trips", 
                     label = "Trip Number", 
                     choices = available_trips(), 
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Select Trip(s)',
                       onInitialize = I('function() { this.setValue(""); }'))
      )
    })
  })
  
  output$errors <- renderText({toString(isolate(state$host))})
  
  
  
  #output$testplot <- renderPlot({
  #  ggplot(data = host_f(), aes(x = Speed)) + geom_histogram(col = 'blue') 
  
  #})
  output$geoplot <- renderLeaflet({
    leaflet() %>%
      setView(
        lng = -83.373, 
        lat = 40.232, 
        zoom = 12) %>%
      addTiles() 
  })
  
  observeEvent(input$change_plot, {
    if (state$host == TRUE) {
      leafletProxy("geoplot", data = data$host) %>%
        addCircleMarkers(
          lng=~Longitude,
          lat=~Latitude,
          color = 'blue') 
    }
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

