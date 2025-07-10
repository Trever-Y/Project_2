#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Major Weather Events"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("book")),
      menuItem("Data Download", tabName = "download", icon = icon("archive")),
      menuItem("Data Exploration", tabName = "exploration", icon = icon("binoculars"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              titlePanel("About"),
              h1("The purpose of this app is to easily compare rainfall and wind speeds of recent major storms."),
              h1("These data are historic forecast data from the Open-Meteo API. The storms and region options were selected due to the extreme damages caused by these storms.", a("Click here to read more about Open-Meteo.", href = "https://open-meteo.com/en/docs/historical-forecast-api", target = "_blank")),
              h1("The Data Download tab allows the user to interactivly select locations and variables and download the data file. The Data Exploration tab allows the user to interact with various widgets and tools to view a variety of data summary graphics."),
              img(src = "APIimage.png", height = "300px"),
              img(src = "Cloud.png", height = "300px")
      ),
      tabItem(tabName = "download",
              fluidPage(
                titlePanel("Download Rainfall and Wind Data"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("location", "Select Location", choices = c(
                      "Asheville, NC" = "Asheville",
                      "Busick, NC" = "Busick",
                      "Kerrville, TX" = "Kerrville",
                      "Orange County, NC" = "Orange County"
                    )),
                    dateInput("start_date", "Start Date"),
                    dateInput("end_date", "End Date"),
                    selectInput("interval", "Time Interval",
                                choices = c("1 hour" = "1 hours",
                                            "2 hours" = "2 hours",
                                            "4 hours" = "4 hours",
                                            "6 hours" = "6 hours"),
                                selected = "1 hours"),
                    checkboxGroupInput("columns", "Select columns to include in download:",
                                       choices = c("interval", "precipitation_sum", "wind_speed_avg", "wind_gust_max"),
                                       selected = c("interval", "precipitation_sum", "wind_speed_avg", "wind_gust_max")),
                    radioButtons("rain_filter", "Hourly Rainfall Filter (inches):",
                                 choices = c("All" = "all", 
                                             "> 0.2 in" = "0.2",
                                             "< 1 in (use larger Time Interval)" = "1"),
                                 selected = "all"),
                    actionButton("fetch_data", "Preview Data"),
                    downloadButton("download_csv", "Download CSV"),
                    uiOutput("storm_name_display"),
                  ),
                  mainPanel(
                    h4("Query Preview"),
                    tableOutput("preview_table")
                  )
                )

              
      )
    )
  )
)
)

#Create API Query function
data_rainfall_wind <- function(lat, lon, start_date, end_date, time_interval = "1 hours", ...) {
  url <- "https://archive-api.open-meteo.com/v1/archive"
  response <- GET(url, query = list (
    latitude = lat,
    longitude = lon,
    start_date = start_date,
    end_date = end_date,
    hourly = "precipitation,wind_speed_10m,wind_gusts_10m",
    timezone = "auto"
  ))
  data1 <- fromJSON(content(response, as = "text"))
  
  tibbl <- tibble(
    time = ymd_hm(data1$hourly$time),
    precipitation = data1$hourly$precipitation * 0.03937,
    wind_speed = data1$hourly$wind_speed_10m,
    wind_gust = data1$hourly$wind_gusts_10m
  )
  # time interval options
  time_data <- tibbl %>%
    mutate(interval = floor_date(time, time_interval)) %>%
    group_by(interval) %>%
    summarize(
      precipitation_sum = sum(precipitation, na.rm = TRUE),
      wind_speed_avg = mean(wind_speed, na.rm = TRUE),
      wind_gust_max = max(wind_gust, na.rm = TRUE),
      .groups = "drop"
    )
  return(time_data)
}

#Create master dataset used for graphs

#Asheville Helene flood
rain_asheville_nc <- data_rainfall_wind(35.5975, -82.5461, "2024-09-25", "2024-09-27", "1 hours")

#Busick nc helene flood
rain_busick_nc <- data_rainfall_wind(35.7698, -82.1829, "2024-09-25", "2024-09-27", "1 hours")

#houston July 2025 flood
rain_kerrville_tx <-data_rainfall_wind(30.0474, -99., "2025-07-03", "2025-07-05", "1 hours")

#Nc Chantal July 2025 flood
rain_orangecounty_nc <- data_rainfall_wind(36.0263, -79.1097, "2025-07-06", "2025-07-07", "1 hours")

#add location and storm name
rain_asheville_nc <- rain_asheville_nc %>% 
  mutate(location = "Asheville", storm_name = "Helene")

rain_busick_nc <- rain_busick_nc %>% 
  mutate(location = "Busick", storm_name = "Helene")

rain_kerrville_tx <- rain_kerrville_tx %>% 
  mutate(location = "Kerrville", storm_name = "Barry")

rain_orangecounty_nc <- rain_orangecounty_nc %>% 
  mutate(location = "Orange County", storm_name = "Chantal")

#combine data sets into One
hourly_all_data <- bind_rows(rain_asheville_nc, rain_busick_nc, rain_kerrville_tx, rain_orangecounty_nc) %>%
  group_by(location) %>%
  mutate(
    hours_since_start = as.numeric(difftime(interval, min(interval), units = "hours")),
    cumulative_rainfall = cumsum(precipitation_sum)
  ) %>%
  ungroup()

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Define lat/lon for each location
  location_coords <- list(
    "Asheville" = list(lat = 35.5975, lon = -82.5461),
    "Busick" = list(lat = 35.7698, lon = -82.1829),
    "Kerrville" = list(lat = 30.0474, lon = -99.0),
    "Orange County" = list(lat = 36.0263, lon = -79.1097)
  )
  
  # Store the most recent data and store hourly data for use in Data Exploration
  queried_data <- reactiveVal()
  raw_hourly_data <- reactiveVal()
  
  #auto fill date range based on location so specific storm data is selected
  observeEvent(input$location, {
    if (input$location == "Asheville" || input$location == "Busick") {
      updateDateInput(session, "start_date", value = as.Date("2024-09-25"))
      updateDateInput(session, "end_date", value = as.Date("2024-09-27"))
    } else if (input$location == "Kerrville") {
      updateDateInput(session, "start_date", value = as.Date("2025-07-03"))
      updateDateInput(session, "end_date", value = as.Date("2025-07-05"))
    } else if (input$location == "Orange County") {
      updateDateInput(session, "start_date", value = as.Date("2025-07-06"))
      updateDateInput(session, "end_date", value = as.Date("2025-07-07"))
    }
  })
  # When button is clicked
  observeEvent(input$fetch_data, {
    req(input$location, input$start_date, input$end_date)
    
    # Extract coordinates
    coords <- location_coords[[input$location]]
    
    # Run the query
    the_query <- data_rainfall_wind(
      lat = coords$lat,
      lon = coords$lon,
      start_date = as.character(input$start_date),
      end_date = as.character(input$end_date),
      time_interval = input$interval
    )
    
    # Store full data
    queried_data(the_query)
    })
  
  filtered_data <- reactive({
    req(queried_data())
    data2 <- queried_data()
    if (input$rain_filter == "0.2") {
      data2 <- data2[data2$precipitation_sum >= 0.2, ]
    } else if (input$rain_filter == "1") {
      data2 <- data2[data2$precipitation_sum >= 1, ]
    }
    data2
  })
    
  filtered_data_selected <- reactive({
    req(filtered_data(), input$columns)
    filtered_data()[, input$columns, drop = FALSE]
  })

  
  # Show preview table
  output$preview_table <- renderTable({
    head(filtered_data_selected(), 10)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("weather_data_", input$location, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  output$storm_name_display <- renderUI({
    storm <- switch(input$location,
                    "Asheville" = "Helene",
                    "Busick" = "Helene",
                    "Kerrville" = "Barry",
                    "Orange County" = "Chantal")
    strong(paste("Storm:", storm))
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
