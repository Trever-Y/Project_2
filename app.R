#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#Define UI
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
              h1("The purpose of this app is to easily compare duration and intensity of rainfall and wind speeds from recent major storms. Please keep in mind that the storm durations vary and start/end points are daily, not hourly. This means that even if a storm starts at night, that entire day of data is pulled from the API."),
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
                    sliderInput("interval", "Time Interval (Hours)",
                                min = 1, max = 6, step = 1, value = 1),
                    checkboxGroupInput("columns", "Select columns to include in download:",
                                       choices = c("interval", "precipitation_sum", "wind_speed_avg", "wind_gust_max"),
                                       selected = c("interval", "precipitation_sum", "wind_speed_avg", "wind_gust_max")),
                    radioButtons("rain_filter", "Hourly Rainfall Filter (inches):",
                                 choices = c("All" = "all", 
                                             "> 0.2 in" = "0.2",
                                             "> 1 in (use larger Time Interval)" = "1"),
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
      ),
      tabItem(tabName = "exploration",
              fluidPage(
                titlePanel("Weather Data Visualizations"),
                
                #Plot type options
                selectInput("plot_type", "Select Plot Type",
                            choices = c("Table", "Line Plot", "Box Plot", "Heat Map"),
                            selected = "Table"),
                
                #Show these tabs only if Line Plot is selected
                conditionalPanel(
                  condition = "input.plot_type == 'Line Plot'",
                  tabsetPanel(
                    tabPanel("Average Rainfall",
                             plotOutput("avg_rainfall_plot")),
                    tabPanel("Cumulative Rainfall",
                             plotOutput("cumulative_rainfall_plot")),
                    tabPanel("Wind Averages",
                             plotOutput("wind_avg_plot")),
                    tabPanel("Max Wind Gusts",
                             plotOutput("wind_gust_plot"))
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Box Plot'",
                  tabsetPanel(
                    tabPanel("Rainfall Distribution", plotOutput("box_rainfall_plot")),
                    tabPanel("Wind Averages Distribution", plotOutput("box_wind_avg_plot")),
                    tabPanel("Wind Gusts Distribution", plotOutput("box_wind_gust_plot"))
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Heat Map'",
                  tabsetPanel(
                    tabPanel("Rainfall Heat Map",
                             plotOutput("heat_rainfall_plot")),
                    tabPanel("Wind Avg Heat Map",
                             plotOutput("heat_wind_avg_plot")),
                    tabPanel("Max Gust Heat Map",
                             plotOutput("heat_wind_gust_plot"))
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Table'",
                  tabsetPanel(
                    tabPanel("Rainfall Contingency Table",
                            plotOutput("rain_facet_table")),
                    tabPanel("Rain-Wind Interaction Table",
                             plotOutput("rain_wind_interaction_plot"))
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
  #time interval options
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

#combine data sets into One and create necessary variables
hourly_all_data <- bind_rows(rain_asheville_nc, rain_busick_nc, rain_kerrville_tx, rain_orangecounty_nc) %>%
  group_by(location) %>%
  mutate(
    hours_since_start = as.numeric(difftime(interval, min(interval), units = "hours")),
    cumulative_rainfall = cumsum(precipitation_sum),
    rain_category = cut(
      precipitation_sum,
      breaks = c(-Inf, 0.2, 0.4, Inf),
      labels = c("< 0.2", "0.2 - 0.4", "> 0.4")
    ),
    wind_category = cut(
      wind_speed_avg,
      breaks = c(-Inf, 10, 15, Inf),
      labels = list("Low (<10 mph)", "Moderate (10-15 mph)", "High (>15 mph)")
    )
  ) %>%
  ungroup()

#Define line plot function
plot_line <- function(data, x_var, y_var, x_label, y_label, title) {
  ggplot(data, aes_string(x = x_var, y = y_var, color = "location")) +
    geom_line(linewidth = 1) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Location"
    ) +
    theme_minimal()
}

#Define box plot function
plot_box <- function(data, x_var, y_var, color_var = NULL, x_label = NULL, y_label = NULL, title = NULL) {
  ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) +
    geom_boxplot() +
    labs(title = title,
         x = x_label, 
         y = y_label, 
         color = color_var) +
    theme_minimal()
}

#Define heat map function
plot_heatmap <- function(data, fill_var, title, fill_label) {
  ggplot(data, aes(x = hours_since_start, y = location, fill = .data[[fill_var]])) +
    geom_tile() +
    labs(
      title = title,
      x = "Hours Since Storm Start",
      y = "City/Region",
      fill = fill_label
    ) +
    scale_fill_viridis_c() +
    theme_minimal()
}

#Define server logic
server <- function(input, output, session) {
  
  #Define lat/lon for each location
  location_coords <- list(
    "Asheville" = list(lat = 35.5975, lon = -82.5461),
    "Busick" = list(lat = 35.7698, lon = -82.1829),
    "Kerrville" = list(lat = 30.0474, lon = -99.0),
    "Orange County" = list(lat = 36.0263, lon = -79.1097)
  )
  
  #Store the most recent data selected
  queried_data <- reactiveVal()
  
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
  #compute on click
  observeEvent(input$fetch_data, {
    req(input$location, input$start_date, input$end_date)
    
    #grab coordinates
    coords <- location_coords[[input$location]]
    
    #Run the query
    the_query <- data_rainfall_wind(
      lat = coords$lat,
      lon = coords$lon,
      start_date = as.character(input$start_date),
      end_date = as.character(input$end_date),
      time_interval = paste(input$interval, "hours")
    )
    
    #Store and filter/select data
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

  
  #Preview, download, storm
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
  
  #Data exploration tab
  ##Line Plots
  output$avg_rainfall_plot <- renderPlot({
    req(input$plot_type == "Line Plot")
    plot_line(
      data = hourly_all_data,
      x_var = "hours_since_start",
      y_var = "precipitation_sum",
      x_label = "Hours Since Storm Start",
      y_label = "Rainfall (Inches)",
      title = "Average Rainfall Over Time by Location"
    )
  })
  
  output$cumulative_rainfall_plot <- renderPlot({
    req(input$plot_type == "Line Plot")
    plot_line(
      data = hourly_all_data,
      x_var = "hours_since_start",
      y_var = "cumulative_rainfall",
      x_label = "Hours Since Storm Start",
      y_label = "Cumulative Rainfall (Inches)",
      title = "Cumulative Rainfall Over Time by Location"
    )
  })
  
  output$wind_avg_plot <- renderPlot({
    req(input$plot_type == "Line Plot")
    plot_line(
      data = hourly_all_data,
      x_var = "hours_since_start",
      y_var = "wind_speed_avg",
      x_label = "Hours Since Storm Start",
      y_label = "Wind Speed Average (Miles Per Hour)",
      title = "Hourly Wind Averages Over Storm Duration"
    )
  })
  output$wind_gust_plot <- renderPlot({
    req(input$plot_type == "Line Plot")
    plot_line(
      data = hourly_all_data,
      x_var = "hours_since_start",
      y_var = "wind_gust_max",
      x_label = "Hours Since Storm Start",
      y_label = "Max Wind Gust (Miles Per Hour)",
      title = "Maximum Wind Gusts Over Storm Duration"
    )
  })
  ##Box plots
  output$box_rainfall_plot <- renderPlot({
    req(input$plot_type == "Box Plot")
    plot_box(
      data = hourly_all_data,
      x_var = "storm_name",
      y_var = "precipitation_sum",
      color_var = "location",
      x_label = "Name of the Storm",
      y_label = "Rainfall (Inches)",
      title = "Distribution of Hourly Rainfall by Location and Storm"
    )
  })
  
  output$box_wind_avg_plot <- renderPlot({
    req(input$plot_type == "Box Plot")
    plot_box(
      data = hourly_all_data,
      x_var = "storm_name",
      y_var = "wind_speed_avg",
      color_var = "location",
      x_label = "Name of the Storm",
      y_label = "Wind Speed (Miles Per Hour)",
      title = "Distribution of Hourly Wind Averages by Location and Storm"
    )
  })
  
  output$box_wind_gust_plot <- renderPlot({
    req(input$plot_type == "Box Plot")
    plot_box(
      data = hourly_all_data,
      x_var = "storm_name",
      y_var = "wind_gust_max",
      color_var = "location",
      x_label = "Name of the Storm",
      y_label = "Max Wind Gust (Miles Per Hour)",
      title = "Distribution of Wind Gust Maximums by Location and Storm"
    )
  })
  
  ##Heat Maps
  output$heat_rainfall_plot <- renderPlot({
    req(input$plot_type == "Heat Map")
    plot_heatmap(
      data = hourly_all_data,
      fill_var = "precipitation_sum",
      title = "Heat Map of Hourly Rainfall by Location",
      fill_label = "Inches Per Hour"
    )
  })
  
  output$heat_wind_avg_plot <- renderPlot({
    req(input$plot_type == "Heat Map")
    plot_heatmap(
      data = hourly_all_data,
      fill_var = "wind_speed_avg",
      title = "Heat Map of Hourly Averaged Windspeed by Location",
      fill_label = "MPH"
    )
  })
  
  output$heat_wind_gust_plot <- renderPlot({
    req(input$plot_type == "Heat Map")
    plot_heatmap(
      data = hourly_all_data,
      fill_var = "wind_gust_max",
      title = "Heat Map of Maximum Hourly Wind Gust by Location",
      fill_label = "MPH"
    )
  })
  
  ##Faceted Bar Plot
  output$rain_facet_table <- renderPlot({
    req(input$plot_type == "Table")
    ggplot(hourly_all_data, aes(x = rain_category, fill = location)) +
      geom_bar(position = "dodge") +
      facet_wrap(~storm_name) +
      labs(
        title = "Hourly Rainfall Distribution by Storm and Location",
        x = "Rainfall Category (in/hr)",
        y = "Count of Hours",
        fill = "Location"
      ) +
      theme_minimal()
  })
  
  ##Rain and wind plot
  output$rain_wind_interaction_plot <- renderPlot({
    req(input$plot_type == "Table")
    
    ggplot(hourly_all_data, aes(x = rain_category, y = wind_category)) +
      geom_count() +
      facet_wrap(~ storm_name) +
      labs(
        title = "Rainfall and Wind Speed Interaction by Storm",
        x = "Rainfall Category (in/hr)",
        y = "Wind Speed Category",
        size = "Count"
      ) +
      theme_minimal()
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
