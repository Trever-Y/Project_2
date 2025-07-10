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
              titlePanel("Data Download")
              
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)
