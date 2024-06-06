library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)

# Load the data
covid19_weekly <- read.csv("../covid19_weekly.csv")
covid_weekly <- read.csv("../covid_weekly.csv")

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "COVID-19 Pandemic Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Summary Table", tabName = "table", icon = icon("table")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Healthcare", tabName = "healthcare", icon = icon("hospital"))
    ),
    dateInput("date", "Select Date:", value = Sys.Date())
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(title = "COVID-19 Cases Map", width = 12, status = "primary", solidHeader = TRUE, leafletOutput("covidMap"))
              )),
      tabItem(tabName = "table",
              fluidRow(
                box(title = "Summary Table", width = 12, status = "primary", solidHeader = TRUE, DTOutput("covidTable"))
              )),
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "Trends", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("covidTrends"))
              )),
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Statistics", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("covidStats"))
              )),
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Demographics", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("covidDemographics"))
              )),
      tabItem(tabName = "healthcare",
              fluidRow(
                box(title = "Healthcare", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("covidHealthcare"))
              ))
    )
  )
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    covid19_weekly %>%
      filter(Date <= as.Date(input$date))
  })
  
  output$covidMap <- renderLeaflet({
    data <- filteredData()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircles(
        lat = ~Lat, lng = ~Long, weight = 1,
        radius = ~sqrt(Confirmed) * 50, popup = ~paste(Confirmed, "cases")
      )
  })
  
  output$covidTable <- renderDT({
    data <- filteredData()
    datatable(data)
  })
  
  output$covidTrends <- renderPlotly({
    data <- filteredData()
    
    plot_ly(data, x = ~Date, y = ~Confirmed, type = 'scatter', mode = 'lines') %>%
      layout(title = "COVID-19 Confirmed Cases Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Confirmed Cases"))
  })
  
  output$covidStats <- renderPlotly({
    data <- filteredData()
    
    plot_ly(data, x = ~Date, y = ~Deaths, type = 'bar') %>%
      layout(title = "COVID-19 Deaths Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Deaths"))
  })
  
  output$covidDemographics <- renderPlotly({
    data <- filteredData()
    
    plot_ly(data, x = ~Date, y = ~Recovered, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "COVID-19 Recoveries Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Recoveries"))
  })
  
  output$covidHealthcare <- renderPlotly({
    data <- filteredData()
    
    plot_ly(data, x = ~Date, y = ~Active, type = 'scatter', mode = 'lines') %>%
      layout(title = "Active COVID-19 Cases Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Active Cases"))
  })
}

shinyApp(ui, server)