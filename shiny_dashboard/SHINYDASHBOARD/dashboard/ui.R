library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)
library(shinyjs)

months <- read.csv("../covid-data-monthly.csv") %>% select(month) %>% unique() %>% arrange(month) %>% pull(month)
tags$script("
    Shiny.addCustomMessageHandler('open_panel', function(value) {
    Shiny.setInputValue('open_panel', value);
    });
  ")
dashboardPage(
  
  skin = "black",
  dashboardHeader(title = "COVID-19 Pandemic Analysis"),
  dashboardSidebar(
    sidebarMenu(
      useShinyjs(),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Summary Table", tabName = "table", icon = icon("table")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Healthcare", tabName = "healthcare", icon = icon("hospital")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    #dateInput("date", "Select Date:", value = Sys.Date()),
    htmlOutput("logo")
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "map",
              fluidRow(
                
                box(title = "COVID-19 Cases Map", width = 12,height = 7, status = "primary", solidHeader = TRUE,
                    column(width=8,leafletOutput("covidMap",width="100%",height="60vh")), #I have no idea why but height: 100% compresses it to 0 height lmao
                    column(width=4,
                           h4(textOutput("selected_name")),
                           plotOutput("selected_plot"),
                           sliderInput("date",
                                       "Month:",
                                       min = as.Date(months[1],"%Y-%m-%d"),
                                       max = as.Date(months[length(months)],"%Y-%m-%d"),
                                       value = as.Date(months[1],"%Y-%m-%d"),
                                       timeFormat="%Y-%m"),
                           selectInput("selectedVar",
                            "Variable:",
                            c("Total Cases" = "total_cases",
                              "Total Deaths" = "total_deaths",
                              "New Cases" = "new_cases",
                              "ICU Patients" = "icu_patients",
                              "Hospitalized Patients" = "hosp_patients",
                              "Total Tests" = "total_tests",
                              "People Fully Vaccinated" = "people_fully_vaccinated"))
                           
                          )
                    ),
                
                #panel for the pop-up graph
                conditionalPanel(
                  box(id="myBox",textInput("country_clicked", "Country Clicked:",value=""),style='display:none;text-color:rgba(0,0,0,0);'),
                  condition = "input.country_clicked != ''",
                  uiOutput("stats")
                )
                
              )),
      tabItem(tabName = "table",
              fluidRow(
                box(title = "Summary Table", width = 12, status = "primary", solidHeader = TRUE, 
                    DT::dataTableOutput("covidTable"),style = "overflow-x: scroll;")
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
              )),
      tabItem(tabName = "about",
              box(title = "About", width = 12, height=12, status = "primary", solidHeader = TRUE, style = "overflow-y: scroll; text-align:center; font-size: 16px",
              h1("Covid 19 Dashboard"),
              p("This dashboard provides an analysis of the COVID-19 pandemic using data from the Our World in Data COVID-19 dataset."),
              h2("Sections"),
              list(
                h3("Map"),
                p("The map section provides an interactive map of COVID-19 cases around the world."),
                p("You can select one of the variables at the right side of the map to view the data on the map. The date slider will also filter the data for the selected month for each country."),
                p("Countries are colored based on the selected variable, compared to an all time high (from start to selected date, per capita) value of the variable."),
                p("Click on a country to view more information about it (it's ranking in terms of cases, deaths and vaccinations compared to other countries)."),
                p("The graph on the right side of the map shows the selected variable for the world or a selected country for the entire time period."),
                h3("Summary Table"),
                p("The summary table can be used to find data of interest. It presents the entire dataset in a filterable way."),
                h3("Trends"),
                p("Lorem ipsum karton gipsum"),
                h3("Statistics"),
                p("Lorem ipsum karton gipsum"),
                h3("Demographics"),
                p("Lorem ipsum karton gipsum"),
                h3("Healthcare"),
                p("Lorem ipsum karton gipsum")
              )
              ))
    )
  )
)
