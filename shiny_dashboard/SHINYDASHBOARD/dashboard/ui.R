library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)
library(shinyjs)

months <- read.csv("../covid-data-monthly.csv") %>% select(month) %>% unique() %>% arrange(month) %>% filter(month <= as.Date("2022-02-01","%Y-%m-%d")) %>% pull(month)
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
      menuItem("Healthcare", tabName = "healthcare", icon = icon("hospital"))
    ),
    #dateInput("date", "Select Date:", value = Sys.Date()),
    imageOutput("logo")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(title = "COVID-19 Cases Map", width = 12, status = "primary", solidHeader = TRUE, 
                    column(width=8,leafletOutput("covidMap", width = "100%", height = "600px")),
                    column(width=4,
                           textOutput("selected_name"),
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
                  condition = "input.country_clicked != ''",
                  box(id="myBox",textInput("country_clicked", "Country Clicked:",value="")),
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
              ))
    )
  )
)