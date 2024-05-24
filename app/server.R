#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(tidyr)

data <- read.csv("./../data/covid-data.csv")
dates <- data %>% select(date) %>% unique()
geopoly <- read_sf("./../data/countries.geojson")
iso = data %>% select(iso_code) %>% unique()

max_cases <- max(na.omit(data$new_cases))

library(tidyr)
library(dplyr)

max_change = 10000
# Define server logic required to draw a histogram
function(input, output, session) {

    output$map <- renderLeaflet({
      dayData <- data %>% 
        filter(date == input$date) %>% 
        select(iso_code, new_cases,population)
      
      dayData <- iso %>% 
        left_join(dayData, by = c("iso_code" = "iso_code")) %>% 
        replace_na(list(new_cases = 0))
      
      dayData <- dayData %>% 
        mutate(new_cases_per_capita = new_cases*100000/population)
      
      joined_geo <- geopoly %>% left_join(dayData, by = c("ISO_A3" = "iso_code")) %>% replace_na(list(new_cases = 0))
      
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = c(0,max(dayData$new_cases_per_capita)+1)
      )
      
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 50, zoom = 2.2) %>%
        addPolygons(data = joined_geo, color = "white",fillColor=pal(joined_geo$new_cases_per_capita), stroke = 0.1, opacity = 0.8)
    })

}
