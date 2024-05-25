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
library(ggplot2)
library(paletteer)

data <- read.csv("./../data/covid-data.csv")
dates <- data %>% select(date) %>% unique() %>% arrange(date) %>%pull(date) %>% as.Date("%Y-%m-%d")
geopoly <- read_sf("./../data/countries.geojson")
iso = data %>% select(iso_code) %>% unique()

max_cases <- max(na.omit(data$new_cases_smoothed))

library(tidyr)
library(dplyr)

max_change = 10000
# Define server logic required to draw a histogram
function(input, output, session) {

    output$map <- renderLeaflet({
      dayData <- data %>% 
        filter(date == input$date) %>% 
        select(location,iso_code, new_cases_smoothed,population)
      
      dayData <- iso %>% 
        left_join(dayData, by = c("iso_code" = "iso_code")) %>% 
        replace_na(list(new_cases_smoothed = 0,population=1))
      
      dayData <- dayData %>% 
        mutate(new_cases_smoothed_per_capita = new_cases_smoothed/population)
      
      joined_geo <- geopoly %>% left_join(dayData, by = c("ISO_A3" = "iso_code")) %>% replace_na(list(new_cases_smoothed_per_capita = 0))

      
      pal <- colorNumeric(
        as.vector(paletteer::paletteer_c("ggthemes::Red-Blue Diverging",n=6,direction = -1)),
        domain = data %>% 
                     filter(date <= input$date) %>%
                     filter(date >= input$date - 10) %>%
                     mutate(new_cases_smoothed_per_capita = new_cases_smoothed/population) %>%
                     select(new_cases_smoothed_per_capita) %>% 
                     replace_na(list(new_cases_smoothed_per_capita=0)) %>%
                     pull(new_cases_smoothed_per_capita)
      )
      
      leaflet() %>%
        setView(lng = 0, lat = 50, zoom = 2) %>%
        addPolygons(
          data = joined_geo, 
          color = "white",
          fillColor=pal(joined_geo$new_cases_smoothed_per_capita), 
          stroke = 0.01, 
          opacity = 0.8,
          fillOpacity=1,
          label = ~paste0(ADMIN, ": New cases per 1k: ", round(new_cases_smoothed/1000),2))
    })
    
    output$histo <- renderPlot({
      cases_aggr <- data %>% select(date,new_cases_smoothed) %>%replace_na(list(new_cases_smoothed=0)) %>% group_by(date) %>% arrange(date) %>% summarise(new_cases_smoothed = sum(new_cases_smoothed))
      ggplot(data=cases_aggr, aes(x=date, y=new_cases_smoothed, group=1)) + 
        geom_line() + 
        geom_vline(xintercept=match(input$date,dates), linetype="dashed", color = "red") +
        theme(axis.line=element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks=element_blank(),
                            axis.title.x=element_blank(),
                            axis.title.y=element_blank(),
                            legend.position="none",
                            panel.background=element_blank(),
                            panel.border=element_blank(),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            plot.background=element_blank())
    })

}
