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
library(htmltools)

data <- read.csv("./../data/covid-data.csv")
dates <- data %>% select(date) %>% unique() %>% arrange(date) %>% pull(date) %>% as.Date("%Y-%m-%d")
geopoly <- read_sf("./../data/countries.geojson")
iso = data %>% select(iso_code) %>% unique()

# Define server logic
function(input, output, session) {
    dataSelected <- reactive({
      dataSelected <- data %>% select(all_of(input$selectedVar),location,iso_code,date,population)
      names(dataSelected)[names(dataSelected) == input$selectedVar] <- 'selected'
      dataSelected <- dataSelected %>% 
        replace_na(list(selected = 0)) %>%
        mutate(selected_per_capita = selected/population)
      return(dataSelected)
      })
  
    output$map <- renderLeaflet({
      dayData <- dataSelected() %>% 
        filter(date == input$date) %>% 
        select(location,iso_code, selected,population,selected_per_capita)
      
      dayData <- iso %>% 
        left_join(dayData, by = c("iso_code" = "iso_code")) %>% 
        replace_na(list(selected = 0,population=1))
      
      joined_geo <- geopoly %>% 
        left_join(dayData, by = c("ISO_A3" = "iso_code")) %>% 
        replace_na(list(selected_per_capita = 0))
      joined_geo$label <- paste(
          paste("<strong>",joined_geo$ADMIN,"</strong>"),
          paste("<strong>",input$selectedVar,"</strong>: ", format(joined_geo$selected, big.mark = ",", scientific = FALSE)), 
          paste("<strong>Population:</strong> ", format(joined_geo$population, big.mark = ",", scientific = FALSE)), 
          paste("<strong>Percent of population:</strong> ", format(round(joined_geo$selected_per_capita*100,4), nsmall=4, scientific = FALSE),"%"),
          sep="<br/>") %>% lapply(htmltools::HTML)

      
      pal <- colorNumeric(
        as.vector(paletteer::paletteer_c("ggthemes::Red-Blue Diverging",n=6,direction = -1)),
        domain = dataSelected()$selected_per_capita
      )
      
      leaflet() %>%
        setView(lng = 0, lat = 50, zoom = 2) %>%
        addPolygons(
          data = joined_geo, 
          color = "white",
          fillColor=pal(joined_geo$selected_per_capita), 
          stroke = 0.01, 
          opacity = 0.8,
          fillOpacity=1,
          label = ~label
          ) 
    })
    
    output$plot <- renderPlot({
      cases_aggr <- dataSelected() %>% 
        select(date,selected) %>%
        group_by(date) %>% 
        arrange(date) %>% 
        summarise(sum_selected = sum(selected))
      
      ggplot(data=cases_aggr, aes(x=date, y=sum_selected, group=1)) + 
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
