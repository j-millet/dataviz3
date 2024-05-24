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

data <- read.csv("~/Desktop/covid-data.csv")
dates <- data %>% select(date) %>% unique()
library(tidyr)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({
      
      cases <- data %>% filter(location == "Poland") %>% select(date,total_cases) %>% arrange(date) %>% head(input$bins)
      plot(cases$total_cases,type="line")
    })

}
