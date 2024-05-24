#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dates <- read.csv("./../data/dates.csv")
# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        ),
        sidebarPanel(
          sliderInput("date",
                      "Date:",
                      min = as.Date(dates$date[1],"%Y-%m-%d"),
                      max = as.Date(dates$date[length(dates$date)],"%Y-%m-%d"),
                      value = as.Date(dates$date[1],"%Y-%m-%d"),
                      timeFormat="%Y-%m-%d")
        )
    )
)
