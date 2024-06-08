library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)
library(tidyr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(rmapshaper)
library(shinyjs)
library(stringr)

covid_monthly <- read.csv("../covid-data-monthly.csv") %>% 
  mutate(month = as.Date(month,"%Y-%m-%d"))
geopoly <- read_sf("../countries.geojson")
geopoly <- rmapshaper::ms_simplify(geopoly,keep=0.05)

covid_monthly <- covid_monthly %>% filter(iso_code %in% geopoly$ISO_A3) 

iso = covid_monthly %>% select(iso_code) %>% unique()
country_populations <- covid_monthly %>% select(iso_code,population) %>% unique()

geopoly <- geopoly %>% filter(ADMIN != "Antarctica") #turns out penguins can't get COVID-19

get_country_rank <- function(data,country_iso_code,variable){
  r <- data %>% 
    filter(month == max(month)) %>%
    arrange(desc({{variable}})) %>%
    mutate(rank = row_number()) %>%
    filter(iso_code == country_iso_code) %>%
    pull(rank)
  r[1]
}

get_country_rank_per_capita <- function(data,country_iso_code,variable){
  data_mod <- data %>% mutate(per_capita = {{variable}}/population)
  get_country_rank(data_mod,country_iso_code,per_capita)
}


function(input, output,session) {
  
  
  output$logo <- renderText({
    HTML("<img style=\"width:90%; height:auto; padding:5%; position:absolute; bottom:0;\" src=\"https://raw.githubusercontent.com/j-millet/dataviz3/shinydashv2/shiny_dashboard/SHINYDASHBOARD/logo.svg?token=GHSAT0AAAAAACTCG75JVLCY5A2E3ZDW7P4AZTE2MNQ\">")
  })
  
  filteredData <- reactive({
    dataSelected <- covid_monthly %>%
      select(all_of(input$selectedVar),location,iso_code,month,population)
    
    names(dataSelected)[names(dataSelected) == input$selectedVar] <- 'selected'
    dataSelected <- dataSelected %>% 
      replace_na(list(selected = 0)) %>%
      mutate(
        selected_per_capita = selected/population
      )
    return(dataSelected)
  })
  
  output$covidMap <- renderLeaflet({
    
    leaflet(geopoly) %>%
      setView(lng = 0, lat = 50, zoom = 1.5) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      addPolygons(
        data = geopoly, 
        color = "white",
        fillColor="grey", 
        stroke = 0.01, 
        opacity = 0.8,
        fillOpacity=1,
        layerId = ~ISO_A3,
        group = "countries"
      )
  })
  
  updateMap <- function(input){
    data <- filteredData()
    inputmonth <- as.Date(cut(input$date, breaks = "month"))
    dayData <- data %>% 
      filter(month == inputmonth) %>% 
      select(location,iso_code, selected,population,selected_per_capita)
    
    dayData <- iso %>% 
      left_join(dayData, by = c("iso_code" = "iso_code")) %>% 
      replace_na(list(selected = 0)) %>%
      select(-population) %>% 
      inner_join(country_populations, by = c("iso_code" = "iso_code"))
    
    joined_geo <- geopoly %>% 
      left_join(dayData, by = c("ISO_A3" = "iso_code")) %>% 
      replace_na(list(selected_per_capita = 0))
    
    joined_geo$label <- paste(
      paste("<strong>",joined_geo$ADMIN,"</strong>"),
      paste("<strong>",str_to_sentence(gsub("_"," ",input$selectedVar)),"</strong>: ", format(joined_geo$selected, big.mark = ",", scientific = FALSE)), 
      paste("<strong>Population:</strong> ", format(joined_geo$population, big.mark = ",", scientific = FALSE)), 
      paste("<strong>Percent of population:</strong> ", format(round(joined_geo$selected_per_capita*100,4), nsmall=4, scientific = FALSE),"%"),
      sep="<br/>") %>% lapply(htmltools::HTML)
    
    
    pal <- colorNumeric(
      as.vector(
        paletteer::paletteer_c(
          ifelse(
            input$selectedVar %in% c("total_cases","total_deaths","new_cases","icu_patients","hosp_patients"),#bad
            "ggthemes::Red-Blue Diverging",
            "ggthemes::Green-Blue Diverging"
          ),
          n=20,
          direction = -1
        )
      ),
      domain = c(
        data %>% filter(month <= inputmonth) %>% na.omit() %>% pull(selected_per_capita) %>% min() * 100,
        data %>% filter(month <= inputmonth) %>% na.omit() %>%pull(selected_per_capita) %>% max() * 100 + 1e-20
      )
    )
    
    leafletProxy("covidMap") %>%
      clearGroup("countries") %>%
      addPolygons(
        data = joined_geo, 
        color = "white",
        fillColor=pal(joined_geo$selected_per_capita*100), 
        stroke = 0.01, 
        opacity = 0.8,
        fillOpacity=1,
        label = ~label,
        layerId = ~ISO_A3,
        group = "countries"
      )
  }
  
  observeEvent(input$date,{
    updateMap(input)
  })
  observeEvent(input$selectedVar,{
    updateMap(input)
  })
  observeEvent(input$covidMap_shape_click,{
    updateTextAreaInput(session,'country_clicked', value = input$covidMap_shape_click$id)
  })
  output$selected_plot <- renderPlot({
    cases_aggr <- filteredData() %>% 
      select(month,selected,iso_code)
      
      if(input$country_clicked != ""){
        cases_aggr <- cases_aggr %>% 
          filter(iso_code == input$country_clicked)
      }
    
    cases_aggr <- cases_aggr %>% 
      select(month,selected) %>%
      mutate(month = as.Date(month,"%Y-%m-%d"))%>%
      group_by(month) %>% 
      arrange(month) %>% 
      summarise(sum_selected = sum(selected))
    
    
    
    ggplot(data=cases_aggr, aes(x=month, y=sum_selected, group=1)) + 
      geom_line() + 
      geom_vline(xintercept=input$date, linetype="dashed", color = "red") +
      labs(x = "Month", y = "") +
      theme_minimal() 
      
  })
  
  output$selected_name <- renderText({
    if(input$country_clicked != ""){
      country_name <- covid_monthly %>% filter(iso_code == input$country_clicked) %>% unique() %>% pull(location)
      return(paste(country_name[1]))
    }
    return("World")
  })
  
  output$stats <- renderUI({
    choose_color_bad <- function(rank){
      if(is.na(rank)){
        return("yellow")
      }else if(rank < 10){
        return("red")
      } else if(rank < 50){
        return("yellow")
      } else {
        return("green")
      }
    }
    choose_color_good <- function(rank){
      if(is.na(rank)){
        return("yellow")
      }else if(rank < 50){
        return("green")
      } else if(rank < 120){
        return("yellow")
      } else {
        return("red")
      }
    }
    
    rank_cases <- get_country_rank(covid_monthly,input$country_clicked,total_cases)
    rank_deaths <- get_country_rank(covid_monthly,input$country_clicked,total_deaths)
    rank_cases_per_capita <- get_country_rank_per_capita(covid_monthly,input$country_clicked,total_cases)
    rank_deaths_per_capita <- get_country_rank_per_capita(covid_monthly,input$country_clicked,total_deaths)
    rank_people_vaccinated <- get_country_rank(covid_monthly,input$country_clicked,people_fully_vaccinated)
    rank_people_vaccinated_per_capita <- get_country_rank_per_capita(covid_monthly,input$country_clicked,people_fully_vaccinated)
    
    if(is.na(rank_cases) || is.na(rank_deaths) || is.na(rank_cases_per_capita) || is.na(rank_deaths_per_capita) || is.na(rank_people_vaccinated) || is.na(rank_people_vaccinated_per_capita)){
      return("")
    }
    country_name <- covid_monthly %>% filter(iso_code == input$country_clicked) %>% unique() %>% pull(location)
    
    box(title = paste(country_name[1], " statistics"), width = 12,height = 3, status = "primary", solidHeader = F, 
        tags$style(type="text/css", "#reset { margin-top: 0px; } .small-box{height:15vh; width:100%;} "),
        column(width=1,actionButton("reset", "✗")),
        column(width=11,
               column(width=4,
                valueBox(
                 value = rank_cases,
                 subtitle = "Total Cases Rank                ",
                 color = choose_color_bad(rank_cases)
               ),
               valueBox(
                 value = rank_cases_per_capita,
                 subtitle = "Total Cases Per Capita Rank",
                 color = choose_color_bad(rank_cases_per_capita)
               )),
               column(width=4,
               valueBox(
                 value = rank_deaths,
                 subtitle = "Total Deaths Rank",
                 color = choose_color_bad(rank_deaths)
               ),
               
               valueBox(
                 value = rank_deaths_per_capita,
                 subtitle = "Total Deaths Per Capita Rank",
                 color = choose_color_bad(rank_deaths_per_capita)
               )),
               column(width=4,
                      valueBox(
                        value = rank_people_vaccinated,
                        subtitle = "Total People Vaccinated Rank",
                        color = choose_color_good(rank_people_vaccinated)
                      ),
                      
                      valueBox(
                        value = rank_people_vaccinated_per_capita,
                        subtitle = "Total People Vaccinated Per Capita Rank",
                        color = choose_color_good(rank_people_vaccinated_per_capita)
                      ))))
      
    
  })
  observeEvent(input$reset, {
    updateTextInput(session,'country_clicked', value = "")
  })
  output$covidTable <- renderDT({
    covid_monthly %>% 
      select(-iso_code) %>%
      mutate(month = as.Date(month,"%Y-%m-%d")) %>%
      datatable(
        style="bootstrap",
        filter="top",
        extension="Buttons",
        options=list(buttons=c('copy','csv','excel','pdf','print')),
        rownames = F)
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