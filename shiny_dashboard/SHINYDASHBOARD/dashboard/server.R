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

covid_monthly <- read.csv("../covid-data-monthly.csv") %>% replace_na(list(total_cases=0,total_deaths=0,new_cases_smoothed=0)) 
geopoly <- read_sf("../countries.geojson")
geopoly <- rmapshaper::ms_simplify(geopoly,keep=0.1)

covid_monthly <- covid_monthly %>% filter(iso_code %in% geopoly$ISO_A3) 

iso = covid_monthly %>% select(iso_code) %>% unique()

geopoly <- geopoly %>% filter(ADMIN != "Antarctica") #turns out penguins can't get COVID-19

get_country_rank <- function(data,country_iso_code,variable){
  data %>% 
    filter(month == max(month)) %>%
    arrange(desc({{variable}})) %>%
    mutate(rank = row_number()) %>%
    filter(iso_code == country_iso_code) %>%
    pull(rank)
}

get_country_rank_per_capita <- function(data,country_iso_code,variable){
  data_mod <- data %>% mutate(per_capita = {{variable}}/population)
  get_country_rank(data_mod,country_iso_code,per_capita)
}


function(input, output,session) {
  shinyjs::hide(id = "myBox")
  output$logo <- renderImage({
    list(src = "logo.svg", width = 100, height = 100)
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
    data <- filteredData()
    inputmonth <- as.Date(cut(input$date, breaks = "month"))
    dayData <- data %>% 
      filter(month == inputmonth) %>% 
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
      domain = c(
        data %>% filter(month <= inputmonth) %>% na.omit() %>% pull(selected_per_capita) %>% min() * 100,
        data %>% filter(month <= inputmonth) %>% na.omit() %>%pull(selected_per_capita) %>% max() * 100 + 1e-20
      )
    )
    js_code <- "
function(el, x) {
  this.eachLayer(function(layer) {
    if (layer instanceof L.Polygon) {
      layer.on('click', function(e) {
        var countryName = layer.options.layerId;
        console.log(countryName);
        Shiny.setInputValue('country_clicked', countryName);
        Shiny.setInputValue('open_panel', 'T');
      });
    }
  });
}
"
    leaflet(joined_geo) %>%
      setView(lng = 0, lat = 50, zoom = 2) %>%
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
      ) %>% 
      onRender(js_code)
  
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
      theme(axis.line=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
  })
  
  output$stats <- renderUI({
    rank_cases <- get_country_rank(covid_monthly,input$country_clicked,total_cases)
    rank_deaths <- get_country_rank(covid_monthly,input$country_clicked,total_deaths)
    rank_cases_per_capita <- get_country_rank_per_capita(covid_monthly,input$country_clicked,total_cases)
    rank_deaths_per_capita <- get_country_rank_per_capita(covid_monthly,input$country_clicked,total_deaths)
    country_name <- covid_monthly %>% filter(iso_code == input$country_clicked) %>% unique() %>% pull(location)
    
    box(title = paste(country_name[1], " statistics"), width = 12, status = "primary", solidHeader = F, 
        column(width=11,
               column(width=6,
                valueBox(
                 value = get_country_rank(covid_monthly,input$country_clicked,total_cases),
                 subtitle = "Total Cases Rank",
                 color = ifelse(rank_cases < 10,"red",ifelse(rank_cases < 50,"yellow","green"))
               ),
               valueBox(
                 value = get_country_rank_per_capita(covid_monthly,input$country_clicked,total_cases),
                 subtitle = "Total Cases Rank Per Capita",
                 color = ifelse(rank_cases_per_capita < 10,"red",ifelse(rank_cases_per_capita < 50,"yellow","green"))
               )),
               column(width=6,
               valueBox(
                 value = get_country_rank(covid_monthly,input$country_clicked,total_deaths),
                 subtitle = "Total Deaths Rank",
                 color = ifelse(rank_deaths < 10,"red",ifelse(rank_deaths < 50,"yellow","green"))
               ),
               
               valueBox(
                 value = get_country_rank_per_capita(covid_monthly,input$country_clicked,total_deaths),
                 subtitle = "Total Deaths Rank Per Capita",
                 color = ifelse(rank_deaths_per_capita < 10,"red",ifelse(rank_deaths_per_capita < 50,"yellow","green"))
               ))),
        column(width=1,actionButton("reset", "Close")))
      
    
  })
  observeEvent(input$reset, {
    updateTextInput(session,'country_clicked', value = "")
  })
  output$covidTable <- renderDT({
    covid_monthly %>% select(month,location,total_cases,total_deaths,new_cases_smoothed) %>% 
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