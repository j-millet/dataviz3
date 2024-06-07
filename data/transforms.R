library(dplyr)
library(tidyr)
library(sf)
data <- read.csv("./covid-data.csv") %>% 
  select(iso_code,location,date,total_cases,total_deaths,new_cases_smoothed,population) %>% 
  mutate(
    date = as.Date(date,"%Y-%m-%d"),
    month = as.Date(cut(date, breaks = "month"))
  ) %>%
  select(-date) %>%
  group_by(iso_code,location,month,population) %>% 
  summarise(
    total_cases = max(total_cases),
    total_deaths = max(total_deaths),
    new_cases_smoothed = max(new_cases_smoothed)
  ) %>%
  ungroup()
dates <- data %>% select(date) %>% unique() %>% arrange(date) %>% pull(date) %>% as.Date("%Y-%m-%d")
dates <- data.frame(date=dates)
#save dates as csv
write.csv(dates, file = "./dates.csv", row.names = FALSE,col.names = c('date'))

write.csv(data, file = "./covid-data-monthly.csv", row.names = FALSE)
library(ggplot2)

geopoly <- read_sf("./countries.geojson")
iso = data %>% select(iso_code) %>% unique()
dayData <- data %>% 
  filter(month == as.Date("2021-01-01","%Y-%m-%d")) %>% 
  select(location,iso_code, new_cases_smoothed,population)

dayData <- iso %>% 
  left_join(dayData, by = c("iso_code" = "iso_code")) %>% 
  replace_na(list(new_cases_smoothed = 0,population=1))

dayData <- dayData %>% 
  mutate(new_cases_smoothed_per_capita = new_cases_smoothed/population)

joined_geo <- geopoly %>% left_join(dayData, by = c("ISO_A3" = "iso_code")) %>% replace_na(list(new_cases_smoothed_per_capita = 0))

library(leaflet)
leaflet() %>%
  #addTiles() %>%
  setView(lng = 0, lat = 50, zoom = 2) %>%
  addPolygons(
    data = joined_geo, 
    color = "white",
    fillColor="black", 
    stroke = 0, 
    opacity = 0.8,
    fillOpacity=1,
    label = ~paste0(location, ": New cases per 100k: ", new_cases_smoothed))



library(paletteer)
v = paletteer_c("viridis::viridis",n=30)
as.vector(v)


cases_aggr <- data %>% select(date,new_cases_smoothed) %>%replace_na(list(new_cases_smoothed=0)) %>% group_by(date) %>% arrange(date) %>% summarise(new_cases_smoothed = sum(new_cases_smoothed))
plot(cases_aggr$new_cases_smoothed, type = "l", xlab = "", ylab = "New cases",xaxt = "n",yaxt = "n")

ggplot(data=cases_aggr, aes(x=date, y=new_cases_smoothed, group=1)) + 
  geom_line() + theme(axis.line=element_blank(),
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
