data <- read.csv("./covid-data.csv")
dates <- data %>% select(date) %>% unique()
#save dates as csv
write.csv(dates, file = "./dates.csv", row.names = FALSE)



geopoly <- read_sf("./countries.geojson")
iso = data %>% select(iso_code) %>% unique()
dayData <- data %>% filter(date == as.Date("2020-06-11","%Y-%m-%d")) %>% select(iso_code, total_cases)
dayData <- iso %>% left_join(dayData, by = c("iso_code" = "iso_code")) %>% replace_na(list(total_cases = 0))
joined_geo <- geopoly %>% left_join(dayData, by = c("ISO_A3" = "iso_code")) %>% replace_na(list(total_cases = 0))
