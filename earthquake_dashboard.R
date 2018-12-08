library("jsonlite")
library("tidyverse")

data <- fromJSON("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson")

df <- bind_cols(data$features$properties, data$features$geometry)

df <- separate(df, coordinates, c("lng", "lat", "depth"), sep = ",")
df$lng <- 
  df$lng %>% 
  str_replace_all("c", "") %>% 
  str_replace_all("\\(", "") %>% 
  as.numeric(as.character())
df$lat <- 
  df$lat %>% 
  as.numeric(as.character())
df$depth <- 
  df$depth %>% 
  str_replace_all("\\)", "") %>% 
  as.numeric(as.character())

df
