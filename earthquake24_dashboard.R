library("jsonlite")
library("tidyverse")

# Download of the Data as JSON
data <- fromJSON("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson")

# Row Bind 2 Dataframes of the JSON Date
df <- bind_cols(data$features$properties, data$features$geometry)

# Clean the Data

# Separate Coordinates Column into Longitude Latidude and Depth
df <- separate(df, coordinates, c("lng", "lat", "depth"), sep = ",")
# Remove c and parenthesis from the new Columns
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