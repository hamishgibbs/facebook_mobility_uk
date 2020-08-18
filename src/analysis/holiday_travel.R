#what are journeys kn Fridays and saturdays that have the highest beviation from baseline?
suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character()))

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

weekend_mob <- mob %>% 
  filter(date >= as.Date('2020-06-01')) %>% 
  filter(start_quadkey != end_quadkey) %>% 
  mutate(wkday = as.character(lubridate::wday(date))) %>% 
  filter(wkday %in% c('1', '6', '7')) %>% 
  mutate(perc_change = ((n_crisis - n_baseline) / n_baseline) * 100) %>% 
  filter(perc_change > 100)

hf_journeys <- weekend_mob %>% pull(journey) %>% unique() %>% str_split(., '_')

journey_line <- function(journey){
  
  journey <- str_split(journey, '_')
  
  start_poly <- tiles %>% filter(quadkey == journey[1])
  end_poly <- tiles %>% filter(quadkey == journey[2])
  
  line <- suppressWarnings(st_linestring(rbind(st_coordinates(st_geometry(st_centroid(start_poly))), st_coordinates(st_geometry(st_centroid(end_poly))))))
  
  return(line)
}

lines <- st_as_sf(st_sfc(lapply(hf_journeys, journey_line)))
st_crs(lines) <- 4326
lines$journey <- hf_journeys
  

require(leaflet)

leaflet(lines) %>% 
  addTiles() %>% 
  addPolylines()


weekend_mob %>% 
  ggplot() + 
  geom_density(aes(x = perc_change))

weekend_mob %>% pull(date) %>% unique()
