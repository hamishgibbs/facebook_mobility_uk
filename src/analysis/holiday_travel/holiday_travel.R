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
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/journey_lines/journey_lines.shp')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character()))

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

journey_lines <- st_read(.args[3]) %>% 
  st_set_crs(4326)

weekend_mob <- mob %>% 
  filter(date >= as.Date('2020-06-01'), date < as.Date('2020-08-01')) %>% 
  filter(start_quadkey != end_quadkey) %>% 
  mutate(wkday = as.character(lubridate::wday(date))) %>% 
  filter(wkday %in% c('1', '6', '7')) %>% 
  mutate(perc_change = ((n_crisis - n_baseline) / n_baseline) * 100) %>% 
  filter(perc_change > 100)

weekend_mob <- weekend_mob %>% group_by(journey) %>% summarise(n_crisis = sum(n_crisis, na.rm = T))

hf_journeys <- weekend_mob %>% pull(journey) %>% unique() %>% str_split(., '_')

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

p <- journey_lines %>% 
  left_join(weekend_mob, by = c('journey')) %>% 
  drop_na(n_crisis) %>% 
  ggplot() + 
  geom_sf(aes(size = n_crisis), color = 'blue')  + 
  scale_size(range = c(0.3, 1.5)) + 
  geom_sf(data = world, size = 0.1, colour = 'black', fill = 'transparent') + 
  geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
  theme_void() + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) + 
  labs(size = 'Travellers')
  
ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/high_flow_holiday.png', p,
       width = 5, height = 6,
       units = 'in')



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
