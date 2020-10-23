require(tidyverse)
require(sf)

source('/Users/hamishgibbs/Documents/Covid-19/unequal_mobility_uk/src/utils/quadkey_to_character.R')

mob <- read_csv('/Users/hamishgibbs/Documents/Covid-19/unequal_mobility_uk/data/mobility_data/mobility_data.csv') %>% 
                mutate(start_quadkey = quadkey_to_character(start_quadkey),
                        end_quadkey = quadkey_to_character(end_quadkey))
mob_hrs <- read_csv('/Users/hamishgibbs/Documents/Covid-19/unequal_mobility_uk/data/mobility_data/mobility_referenced.csv') %>% 
  mutate(start_quadkey = quadkey_to_character(start_quadkey),
         end_quadkey = quadkey_to_character(end_quadkey))

old_tiles <- st_read('/Users/hamishgibbs/Documents/Covid-19/unequal_mobility_uk/data/tile_data/bing_tiles_zoom_12.shp') %>% 
  mutate(quadkey = quadkey_to_character(quadkey)) %>% 
  st_set_crs(4326)

new_tiles <- st_read('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp') %>% 
  st_set_crs(4326)

p_old <- old_tiles %>% ggplot() + geom_sf(fill = 'transparent', color = 'purple')
p_new <- new_tiles %>% ggplot() + geom_sf(fill = 'transparent', color = 'green')

#Get centroids from new tiles
new_tiles <- st_centroid(new_tiles)

#intsersect new tile centorids with old tiles
intersect <- st_intersection(old_tiles, new_tiles)

old_len <- old_tiles %>% pull(1) %>% length()
intersect_len <- intersect %>% pull(1) %>% length()

testthat::expect_lt(intersect_len, old_len)

intersect <- intersect %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(old_quadkey = quadkey,
         new_quadkey = quadkey.1)

mob <- mob %>% 
  left_join(intersect, by = c('start_quadkey' = 'old_quadkey')) %>% 
  select(-start_quadkey) %>% 
  rename(start_quadkey = new_quadkey) %>% 
  left_join(intersect, by = c('end_quadkey' = 'old_quadkey')) %>% 
  select(-end_quadkey) %>% 
  rename(end_quadkey = new_quadkey) %>% 
  mutate(journey = paste0(start_quadkey, '_', end_quadkey)) %>% 
  select(journey, date, start_quadkey, end_quadkey, n_baseline, n_crisis)
  
write_csv(mob, '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/old_mobility/old_mobility.csv')

mob_hrs <- mob_hrs %>% 
  left_join(intersect, by = c('start_quadkey' = 'old_quadkey')) %>% 
  select(-start_quadkey) %>% 
  rename(start_quadkey = new_quadkey) %>% 
  left_join(intersect, by = c('end_quadkey' = 'old_quadkey')) %>% 
  select(-end_quadkey) %>% 
  rename(end_quadkey = new_quadkey) %>% 
  mutate(journey = paste0(start_quadkey, '_', end_quadkey)) %>% 
  select(journey, date_time, start_quadkey, end_quadkey, n_baseline, n_crisis)

write_csv(mob_hrs, '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/old_mobility/old_mobility_hours.csv')

