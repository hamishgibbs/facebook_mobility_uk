suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/utils.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Downloads/gadm36_GBR_shp (2)/gadm36_GBR_3.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])
label_map_lei <- read_csv(.args[2])

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

mob <- read_csv(.args[5]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

gadm <- st_read(.args[6]) %>% 
  st_simplify(dTolerance = 0.001)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

#Need: which communities existed in X in 2 weeks before lockdown
#what were the connections between X community and others at each time slice?
#Which community slice to plot?

area <- a3 %>% filter(NAME_3 == 'Leicester') %>% pull(quadkey)
i_date <- as.Date('2020-06-29')

area_communities_interval <- label_map %>% filter(quadkey %in% area) %>% filter(date > i_date - 14 & date <= i_date) 

area_clusters <- area_communities_interval %>% pull(cluster) %>% unique()

mob_clusters <- mob %>% 
  left_join(label_map, by = c('date', 'start_quadkey' = 'quadkey')) %>% 
  rename(start_cluster = cluster) %>% 
  left_join(label_map, by = c('date', 'end_quadkey' = 'quadkey')) %>% 
  rename(end_cluster = cluster) 

mob_clusters %>% 
  filter(date > i_date - 14 & date <= i_date)



tiles %>% left_join(mob_clusters %>% 
  filter(date > i_date - 14 & date <= i_date) %>% 
  mutate(connection = case_when(start_cluster %in% area_clusters ~ 'Flow from area',
                                end_cluster %in% area_clusters ~ 'Flow to area')) %>% 
  filter(connection == 'Flow to area') %>% 
  group_by(start_cluster) %>% 
  mutate(flow_from = sum(n_crisis, na.rm = T)) %>% 
  group_by(start_quadkey) %>% 
  summarise(flow_from = sum(flow_from)), by = c('quadkey' = 'start_quadkey')) %>% 
  drop_na(flow_from) %>% 
  ggplot() + 
  geom_sf(aes(fill = flow_from))




label_map_area <- lockdown_communities(area, i_date, label_map)

label_map_area <- label_map_area %>% filter(date == i_date) %>% pull(cluster) %>% unique()

label_map_date <- label_map %>% filter(date == i_date) %>% select(quadkey, cluster)

mob_date <- mob %>% 
  filter(date == i_date) %>% 
  left_join(label_map_date, by = c('start_quadkey' = 'quadkey')) %>% 
  rename(start_cluster = cluster) %>% 
  left_join(label_map_date, by = c('end_quadkey' = 'quadkey')) %>% 
  rename(end_cluster = cluster) %>% 
  group_by(start_cluster, end_cluster) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T)) %>% 
  filter(start_cluster != end_cluster)


connected_communities <- mob_date %>% 
  filter(start_cluster == label_map_area) %>% 
  pull(end_cluster)

connected <- tiles %>% 
  left_join(label_map %>% filter(date == i_date) %>% filter(cluster %in% connected_communities),
            by = c('quadkey')) %>% 
  drop_na(cluster) %>% 
  left_join(mob_date %>% filter(start_cluster == label_map_area), by = c('cluster' = 'end_cluster'))
area <- tiles %>% 
  left_join(label_map %>% filter(date == i_date, cluster == label_map_area)) %>% 
  drop_na(cluster)

ggplot() + 
  geom_sf(data = uk, fill = 'transparent', size = 0.1) + 
  geom_sf(data = connected, aes(fill = n_crisis), size = 0) +
  colorspace::scale_fill_continuous_sequential('Reds') + 
  geom_sf(data = area, fill = 'red', size = 0)






  
  