suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(RColorBrewer)
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
              '/Users/hamishgibbs/Downloads/Clinical_Commissioning_Groups__April_2018__Boundaries-shp/Clinical_Commissioning_Groups__April_2018__Boundaries.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/ccg_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

mob <- read_csv(.args[5]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

lm_date <- label_map %>% 
  group_by(date) %>% 
  group_split()

clust_shp <- lapply(lm_date, get_c_shp)

clust_shp <- do.call(rbind, clust_shp)

get_c_shp <- function(lmd){
  
  clust_shp <-tiles %>% 
    left_join(lmd, by = c('quadkey')) %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(date = unique(date))
  
  return(clust_shp)
  
}


mob_clusters <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  left_join(label_map, by = c('date', 'start_quadkey' = 'quadkey')) %>% 
  rename(start_cluster = cluster) %>% 
  left_join(label_map, by = c('date', 'end_quadkey' = 'quadkey')) %>% 
  rename(end_cluster = cluster) 


mob_sum <- mob_clusters %>% 
  group_by(date, start_cluster, end_cluster) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T)) %>% 
  drop_na(start_cluster, end_cluster) %>% 
  group_by(date) %>% 
  group_split()

m <- mob_sum[[1]]

cluster_labels <- unique(m %>% pull(start_cluster) %>% unique, m %>% pull(end_cluster) %>% unique)

flow <- list()
flow_shp <- list()
for (clust in cluster_labels){
  inflow <- m %>% filter(end_cluster == clust) %>% mutate(type = 'Inflow', cluster = clust) %>% rename(other_cluster = start_cluster) %>% select(-end_cluster)
  outflow <- m %>% filter(start_cluster == clust) %>% mutate(type = 'Outflow', cluster = clust) %>% rename(other_cluster = end_cluster) %>% select(-start_cluster)
  
  inflow_shp <- clust_shp %>% filter(date == unique(inflow %>% pull(date)), cluster %in% c(inflow %>% pull(other_cluster))) %>% 
                    mutate(type = 'Inflow', tar_cluster = unique(inflow %>% pull(cluster))) %>% 
    left_join(inflow, by = c('cluster' = 'other_cluster', 'date'))
  outflow_shp <- clust_shp %>% filter(date == unique(inflow %>% pull(date)), cluster %in% c(outflow %>% pull(other_cluster))) %>% 
                    mutate(type = 'Outflow', tar_cluster = unique(outflow %>% pull(cluster))) %>% 
                    left_join(outflow, by = c('cluster' = 'other_cluster', 'date'))
  
  flow[[clust]] <- rbind(inflow, outflow)
  flow_shp[[clust]] <- rbind(inflow_shp, outflow_shp)
  
}

flow <- do.call(rbind, flow)
flow_shp <- do.call(rbind, flow_shp)

max_connect <- flow_shp %>% 
  group_by(date, tar_cluster) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  slice(1)

map_data <- flow_shp %>% filter(tar_cluster == max_connect$tar_cluster, date == max_connect$date)
map_data %>% 
  ggplot() + 
  geom_sf(aes(fill = log(n_crisis, 10)), size = 0) + 
  colorspace::scale_fill_continuous_sequential('Reds') + 
  ggtitle(paste0(map_data$date, ' - ', map_data$tar_cluster))










