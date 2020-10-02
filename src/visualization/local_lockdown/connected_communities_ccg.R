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

asm <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/asmodee/20200924_Outlier_detection_k4_ccg.csv') %>% 
  rename(date = X1) %>% 
  pivot_longer(!date, names_to = "area", values_to = "value") %>% 
  mutate(area = stringr::str_to_upper(area))
  #mutate(area_type = stringr::str_sub(area, 1, 2)) %>% 
  #filter(area_type == 'E0')

a3 <- read_csv(.args[4])

mob <- read_csv(.args[5]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

gadm <- st_read(.args[6]) %>% 
  st_simplify(dTolerance = 0.001)

ccg <- st_read(.args[7]) %>% 
  st_simplify(dTolerance = 0.001) #%>% 
  #mutate(country = stringr::str_sub(lad19cd, 1, 1)) %>% 
  #filter(country == 'E')

ccg_ref <- read_csv(.args[8])

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

first_trigger <- asm %>% filter(area == 'E38000097', value == 'trigger') %>% pull(date) %>% min()

departure_connection <- mob_clusters %>% 
  filter(start_cluster %in% area_clusters) %>% 
  #filter(date >= first_trigger - 14 & date <= first_trigger) %>% 
  group_by(date, start_cluster, end_cluster) %>% 
  summarise(n_crisis = sum(n_crisis)) %>% 
  group_by(date) %>% 
  group_split()

#get which local authorities are in the connection at a given time

filter_label_map <- function(departure_connection, start_and_end = F){
  #extract all tiles in connected communities on a given day
  
  if (start_and_end){
    departure_connection_clust <- unique(departure_connection %>% pull(end_cluster), departure_connection %>% pull(start_cluster))
  } else {
    departure_connection_clust <- departure_connection %>% pull(end_cluster)
  }

  d <- departure_connection$date %>% unique()
  
  clusters <- label_map %>% filter(date == d, cluster %in% departure_connection_clust)
  
  clusters <- clusters %>% 
    left_join(departure_connection %>% select(-start_cluster), by = c('date', 'cluster' = 'end_cluster'))
  
  return(clusters)
  
}

ccg_intersection <- function(departure_connection){
  #get the local authority names of intersecting tiles 
  
  clusters <- filter_label_map(departure_connection)
  
  clusters <- clusters %>% 
    left_join(ccg_ref, by = c('quadkey')) %>% 
    select(quadkey, date, cluster, ccg18cd, n_crisis) 
    
  
  return(clusters)
  
  
}


plot_timepoint = function(departure_connection, start_and_end = F){
  
  clusters <- filter_label_map(departure_connection, start_and_end)
  
  d <- departure_connection$date %>% unique()
  
  p <- plot_clusters(clusters, basemap = uk, title = d)
  
  return(p)
  
}

p <- lapply(departure_connection[1:10], plot_timepoint, start_and_end = T)

p <- cowplot::plot_grid(plotlist = p)


departure_connection

ccg_intersections <- lapply(departure_connection, ccg_intersection) 
lm_filter <- lapply(departure_connection, filter_label_map) 

clust_levels <- do.call(rbind, departure_connection) %>% 
  group_by(end_cluster) %>% 
  summarise(n_crisis = sum(n_crisis), .groups = 'drop') %>% arrange(-n_crisis) %>% 
  pull(end_cluster)

connections <- do.call(rbind, ccg_intersections) %>% 
  group_by(cluster, ccg18cd) %>% 
  summarise(n_crisis = sum(n_crisis), .groups = 'drop') %>% 
  mutate(n_crisis_rank = rank(n_crisis))

connections$cluster <- factor(connections$cluster, levels = clust_levels)

date_arrival <- asm %>% 
  filter(date > first_trigger) %>% 
  filter(value == 'trigger') %>% 
  group_by(area) %>% 
  summarise(min_date = min(date), .groups = 'drop') %>% 
  mutate(days_from = as.numeric(min_date - first_trigger))

p2 <- connections %>% left_join(date_arrival, by = c('ccg18cd' = 'area')) %>% 
  replace_na() %>% 
  ggplot() + 
  geom_point(aes(x = n_crisis_rank, y = days_from)) + 
  theme_bw() + 
  xlab('Connection to Leicester community (rank)') + 
  ylab('Days from first Leicester trigger')


p2 <- connections %>% left_join(date_arrival, by = c('ccg18cd' = 'area')) %>% 
  ggplot() + 
  geom_boxplot(aes(x = cluster, y = days_from)) + 
  geom_jitter(aes(x = cluster, y = days_from)) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  ylab('Days from first Leicester trigger') + 
  xlab('Community (ranked by connection to Leicester, Left:High, Right:Low)')

ccg_simple <- ccg %>% st_simplify(dTolerance = 1000) %>% 
  st_transform(4326)

p_conn <- ccg_simple %>% 
  left_join(connections, by = c('ccg18cd' = 'ccg18cd')) %>% 
  #replace_na(list(n_crisis = 0)) %>% 
  ggplot() + 
  geom_sf(aes(fill = n_crisis), size= 0.1, color = 'black') + 
  colorspace::scale_fill_continuous_sequential('Blues', na.value="#ECECEC") + 
  ylim(52, 54) + 
  xlim(-3, 0) + 
  ggtitle('Connection')


p_df <- la_simple %>% 
  left_join(date_arrival, by = c('lad19cd' = 'area')) %>% 
  ggplot() + 
  geom_sf(aes(fill = days_from), size= 0.1, color = 'black') + 
  colorspace::scale_fill_continuous_sequential('Mint', na.value="#ECECEC") + 
  labs(fill = 'Days from\nfirst Leicester\ntrigger') + 
  theme_bw()


ggplot() + geom_sf(data = la_simple[1,])
connections

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/asm_p.png', p2)
ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/asm_connection.png', p_df)


connections %>% 
  left_join()

connected_las <- lapply(la_intersections, function(x){x$lad17cd}) %>% unlist() %>% unique()

asm %>% 
  filter(date >= i_date & date <= i_date + 14) %>% 
  mutate(connected = ifelse(area %in% connected_las, 'connected', 'not connected')) %>% 
  group_by(connected) %>% 
  mutate(connected_n = length(connected)) %>% 
  group_by(connected, value) %>% 
  summarise(n = n())

asm %>% 
  filter(date >= i_date & date <= i_date + 14) %>% 
  mutate(value_numeric = ifelse(value == 'normal', 0, 1)) %>% 
  mutate(connected = ifelse(area %in% connected_las, 'connected', 'not connected')) %>% 
  group_by(connected) %>% 
  mutate(connected_la_num = length(unique(area))) %>% 
  group_by(value, connected, connected_la_num) %>% 
  summarise(n = n(), 
            connected_la_num = unique(connected_la_num)) %>% 
  ungroup() %>% 
  mutate(norm = connected_la_num / n)










setdiff(colnames(asm), as.character(la$lad19cd))
la %>% filter(lad19cd %in% setdiff(as.character(la$lad19cd), colnames(asm))) %>% 
  ggplot() + geom_sf()

#%%
gadm <- st_read(.args[6]) %>% 
  st_simplify(dTolerance = 0.001)

departure_connection %>% ggplot() + geom_density(aes(x = log(n_crisis)))

departure_connection_clust <- departure_connection %>% pull(end_cluster)

tiles %>% 
  left_join(label_map %>% filter(date == i_date, cluster %in% departure_connection_clust)) %>% 
  drop_na(cluster) %>% 
  ggplot() + 
  geom_sf(aes(fill = cluster))




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



### GIF HERE
asm_date <- asm %>% 
  group_by(date) %>% 
  group_split()

plot_trigger <- function(asm_date){
  
  p <- ccg_simple %>% 
    left_join(asm_date, by = c('ccg18cd' = 'area')) %>% 
    drop_na(value) %>% 
    ggplot() + 
    geom_sf(aes(fill = value), size= 0.1, color = 'black') + 
    scale_fill_manual(values = c('normal' = 'lightblue', 'trigger' = 'red', 'nodata' = 'white', 'insuffdata' = 'lightgrey')) + 
    labs(fill = 'Days from\nfirst Leicester\ntrigger') + 
    theme_void() + 
    theme(legend.position = c(0.15, 0.5)) + 
    ggtitle(asm_date %>% pull(date) %>% unique())
  
  return(p)  
  
}

p <- lapply(asm_date, plot_trigger)

for (i in 1:length(p)){
  ggsave(paste0('/Users/hamishgibbs/Downloads/trigger_gif/', i, '_frame.png'), p[[i]])
}

require(magick)

list.files(path='/Users/hamishgibbs/Downloads/trigger_gif', pattern = '*.png', full.names = F) %>% 
  sort()

paste0('/Users/hamishgibbs/Downloads/trigger_gif/', 1:length(p), '_frame.png') %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("/Users/hamishgibbs/Downloads/trigger_gif/trigger.gif") 







