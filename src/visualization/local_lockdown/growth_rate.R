suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(RColorBrewer)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/qualitative_pal.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/utils.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Downloads/gadm36_GBR_shp (2)/gadm36_GBR_3.shp',
              '/Users/hamishgibbs/Downloads/Local_Authority_Districts__December_2019__Boundaries_UK_BFC-shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFC.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/la_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain-shp/la_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

cases <- readr::read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>% 
  rename(date = `Specimen date`) %>% 
  rename(code = `Area code`) %>% 
  rename(count = `Daily lab-confirmed cases`) %>% 
  mutate(area_type = stringr::str_sub(code, 1, 2)) %>% 
  filter(area_type == 'E0')

a3 <- read_csv(.args[4])

mob <- read_csv(.args[5]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

la <- st_read(.args[7]) %>% 
  st_simplify(dTolerance = 0.001) %>% 
  mutate(country = stringr::str_sub(lad19cd, 1, 1)) %>% 
  filter(country == 'E') %>% 
  st_transform(4326)

la_simple <- la %>% st_simplify(dTolerance = 100) %>% 
  st_transform(4326)

la_pop <- read_csv(.args[9]) %>% 
  rename(pop = `All ages`)

cases_wk <- cases %>% 
  #mutate(week = lubridate::week(date)) %>% 
  #group_by(week, code) %>% 
  #summarise(count = sum(count, na.rm = T), .groups = 'drop') %>% 
  left_join(la_pop, by = c('code')) %>% 
  select(-Geography1) %>% 
  mutate(cases_per = (count / pop ) * 100000) %>% 
  select(date, code, count, cases_per)

cases_clust <- cases_wk %>% 
  left_join(label_map_la, by = c('date', 'code')) %>% 
  drop_na(cluster)

cases_clust %>% 
  group_by(date, code) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

pal <- qualitative_pal(cases_clust %>% pull(cluster) %>% unique(), 300)

cases_clust %>% 
  ggplot() + 
  geom_point(aes(x = date, y = cases_per, color = cluster)) +
  scale_color_manual(values = pal) + 
  theme(legend.position = 'none')



label_map_la <- label_map %>% 
  left_join(la_ref, by = c('quadkey')) %>% 
  select(-X1.x, -X1.y, -lad17nm, -lad17nmw, -st_areasha, -st_lengths) %>% 
  rename(code = lad17cd) %>% 
  select(date, code, cluster) %>% 
  distinct() 

gr <- cases %>% 
  group_by(date, code) %>% 
  arrange(date) %>% 
  group_by(code) %>% 
  mutate(count_prev = RcppRoll::roll_sum(lag(count, 14), 14, align = "right", fill = NA),
         count_curr = RcppRoll::roll_sum(count, 14, align = "right", fill = NA)) %>% 
  mutate(gr = count_curr - count_prev) %>% 
  ungroup()

ggplot() + geom_path(data = gr, aes(x = date, y = count_curr, group = code))

gr_date <- gr %>% group_by(date) %>% 
  group_split()

gr_log <- gr %>% 
  filter(gr != 0) %>% 
  mutate(neg = ifelse(gr < 0, T, F),
         gr = abs(gr),
         gr = log(gr, 10),
         gr = ifelse(neg, gr * -1, gr))

date_p <- gr %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_point(aes(x = date, y = 1), color = 'white') + theme_bw() + 
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank())

plot_gr <- function(gr_date){
  
  bbox <- st_bbox(la_simple)
  
  scale_max <- max(gr_log$gr, na.rm = T)
  scale_min <- min(gr_log$gr, na.rm = T)
  
  p <- la_simple %>% 
    left_join(gr_date, by = c('lad19cd' = 'code')) %>% 
    drop_na(gr) %>% 
    filter(gr != 0) %>% 
    mutate(neg = ifelse(gr < 0, T, F),
           gr = abs(gr),
           gr = log(gr, 10),
           gr = ifelse(neg, gr * -1, gr)) %>% 
    ggplot() + 
    geom_sf(data = la_simple, size= 0.1, color = 'black', fill = '#F8F8F8') + 
    geom_sf(aes(fill = gr), size= 0.1, color = 'black') + 
    scale_fill_gradient2(low = 'darkblue', mid = 'white', high = 'darkred', midpoint = 0, limits = c(scale_min, scale_max)) + 
    theme_void() + 
    xlim(bbox[1], bbox[3]) + 
    ylim(bbox[2], bbox[4]) + 
    theme(legend.position = c(0.15, 0.5)) + 
    ggtitle(gr_date %>% pull(date) %>% unique()) + 
    labs(fill = 'Growth\nRate (log)')
  
  p_date <- date_p + geom_vline(aes(xintercept = unique(gr_date %>% pull(date))), color = 'red')
  
  p <- cowplot::plot_grid(p, p_date, ncol = 1, rel_heights = c(0.9, 0.1))
   
  return(p)  
  
}

area <- a3 %>% filter(NAME_3 == 'Leicester') %>% pull(quadkey)
i_date <- as.Date('2020-06-29')

#which communities included those tiles at any point in an interval?
area_communities_interval <- label_map %>% filter(quadkey %in% area) %>% 
  filter(date > i_date - 14 & date <= i_date) 

#for lei only one
area_clusters <- area_communities_interval %>% pull(cluster) %>% unique()

#which other quadkeys were in that community(ies) in an interval
community_quadkeys <- label_map %>% filter(cluster %in% area_clusters) %>% 
  filter(date > i_date - 14 & date <= i_date) %>% pull(quadkey) %>% unique()

#which LAs were in the quadeys in the community in that interval?
community_las <- la_ref %>% filter(quadkey %in% community_quadkeys) %>% 
  group_by(lad17cd, lad17nm) %>% summarise(n = n(), .groups='drop') %>% 
  add_row(lad17cd = 'E07000135', lad17nm = 'Oadby and Wigston', n = 5)

p <- gr %>% filter(code %in% community_las$lad17cd) %>% 
  ggplot() + 
  geom_path(data = gr, aes(x = date, y = gr, group = `Area name`), color = 'black', size = 0.1) + 
  geom_path(aes(x = date, y = gr, group = `Area name`), color = 'red', size = 0.3) + 
  theme_bw() + 
  ylab('Growth Rate') + 
  theme(axis.title.x = element_blank())

p_cases <- cases %>% 
  filter(code %in% community_las$lad17cd) %>% 
  ggplot() + 
  geom_path(data = cases, aes(x = date, count, group = `Area name`), color = 'black', size = 0.1) + 
  geom_path(aes(x = date, y = count, group = `Area name`), color = 'red', size = 0.3) + 
  theme_bw() + 
  ylab('Cases') + 
  theme(axis.title.x = element_blank())

p_grid <- cowplot::plot_grid(p, p_cases, ncol = 1)
ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/growth_rate_communities.png', p_grid, width = 8.5, height = 4, units= 'in')

comm_tiles <- tiles %>% filter(quadkey %in% community_quadkeys)

comm_las <- la %>% filter(lad19nm %in% community_las$lad17nm)

p_map <- ggplot() + 
  geom_sf(data = comm_las, fill = '#E6E6E6') + 
  geom_sf(data = comm_tiles, fill = 'transparent') + 
  theme_void() 

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/growth_rate_communities_map.png', p_map, width = 6, height = 6, units= 'in')

leaflet() %>% addTiles() %>% addPolygons(data = comm_las, popup = ~lad19nm)

la %>% filter(lad19nm)

plot_gr(gr_date[[100]])

p <- lapply(gr_date, plot_gr)

for (i in 1:length(p)){
  ggsave(paste0('/Users/hamishgibbs/Downloads/gr_gif/', i, '_frame.png'), p[[i]], width = 4.5, height = 6, units= 'in',
         dpi=120)
}

require(magick)

list.files(path='/Users/hamishgibbs/Downloads/gr_gif', pattern = '*.png', full.names = F) %>% 
  sort()

paste0('/Users/hamishgibbs/Downloads/gr_gif/', 1:length(p), '_frame.png') %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("/Users/hamishgibbs/Downloads/gr_gif/gr.gif") 





