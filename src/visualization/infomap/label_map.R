

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

uuids = label_map %>% pull(cluster) %>% unique()

uuids[39]

cluster_date_data <- label_map %>% filter(cluster == 'abf41ead-2d89-457f-a577-d94f5a1ed69f') %>% 
  group_by(date) %>% 
  group_split()

plot_cluster_timepoint = function(cluster_date){
  d <- tiles %>% 
    left_join(cluster_date, by = c('quadkey')) %>% 
    drop_na(cluster)
  
  date = d %>% pull(date) %>% unique()
  
  bbox <- st_bbox(d)
  
  p <- d %>% 
    ggplot() + 
    geom_sf(size = 0.1, colour = 'black', fill = 'lightblue') + 
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) +
    theme_void() + 
    ggtitle(date)
  
  return(p)
  
}

p <- lapply(cluster_date_data[1:36], plot_cluster_timepoint)

p <- cowplot::plot_grid(plotlist = p)


ggsave(tail(.args, 1), p,
       width = 19, height = 20,
       units = 'in')

