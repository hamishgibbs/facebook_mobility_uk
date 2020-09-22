suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  library(RColorBrewer)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/sbm_full_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

sbm <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

sbm %>% 
  group_by(level) %>% 
  summarise(n_clust = length(unique(cluster))) %>% 
  ggplot() + 
  geom_path(aes(x = level, y = rev(n_clust)))

sbm <- sbm %>% group_by(level) %>% 
  group_split()

plot_clusters(sbm[[1]], uk_lims = T)

plist = lapply(sbm, plot_clusters, uk_lims = T)

p <- cowplot::plot_grid(plotlist = plist)

ggsave('/Users/hamishgibbs/Downloads/test.png', p,
       width = 9, height = 7,
       units = 'in')

