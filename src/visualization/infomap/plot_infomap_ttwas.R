#Visualise OA population vs mean FB pop (internal movement) per tile over time periods 
suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Downloads/baseline_clust.csv',
              '/Users/hamishgibbs/Downloads/Travel_to_Work_Areas__December_2011__Boundaries-shp (1)/Travel_to_Work_Areas__December_2011__Boundaries.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp')
} else {
  .args <- commandArgs(trailingOnly = T)
}

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')

clust = read_csv(.args[1]) %>% 
  mutate(quadkey = as.character(quadkey), 
         quadkey = str_pad(quadkey, 12, pad = "0"))

ttwa = st_read(.args[2]) %>% 
  st_transform(4326)

tiles = st_read(.args[3]) %>% 
  st_set_crs(4326)

tiles_cent = tiles %>% 
  mutate(geometry = st_centroid(geometry))

intersection = st_intersection(ttwa, tiles_cent)

comp = intersection %>% 
  left_join(clust, by = 'quadkey') %>% 
  drop_na(cluster) %>% 
  select(ttwa11cd, quadkey, cluster) %>% 
  st_drop_geometry() %>% 
  mutate(ttwa11cd = as.character(ttwa11cd),
         cluster = as.character(cluster))

ttwas <- unique(comp$ttwa11cd)
ref = list()
for (i in 1:length(ttwas)){
  ref[[ttwas[i]]] = i
}

tt_map <- c()
for (tt in comp$ttwa11cd){
  tt_map <- append(tt_map, ref[[tt]])
}

map_tt <- function(tts){
  tt_map <- c()
  for (tt in tts){
    tt_map <- append(tt_map, ref[[tt]])
  }
  
  return(tt_map)
}

#not sure if it is ok to use on clusters with shifting labels
igraph::compare(tt_map, comp$cluster, method = 'nmi')


ti <- tiles %>% 
  left_join(clust, by = c('quadkey')) %>% 
  drop_na(cluster) %>% 
  group_by(cluster) %>% 
  summarise() %>% 
  ggplot() + 
  geom_sf(aes(fill = as.character(cluster)), size = 0) + 
  geom_sf(data = world, size = 0.2, colour = 'black', fill = 'transparent') + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) +
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

tt_mapping <- map_tt(ttwa$ttwa11cd)

tt <- ttwa %>% 
  mutate(cluster = tt_mapping)
  ggplot() + 
  geom_sf(aes(fill = map_tt(ttwa11cd)), size = 0) + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) +
  geom_sf(data = world, size = 0.2, colour = 'black', fill = 'transparent') + 
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

p <- cowplot::plot_grid(ti, tt)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/im_ttwa.png', p,
       width = 8.5, height = 6,
       units = 'in')
