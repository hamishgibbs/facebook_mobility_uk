suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  library(RColorBrewer)
})

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/leiden/leiden_hierarchy.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

lei_r <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

lei_r %>% 
  group_by(date, resolution, cluster) %>% 
  summarise(n = n()) %>% 
  group_by(date, resolution) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_path(aes(x = resolution, y = n, color = as.character(date), group = as.character(date))) + 
  geom_point(aes(x = resolution, y = n, color = as.character(date), group = as.character(date)))

lei_r <- lei_r %>% 
  group_by(date, resolution) %>% 
  mutate(n_clusters = length(unique(cluster)),
         mean_size = length(unique(quadkey)) / n_clusters) %>% 
  filter(mean_size > 5)

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

lei_r <- lei_r %>% 
  group_by(date) %>% 
  group_split()

qualitative_pal <- function(names, rep_n = 3){
  
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), rep_n)
  
  pal <- pal[1:length(names)]
  names(pal) = names
  
  return(pal)
  
}

plot_clusters <- function(clust){
  clust <- tiles %>% 
    left_join(clust, by = 'quadkey') %>% 
    drop_na(cluster)
  
  pal <- qualitative_pal(unique(clust$cluster), round(length(unique(clust$cluster)) / 80, 0) + 100)
  
  n_clust <- length(unique(clust$cluster))
  res <- unique(clust$resolution)
  
  bbox <- st_bbox(clust)
  
  p <- clust %>% 
    ggplot() + 
    geom_sf(aes(fill = as.character(cluster)), size = 0) + 
    geom_sf(data = world, size = 0.1, colour = 'black', fill = 'transparent') + 
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) +
    scale_fill_manual(values = pal) + 
    theme_void() + 
    theme(legend.position = 'none') + 
    ggtitle(paste(n_clust, 'Communities, Resolution:', round(res, 2)))
  
  return(p)
  
}

#this needs to be done for each date
sample_resolutions <- function(lei_r){
  return(c(1, as.integer(length(lei_r) * 0.25), as.integer(length(lei_r) * 0.75), as.integer(length(lei_r))))
}

plot_resolution_sample <- function(lei_r){
  
  lei_date <- lei_r %>% 
    group_by(resolution) %>% 
    group_split()
  
  p <- lapply(lei_date[sample_resolutions(lei_date)], plot_clusters)
  
  p <- cowplot::plot_grid(plotlist = p, nrow = 1)
  
  title <- cowplot::ggdraw() + cowplot::draw_label(unique(lei_r$date), fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  p <- cowplot::plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))
  
  return(p)
  
}

p1 <- plot_resolution_sample(lei_r[[1]])
p2 <- plot_resolution_sample(lei_r[[2]])
p3 <- plot_resolution_sample(lei_r[[3]])

p <- cowplot::plot_grid(p1, p2, p3, nrow = 3)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/leiden_hierarchy.png', p,
       width = 14, height = 15,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/leiden_hierarchy.pdf', p,
       width = 14, height = 6,
       units = 'in', useDingbats = F)


