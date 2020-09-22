suppressPackageStartupMessages({
  require(RColorBrewer)  
})

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Downloads/leiden_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  filter(date == min(date))

lei <- read_csv(.args[2], col_types = cols(quadkey = col_character())) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = '0'))
  

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

im_clust <- tiles %>% 
  left_join(im, by = c('quadkey')) %>% 
  left_join(a3, by = c('quadkey')) %>% 
  drop_na(NAME_0) %>% 
  drop_na(cluster)

lei_clust <- tiles %>% 
  left_join(lei, by = c('quadkey')) %>% 
  left_join(a3, by = c('quadkey')) %>% 
  drop_na(NAME_0) %>% 
  drop_na(cluster) %>% 
  mutate(date = as.Date('2020-03-19'),
         cluster = as.character(cluster))

plot_cluster <- function(clust, algo){
  cluster_names <- clust %>%  pull(cluster) %>% unique()
  
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), 3)
  
  pal <- pal[1:length(cluster_names)]
  names(pal) = cluster_names
  
  date = clust %>% pull(date) %>% unique()
  
  bbox <- st_bbox(clust)
  
  p <- clust %>% 
    ggplot() + 
    geom_sf(aes(fill = cluster), size = 0.05, colour = 'black') + 
    scale_fill_manual(values = pal) +
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) +
    theme_void() + 
    ggtitle(paste0(algo, ' - ', format(date, "%a %b %d"), ' - ', length(cluster_names), ' clusters')) + 
    theme(legend.position = 'none')
  
  return(p)
  
}

p_im <- plot_cluster(im_clust, 'Infomap')

p_lei <- plot_cluster(lei_clust, 'Leiden')

p <- cowplot::plot_grid(p_im, p_lei)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/im_lei_comparison.png', p,
       width = 6, height = 6,
       units = 'in')

