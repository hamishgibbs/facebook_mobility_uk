suppressPackageStartupMessages({
  require(RColorBrewer)
  require(tidyverse)
  require(sf)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full_norm_months.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/leiden_full_norm_months.csv',
              '/Users/hamishgibbs/Downloads/scan_results_marjun_nooverlap',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Downloads/Middle_Layer_Super_Output_Areas__December_2011__Boundaries-shp/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

#combine mob by month and repeat clustering
im <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  group_by(date) %>% 
  group_split()

im <- im[1:4]

lei <- read_csv(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  rename(date = month) %>%
  group_by(date) %>% 
  group_split()

lei <- lei[1:4]

mob <- read_csv(.args[4], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character()))

mob <- mob %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(journey, start_quadkey, end_quadkey, month) %>% 
  summarise(n_crisis = n_crisis)

mob <- mob %>% group_by(month) %>% 
  group_split()

mob <- mob[1:4]

cluster_pop_data <- function(mob, clust){
  
  cpd <- mob %>% 
    left_join(clust, by = c('start_quadkey' = 'quadkey')) %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(n_crisis = sum(n_crisis))
  
  return(cpd)
  
}


plot_pop <- function(mob, clust, title){
  
  p_data <- tiles %>% 
    left_join(clust %>% left_join(cluster_pop_data(mob, clust))) %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(n_crisis = unique(n_crisis)) %>% 
    mutate(tot_pop = sum(n_crisis),
           n_crisis = (n_crisis / tot_pop) * 100)
  
  bbox <- st_bbox(p_data)
    
  p <- p_data %>% 
    ggplot() + 
    geom_sf(data = uk, fill = 'transparent', size = 0.05, color = 'black') + 
    geom_sf(aes(fill = n_crisis), size = 0.1, color = 'black') + 
    scale_fill_continuous_sequential('Mint') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) + 
    theme_void() + 
    plot_default_theme + 
    theme(legend.position = c(0.1, 0.7)) + 
    labs(fill = '% Population') + 
    ggtitle(title)
  
  return(p)
}

plot_pop2 <- function(mob, clust, title){
  
  p_data <- tiles %>% 
    left_join(clust %>% left_join(cluster_pop_data(mob, clust))) %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(n_crisis = unique(n_crisis)) %>% 
    mutate(tot_pop = sum(n_crisis),
           n_crisis = (n_crisis / tot_pop) * 100)
  
  bbox <- st_bbox(p_data)
  
  p <- p_data %>% 
    ggplot() + 
    geom_sf(data = uk, fill = 'transparent', size = 0.05, color = 'black') + 
    geom_sf(aes(fill = n_crisis), size = 0.1, color = 'black') + 
    scale_fill_continuous_sequential('Blues') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) + 
    theme_void() + 
    plot_default_theme + 
    theme(legend.position = c(0.1, 0.7)) + 
    labs(fill = '% Population') + 
    ggtitle(title)
  
  return(p)
}

pl <- list()

pl[[1]] <- plot_pop(mob[[1]], im[[1]], 'March - Infomap')
pl[[2]] <- plot_pop2(mob[[1]], lei[[1]], 'March - Leiden')
pl[[3]] <- plot_pop(mob[[2]], im[[2]], 'April - Infomap')
pl[[4]] <- plot_pop2(mob[[2]], lei[[2]], 'April - Leiden')
pl[[5]] <- plot_pop(mob[[3]], im[[3]], 'May - Infomap')
pl[[6]] <- plot_pop2(mob[[3]], lei[[3]], 'May - Leiden')
pl[[7]] <- plot_pop(mob[[4]], im[[4]], 'June - Infomap')
pl[[8]] <- plot_pop2(mob[[4]], lei[[4]], 'June - Leiden')

p <- cowplot::plot_grid(plotlist = pl, nrow = 2)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/community_population.png', p,
       width = 9, height = 7,
       units = 'in')
ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/community_population.pdf', p,
       width = 9, height = 7,
       units = 'in',
       useDingbats = F)

