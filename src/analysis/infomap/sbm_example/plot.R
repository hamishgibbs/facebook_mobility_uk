suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})

if(interactive()){
  .args <- c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/analysis/infomap/sbm_example/processed/sbm.csv',
             '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/analysis/infomap/sbm_example/data/tiles_zoom_12.shp', 
             '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/analysis/infomap/sbm_example/output/sbm.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

sbm <- read_csv(.args[1], col_types = cols())

tiles <- st_read(.args[2], quiet = TRUE) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

sbm <- sbm %>% group_by(level) %>% group_split()

plot_partition <- function(comm){
  
  p <- tiles %>% 
    left_join(comm, by = c('quadkey')) %>% 
    drop_na(cluster) %>% 
    ggplot() + 
    geom_sf(aes(fill = as.character(cluster)), size = 0) +
    theme_void() + 
    theme(legend.position = 'none') + 
    ggtitle(paste0('Level ', unique(comm$level), ' - ', length(unique(comm$cluster)), ' Blocks'))
    
  return(p)
  
}

p <- lapply(sbm, plot_partition)

p <- cowplot::plot_grid(plotlist = p)

ggsave(.args[length(.args)], p, width = 8.5, y = 6, units = 'in')
