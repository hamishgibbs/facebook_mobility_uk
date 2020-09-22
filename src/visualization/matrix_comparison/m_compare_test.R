suppressPackageStartupMessages({
  require(tidyverse)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/canberra_distance/c_dist_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/matrix_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

m_compare <- read_csv(.args[1])

m_compare %>% 
  ggplot() + 
  geom_density(aes(x = c_dist))

p <- m_compare %>% 
  ggplot() + 
  geom_raster(aes(date_x, date_y, fill = c_dist)) + 
  scale_fill_continuous_sequential('Mint', rev = T) + 
  labs(fill = 'Canberra\nDistance') +
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title = element_blank())

p 

write_rds(p, '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/matrix_comparison.rdf')

ggsave(tail(.args, 1), p,
       width = 6, height = 5,
       units = 'in')
