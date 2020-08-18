#Visualise OA population vs mean FB pop (internal movement) per tile over time periods 
suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/imd_reference/quadkey_imd.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_imd_pop_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1])

oa_pop <- read_csv(.args[2], col_types = cols()) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

qk_imd <- read_csv(.args[3]) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

tiles <- st_read(.args[4]) %>% 
  st_set_crs(4326)

inter_mob <- mob %>% 
  filter(date <= as.Date('2020-07-01')) %>% 
  filter(start_quadkey == end_quadkey) %>% 
  group_by(start_quadkey) %>% 
  summarise(n_crisis = median(n_crisis, na.rm = T), .groups = 'drop') %>% 
  left_join(oa_pop, by = c('start_quadkey' = 'quadkey_12')) %>% 
  drop_na(pop) %>% 
  mutate(pop_ratio = n_crisis / pop)

p <- inter_mob %>% 
  left_join(qk_imd, by = c('start_quadkey' = 'quadkey_12')) %>% 
  ggplot() + 
  geom_point(aes(x = pop_ratio * 100, y = wm_imd_rank, color = country), size = 0.1) +
  scale_color_discrete_qualitative('Harmonic') + 
  facet_wrap(~country, nrow = 2, scales = 'free') + 
  theme_bw() + 
  xlab('% Facebook users') +
  ylab('Weigted mean IMD rank') +
  plot_default_theme + 
  theme(legend.position = 'none') + 
  ggtitle('a')

write_rds(p, gsub('.png', '.rds', tail(.args, 1)))
ggsave(tail(.args, 1), p,
       width = 8.5, height = 6,
       units = 'in')

a <- read_rds('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_imd_pop_comparison.rds')
b <-  read_rds('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_pop_dens_pop_comparison.rds')

p <- cowplot::plot_grid(a, b)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_census_comparison.png', p,
       width = 8.5, height = 5,
       units = 'in')

