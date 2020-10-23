#Visualise OA population vs mean FB pop (internal movement) per tile over time periods 
suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
})

#Some case and intervention dataa

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_oa_pop_comparison.png',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/n_tiles.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character()))


a3 <- read_csv(.args[2])

start <- mob %>% select(date, start_quadkey) %>% 
  rename(quadkey = start_quadkey) %>% 
  left_join(a3, by = c('quadkey')) %>% 
  drop_na(GID_0)

end <- mob %>% select(date, end_quadkey) %>% 
  rename(quadkey = end_quadkey) %>% 
  left_join(a3, by = c('quadkey')) %>% 
  drop_na(GID_0)

tile_n <- rbind(start, end) %>% 
  select(date, quadkey) %>% 
  distinct()

p <- tile_n %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n)) + 
  theme_bw() + 
  plot_default_theme + 
  ylab('Number of tiles') + 
  theme(axis.title.x = element_blank())

ggsave(tail(.args, 1), p,
       width = 8, height = 2,
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), p,
       width = 8, height = 2,
       units = 'in')


