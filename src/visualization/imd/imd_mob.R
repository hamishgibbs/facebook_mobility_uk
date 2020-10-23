#what are journeys kn Fridays and saturdays that have the highest beviation from baseline?
suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/imd_reference/quadkey_imd.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/imd_mob.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character()))

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

qk_imd <- read_csv(.args[3]) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0")) %>% 
  rename(quadkey = quadkey_12) %>% 
  group_by(country) %>% 
  mutate(imd = rank(-wm_imd_rank),
         quintile = ntile(-imd, 5)) %>% 
  ungroup()

#check plot
tiles %>% 
  left_join(qk_imd, by = c('quadkey')) %>% 
  drop_na(imd) %>% 
  ggplot() + 
  geom_sf(aes(fill = quintile), size = 0)
#1 is most deprived

#aggregate differently - Median IMD? - like unequal
imd_mob <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  left_join(qk_imd %>% select(quadkey, quintile, country), by = c('start_quadkey' = 'quadkey')) %>% 
  rename(start_quint = quintile) %>% 
  left_join(qk_imd %>% select(quadkey, quintile), by = c('end_quadkey' = 'quadkey')) %>% 
  rename(end_quint = quintile) %>% 
  drop_na(start_quint, end_quint)

p <- imd_mob %>% 
  group_by(date, start_quint, country) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T),
            n_baseline = sum(n_baseline, na.rm = T)) %>% 
  mutate(perc_change = (n_crisis - n_baseline) / n_baseline) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = perc_change * 100, color = as.character(start_quint))) + 
  scale_color_manual(values = c('#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081')) + 
  theme_bw() + 
  plot_default_theme + 
  labs(color = 'IMD Quintile') + 
  facet_wrap(~country, scales = 'free') + 
  ylab('Percent change from baseline')

  
ggsave(tail(.args, 1), p,
       width = 8.5, height = 6,
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), p,
       width = 8.5, height = 6,
       units = 'in')





