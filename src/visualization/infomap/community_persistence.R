#creates fig 1 in paper - summary of InfoMap
suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  library(RColorBrewer)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_bank_holidays.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_weekends.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/community_persistence.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"),
         method = 'InfoMap')

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

persist <- im %>% 
  group_by(quadkey) %>% 
  summarise(n_communities = length(unique(cluster)),
            days_present = n(),
            n_communities_norm = n_communities / days_present)

persist <- tiles %>% 
  left_join(persist, by = 'quadkey')

p_raw <- persist %>% 
  drop_na(n_communities) %>% 
  ggplot() + 
  geom_sf(data = world, size = 0.1, colour = 'black', fill = '#EBEBEB') + 
  geom_sf(aes(fill = n_communities), size = 0) + 
  colorspace::scale_fill_continuous_sequential('Blues', rev = F) + 
  geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
  theme_void() + 
  plot_default_theme + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) + 
  labs(fill = 'Community\nlabels') + 
  ggtitle('Number of community labels')

p_norm <- persist %>% 
  drop_na(n_communities) %>% 
  ggplot() + 
  geom_sf(data = world, size = 0.1, colour = 'black', fill = '#EBEBEB') + 
  geom_sf(aes(fill = n_communities_norm), size = 0) + 
  colorspace::scale_fill_continuous_sequential('Blues', rev = F) + 
  geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
  theme_void() + 
  plot_default_theme + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) + 
  labs(fill = 'Normalised\nCommunity\nlabels') + 
  ggtitle('Community labels by cell presence')

p_always <- persist %>% 
  filter(n_communities == 1) %>% 
  ggplot() + 
  geom_sf(data = world, size = 0.1, colour = 'black', fill = '#EBEBEB') + 
  geom_sf(aes(fill = 'Stable\ncommunities'), size = 0) + 
  scale_fill_manual(values = c('Stable\ncommunities' = 'darkgreen')) + 
  geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
  theme_void() + 
  plot_default_theme + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) + 
  labs(fill = '') + 
  ggtitle('Stable communities')
  
p <- cowplot::plot_grid(p_raw, p_norm, p_always, nrow = 1)

ggsave(tail(.args, 1), 
       p, 
       width = 14, 
       height = 6, 
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       useDingbats = FALSE,
       units = 'in')