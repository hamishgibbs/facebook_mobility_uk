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
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_oa_pop_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1])

oa_pop <- read_csv(.args[2], col_types = cols()) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

tiles <- st_read(.args[3]) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')

inter_mob <- mob %>% 
  filter(date <= as.Date('2020-07-01')) %>% 
  filter(start_quadkey == end_quadkey) %>% 
  group_by(start_quadkey) %>% 
  summarise(n_crisis = median(n_crisis, na.rm = T), .groups = 'drop') %>% 
  left_join(oa_pop, by = c('start_quadkey' = 'quadkey_12')) %>% 
  drop_na(pop) %>% 
  mutate(pop_ratio = n_crisis / pop)

p_map <- tiles %>% 
  left_join(inter_mob, by = c('quadkey' = 'start_quadkey')) %>% 
  drop_na(pop) %>% 
  ggplot() + 
  geom_sf(aes(fill = log(pop_ratio * 100)), size = 0) + 
  scale_fill_continuous_sequential('BluGrn') + 
  geom_sf(data = world, size = 0.2, colour = 'black', fill = 'transparent') + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) +
  labs(fill = 'Log % of\nFacebook\nusers') + 
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = c(0.9, 0.8),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size = 8)) + 
  ggtitle('c')
#needs arrow pointing to median
p_dens <- inter_mob %>% 
  left_join(a3, by = c('start_quadkey' = 'quadkey')) %>% 
  drop_na(NAME_1) %>% 
  ggplot() + 
  geom_vline(aes(xintercept = median(inter_mob %>% pull(pop_ratio) * 100)), linetype = 'dashed', size = 0.1) + 
  geom_density(aes(x = pop_ratio * 100, group = NAME_1, color = NAME_1)) + 
  scale_color_discrete_qualitative('Harmonic') + 
  labs(color = 'Country') + 
  ylab('Density of total tiles') + 
  xlab('% Facebook users') +
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = c(0.8, 0.73),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size = 8)) + 
  ggtitle('a')

p_pt <- inter_mob %>% 
  ggplot() + 
  geom_point(aes(x = log(pop, 10), y = log(n_crisis, 10)), size = 0.01) + 
  geom_abline(aes(intercept = 0, slope = 1), linetype = 'dashed', size = 0.5) + 
  xlim(1.8, 6) + 
  ylim(0.5, 6) + 
  ylab('N Facebook users (log)') + 
  xlab('Population (log)') + 
  theme_bw() + 
  plot_default_theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), 
        text = element_text(size = 8)) + 
  ggtitle('b')

#this needs another panel
g <- cowplot::plot_grid(p_dens, p_pt, ncol = 1)

p <- cowplot::plot_grid(g, p_map, rel_widths = c(0.4, 0.6))

ggsave(tail(.args, 1), p,
       width = 8.5, height = 6,
       units = 'in')

