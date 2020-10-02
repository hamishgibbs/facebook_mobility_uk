suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_hours.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/imd_reference/quadkey_imd.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/imd_reference/quadkey_mean_age.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/imd_reference/quadkey_mean_perc_white.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_census.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1])

oa_pop <- read_csv(.args[2], col_types = cols()) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

qk_imd <- read_csv(.args[3]) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

qk_age <- read_csv(.args[4]) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

qk_eth <- read_csv(.args[5]) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"),
         wm_perc_white = 100 * (1 - wm_perc_white))

tiles <- st_read(.args[6]) %>% 
  st_set_crs(4326)

inter_mob <- mob %>% 
  filter(start_quadkey == end_quadkey) %>% 
  group_by(start_quadkey) %>% 
  summarise(n_crisis = median(n_crisis, na.rm = T), .groups = 'drop') %>% 
  left_join(oa_pop, by = c('start_quadkey' = 'quadkey_12')) %>% 
  drop_na(pop) %>% 
  mutate(pop_ratio = n_crisis / pop)

qk_dens <- tiles %>% 
  left_join(inter_mob, by = c('quadkey' = 'start_quadkey')) %>% 
  drop_na(pop_ratio) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>% 
  st_drop_geometry() %>% 
  mutate(pop_dens = pop / area) %>% 
  rename(quadkey_12 = quadkey) %>% 
  select(quadkey_12, pop_dens) %>% 
  left_join(qk_imd, by = c('quadkey_12')) %>% 
  select(-wm_imd_rank)

plot_comparison <- function(df, varname, ylab, p_index, log_10 = FALSE){
  
  if (log_10){
    geom <- geom_point(aes(x = pop_ratio * 100, y = log(!! sym(varname), 10)), size = 0.1)
  } else {
    geom <- geom_point(aes(x = pop_ratio * 100, y = !! sym(varname)), size = 0.1)
  }
  
  p <- inter_mob %>% 
    left_join(df, by = c('start_quadkey' = 'quadkey_12')) %>% 
    filter(pop_ratio < 1) %>% 
    ggplot() + 
    geom +
    scale_color_discrete_qualitative('Harmonic') + 
    facet_wrap(~country, nrow = 2, scales = 'free') + 
    theme_bw() + 
    xlab('% Facebook users') +
    ylab(ylab) +
    plot_default_theme + 
    theme(legend.position = 'none', 
          plot.margin = margin(t = 0, r = 0, b = 0, l = 5, unit = "pt")) + 
    ggtitle(p_index)
  
  return(p)
}

p_imd <- plot_comparison(qk_imd, 'wm_imd_rank', 'Tile Socioeconomic Deprivation', 'a')
p_age <- plot_comparison(qk_age, 'wm_age', 'Tile Average Age', 'b')
p_perc_white <- plot_comparison(qk_eth, 'wm_perc_white', 'Tile % Minority Ethnic', 'c')
p_pop_dens <- plot_comparison(qk_dens, 'pop_dens', 'Population Density (log)', 'd', log_10 = T)

p <- cowplot::plot_grid(p_imd, p_age, p_perc_white, p_pop_dens, ncol = 2)

ggsave(tail(.args, 1), p,
       width = 8.5, height = 6,
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), p,
       width = 8.5, height = 6,
       units = 'in')



