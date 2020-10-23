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
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_census.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

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
  select(-wm_imd_rank) %>% 
  as_tibble()

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

plot_census_var <- function(var, tiles, legend_title = NULL, title = NULL){
  
  var <- var %>% 
    group_by(country) %>% 
    group_split()
  
  var <- lapply(var, function(x){
    geodata <- tiles %>% 
      left_join(x, by = c('quadkey' = 'quadkey_12')) %>% 
      drop_na(value)
    
    return(geodata)
  })
  
  p <- ggplot() + 
    geom_sf(data = world, size = 0.1, colour = 'black', fill = '#EBEBEB') + 
    geom_sf(data = var[[1]], aes(fill = value), size = 0) + 
    colorspace::scale_fill_continuous_sequential('Mint', rev=F) + 
    ggnewscale::new_scale_fill() + 
    geom_sf(data = var[[2]], aes(fill = value), size = 0) + 
    colorspace::scale_fill_continuous_sequential('Blues', rev=F) + 
    ggnewscale::new_scale_fill() + 
    geom_sf(data = var[[3]], aes(fill = value), size = 0) + 
    colorspace::scale_fill_continuous_sequential('Teal', rev=F) + 
    ggnewscale::new_scale_fill() + 
    geom_sf(data = var[[4]], aes(fill = value), size = 0) + 
    colorspace::scale_fill_continuous_sequential('Reds', rev=F) + 
    ggnewscale::new_scale_fill() + 
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    xlim(-8, 2) +
    ylim(50.4, 58.4) + 
    labs(fill = legend_title) + 
    theme_void() + 
    plot_default_theme + 
    theme(legend.position = 'none') +
    ggtitle(title) + 
    theme(plot.margin = margin(r = 0.1, unit = 'in'))
  
  return(p)
  
}

plot_census_var_single <- function(var, tiles, legend_title = NULL, title = NULL, log = FALSE){
  
  var <- tiles %>% 
    left_join(var, by = c('quadkey' = 'quadkey_12')) %>% 
    drop_na(value)
  
  if (log){
    var$value = log(var$value, 10)
  }
  
  p <- ggplot() + 
    geom_sf(data = world, size = 0.1, colour = 'black', fill = '#EBEBEB') + 
    geom_sf(data = var, aes(fill = value), size = 0) + 
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    xlim(-8, 2) +
    ylim(50.4, 58.4) + 
    labs(fill = legend_title) + 
    theme_void() + 
    plot_default_theme + 
    ggtitle(title) + 
    theme(plot.margin = margin(r = 0.1, unit = 'in'))
  
  return(p)
  
}

p_imd <- plot_census_var(qk_imd %>% rename(value = wm_imd_rank), tiles, title = 'Socioeconomic Deprivation Rank')

p_dens <- plot_census_var_single(qk_dens %>% rename(value = pop_dens), tiles, title = expression(Population~Density~(log[10])), log = T) + 
  colorspace::scale_fill_continuous_sequential('Mint', rev=T)  

p_eth <- plot_census_var_single(qk_eth %>% rename(value = wm_perc_white), tiles, title = '% Ethnic Minority') + 
  colorspace::scale_fill_continuous_sequential('BuGn', rev=T)

p_age <- plot_census_var_single(qk_age %>% rename(value = wm_age), tiles, title = 'Mean Age') + 
  colorspace::scale_fill_continuous_sequential('Blues', rev=T)

p1 <- cowplot::plot_grid(p_imd, p_dens, ncol = 2, rel_widths = c(0.5, 0.5))
p2 <- cowplot::plot_grid(p_eth, p_age, ncol = 2, rel_widths = c(0.5, 0.5))
p <- cowplot::plot_grid(p1, p2, ncol = 1)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/census_map.png', p,
       width = 8.5, height = 11,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/census_map.pdf', p,
       width = 8.5, height = 6,
       units = 'in')


