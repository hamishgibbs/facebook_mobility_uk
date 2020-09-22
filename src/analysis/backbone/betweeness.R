suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(igraph)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Downloads/data_2020-Sep-10.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

mob <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  group_by(date) %>% 
  group_split()

compute_betweeness <- function(mob_date){
  
  g <- igraph::graph_from_data_frame(mob_date %>% select(start_quadkey, end_quadkey, n_crisis))
  
  btw <- edge_betweenness(g)

  mob_date$betweeness = btw
  
  return(mob_date)
}

m_btw <- lapply(mob, compute_betweeness)

m_btw <- do.call(rbind, m_btw)

btw_dens_data <- m_btw %>% 
  mutate(weekday = lubridate::wday(date, label = T)) %>% 
  group_by(weekday, journey) %>% 
  summarise(betweeness = mean(betweeness, na.rm = T))

p_dens <- btw_dens_data %>% 
  ggplot() + 
  geom_density(aes(x = log(betweeness, 10), group = weekday, color = weekday)) + 
  scale_color_manual(values = c('#e31a1c', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fdbf6f', '#fb9a99')) + 
  theme_bw() + 
  plot_default_theme + 
  theme(text = element_text(size = 12),
        legend.title = element_blank()) + 
  xlab('Betweenness Centrality (log)') + 
  ylab('Density') + 
  ggtitle('c')

btw_dens

#supplemental figure
p_supp = m_btw %>% 
  mutate(weekday = lubridate::wday(date, label = T)) %>%
  ggplot() + 
  geom_density(aes(x = log(betweeness, 10), color = date, group = date), size = 0.3) + 
  facet_grid(~weekday) + 
  theme_bw() + 
  plot_default_theme + 
  xlab('Betweeness (log)') + 
  ylab('Density') + 
  theme(legend.title = element_blank())

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_comparison_supp.png', p_supp,
       width = 12, height = 4,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_comparison_supp.pdf', p_supp,
       width = 9, height = 6,
       units = 'in',
       useDingbats = F)

journey_line <- function(journey){
  
  journey <- str_split(journey, '_')
  
  start_poly <- tiles %>% filter(quadkey == journey[[1]][1])
  end_poly <- tiles %>% filter(quadkey == journey[[1]][2])
  
  line <- suppressWarnings(st_linestring(rbind(st_coordinates(st_geometry(st_centroid(start_poly))), st_coordinates(st_geometry(st_centroid(end_poly))))))
  
  return(line)
}

betweeness_lines <- function(m_btw, method = 'sd_thresh', sd_thresh = 2, q = 0.9){
  
  if (method %in% c('sd_thresh')){
    hb <- m_btw %>% 
      filter(betweeness > (sd(betweeness) * sd_thresh))
  } else if (method %in% 'quantile'){
    hb <- m_btw %>% 
      filter(betweeness > as.numeric(quantile(betweeness, q)))
  } else {
    stop('Unknown method "', method, '"')
  }
  
  
  hb_journeys <- hb %>% pull(journey)
  hb_btw <- hb %>% pull(betweeness)
  
  lines <- st_as_sf(st_sfc(lapply(hb_journeys, journey_line)))
  st_crs(lines) <- 4326
  lines$journey <- hb_journeys
  lines$btw <- hb_btw
  lines$date <- hb %>% pull(date) %>% unique()
  
  return(lines)
}

plot_btw_lines <- function(lines, title = NULL, legend.position = NULL){
  
  p <- lines %>% 
    ggplot() + 
    geom_sf(data = world, size = 0.07, colour = 'black', fill = 'transparent') + 
    #geom_sf(data = uk, size = 0.07, colour = 'black', fill = 'transparent') + 
    geom_sf(aes(size = betweeness)) + 
    scale_size(range = c(0.001, 0.5)) + 
    xlim(-8, 2) + 
    ylim(50.4, 58.4) + 
    theme_void() + 
    plot_default_theme + 
    labs(size = 'Betweenness') + 
    theme(text = element_text(size = 12)) + 
    ggtitle(paste0(lines %>% pull(date) %>% unique(), ' ', title)) + 
    theme(legend.position = legend.position)
  
  return(p)
  
}

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}


m_btw <- m_btw %>% group_by(date) %>% group_split()

1 & 25

journeys <- do.call(rbind, m_btw) %>% pull(journey) %>% unique()

#lines <- st_as_sf(st_sfc(lapply(journeys, journey_line)))
#st_crs(lines) <- 4326
#lines$journey <- journeys
st_write(lines, '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/journey_lines/journey_lines.shp')

lines_max <- lines %>% left_join(m_btw[[1]], by = c('journey')) %>% drop_na(betweeness)
lines_min <- lines %>% left_join(m_btw[[25]], by = c('journey')) %>% drop_na(betweeness)

p1 <- plot_btw_lines(lines_max, 'Maximum network travel', c(0.8, 0.8))
p2 <- plot_btw_lines(lines_min, 'Minimum network travel', c(0.8, 0.8))

p1 <- cowplot::plot_grid(letter_title('a'), p1, rel_heights = c(0.05, 1), nrow = 2)
p2 <- cowplot::plot_grid(letter_title('b'), p2, rel_heights = c(0.05, 1), nrow = 2)

p_mat <- read_rds('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/matrix_comparison.rds') + 
  theme(text = element_text(size = 12)) + 
  ggtitle('d')

p_map <- cowplot::plot_grid(p1, p2)

p_other <- cowplot::plot_grid(p_dens, p_mat, ncol = 1)

p <- cowplot::plot_grid(p_map, p_other, rel_widths = c(0.65, 0.35))


ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_comparison.png', p,
       width = 12, height = 6,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_comparison.pdf', p,
       width = 9, height = 6,
       units = 'in',
       useDingbats = F)



