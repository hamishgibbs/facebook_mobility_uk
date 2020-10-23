suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(igraph)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/journey_lines/journey_lines.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/betweeness.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

lines <- st_read(.args[3]) %>% 
  st_set_crs(4326)

m_btw <- read_csv(.args[4])

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

btw_dens_data <- m_btw %>% 
  mutate(weekday = lubridate::wday(date, label = T)) %>% 
  group_by(weekday, journey) %>% 
  summarise(betweeness = mean(betweeness, na.rm = T))

p_dens <- btw_dens_data %>% 
  ggplot() + 
  geom_density(aes(x = log(betweeness, 10), group = weekday, color = weekday)) + 
  scale_color_manual(values = c('#e41a1c','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf', '#377eb8')) + 
  theme_bw() + 
  plot_default_theme + 
  theme(text = element_text(size = 12),
        legend.title = element_blank()) + 
  xlab('Betweenness Centrality (log)') + 
  ylab('Density') + 
  ggtitle('c')

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

plot_btw_lines <- function(lines, size_scale, title = NULL, legend.position = NULL){
  
  p <- lines %>% 
    ggplot() + 
    geom_sf(data = world, size = 0.07, colour = 'black', fill = 'transparent') + 
    #geom_sf(data = uk, size = 0.07, colour = 'black', fill = 'transparent') + 
    geom_sf(aes(size = betweeness)) + 
    size_scale + 
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

#p1 <- plot_btw_lines(lines_max, 'Maximum network travel', c(0.8, 0.8))
#p1

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}


m_btw <- m_btw %>% group_by(date) %>% group_split()

journeys <- do.call(rbind, m_btw) %>% pull(journey) %>% unique()

#lines <- st_as_sf(st_sfc(lapply(journeys, journey_line)))
#st_crs(lines) <- 4326
#lines$journey <- journeys
#st_write(lines, '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/journey_lines/journey_lines.shp')


panel_dates <- list('a' = as.Date('2020-03-19'), 
                    'b' = as.Date('2020-04-16'))

p_data <- m_btw[[1]]
testthat::expect_equal(unique(p_data$date), panel_dates[['a']])
lines_max <- lines %>% left_join(p_data, by = c('journey')) %>% drop_na(betweeness)

p_data <- m_btw[[29]]
testthat::expect_equal(unique(p_data$date), panel_dates[['b']])
lines_min <- lines %>% left_join(p_data, by = c('journey')) %>% drop_na(betweeness)

betweeness_tot <- c(lines_max$betweeness, lines_min$betweeness)

size_scale <- scale_size(range = c(0.005, 1), limits = c(min(betweeness_tot), max(betweeness_tot)), labels = scales::comma)

p1 <- plot_btw_lines(lines_max, 'Maximum network travel', size_scale = size_scale, c(0.8, 0.8))
p2 <- plot_btw_lines(lines_min, 'Minimum network travel', size_scale = size_scale, c(0.8, 0.8))

p1 <- cowplot::plot_grid(letter_title('a'), p1, rel_heights = c(0.05, 1), nrow = 2)
p2 <- cowplot::plot_grid(letter_title('b'), p2, rel_heights = c(0.05, 1), nrow = 2)

p_mat <- read_rds('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/matrix_comparison.rds') + 
  theme(text = element_text(size = 12)) + 
  ggtitle('d')

p_map <- cowplot::plot_grid(p1, p2)

p_other <- cowplot::plot_grid(p_dens, p_mat, ncol = 1)

p <- cowplot::plot_grid(p_map, p_other, rel_widths = c(0.65, 0.35))

ggsave(tail(.args, 1), p,
       width = 12, height = 6,
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), p,
       width = 9, height = 6,
       units = 'in',
       useDingbats = F)


# supplemental betweeness figures
p_data <- m_btw[[1]]
testthat::expect_equal(unique(p_data$date), panel_dates[['a']])
lines_max <- lines %>% left_join(p_data, by = c('journey')) %>% drop_na(betweeness)

size_scale <- scale_size(range = c(0.005, 1), limits = c(min(p_data$betweeness), max(p_data$betweeness)), labels = scales::comma)

p <- plot_btw_lines(lines_max, size_scale = size_scale, legend.position = c(0.8, 0.8))

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_large_supp.png', p,
       width = 8.5, height = 11,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_large_supp.pdf', p,
       width = 9, height = 6,
       units = 'in',
       useDingbats = F)

panel_dates <- list('start' = as.Date('2020-04-13'), 
                    'end' = as.Date('2020-04-19'))

m_btw <- do.call(rbind, m_btw)

m_btw <- m_btw %>% 
  filter(date >= panel_dates['start'],
         date <= panel_dates['end']) %>% 
  group_by(date) %>% 
  group_split()

testthat::expect_equal(length(m_btw), 7)

p_lines <- lapply(m_btw, function(x){lines %>% left_join(x, by = c('journey')) %>% drop_na(betweeness)})

p_lines_comb <- do.call(rbind, p_lines)

size_scale <- scale_size(range = c(0.005, 1), limits = c(min(p_lines_comb$betweeness), max(p_lines_comb$betweeness)), labels = scales::comma)

wday <- lapply(p_lines, function(x){weekdays(unique(x$date))}) %>% unlist()

p <- list()

for (i in 1:length(p_lines)){
  p[[i]] <- plot_btw_lines(p_lines[[i]], size_scale = size_scale, title = wday[i], legend.position = 'none')
}


legend <- cowplot::get_legend(plot_btw_lines(p_lines[[1]], size_scale = size_scale))

p[[8]] <- legend

p <- cowplot::plot_grid(plotlist = p, nrow = 2)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_week_supp.png', p,
       width = 9, height = 6,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/btw_week_supp.pdf', p,
       width = 9, height = 6,
       units = 'in',
       useDingbats = F)
