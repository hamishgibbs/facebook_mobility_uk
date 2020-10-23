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
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_descriptive/community_size.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_descriptive/community_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/comm_method_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"),
         method = 'InfoMap')

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

mob <- read_csv(.args[3]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

oa_pop <- read_csv(.args[4]) %>% 
  rename(quadkey = quadkey_12) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

size_res <- read_csv(.args[5])
pop_res <- read_csv(.args[6])

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

panel_dates <- list('a' = as.Date('2020-03-19'), 
                    'b' = as.Date('2020-04-16'), 
                    'c' = as.Date('2020-10-15'))

get_n_clusters_date <- function(comm){
  
  comm <- comm %>% 
    group_by(date) %>% 
    summarise(n_clust = length(unique(cluster)),
              method = unique(method))
  
  return(comm)
  
}

n_clust <- do.call(rbind, lapply(list(im), get_n_clusters_date))

pts <- data.frame(plot_date = c(panel_dates[['a']], panel_dates[['b']], panel_dates[['c']]),
           value = c(n_clust %>% filter(date == panel_dates[['a']]) %>% pull(n_clust),
                     n_clust %>% filter(date == panel_dates[['b']]) %>% pull(n_clust),
                     n_clust %>% filter(date == panel_dates[['c']]) %>% pull(n_clust)))

p_n <- n_clust %>% 
  filter(method == 'InfoMap') %>% 
  ggplot() + 
  plot_weekends(n_clust) + 
  geom_path(aes( x = date, y = n_clust, group = method), size = 0.4) + 
  plot_bank_holidays() + 
  theme_bw() + 
  plot_default_theme + 
  geom_point(data = pts, aes(x = plot_date, y = value), color = 'red', size = 2) + 
  theme(legend.title = element_blank(),
        text = element_text(size = 12),
        axis.title.x = element_blank()) + 
        #axis.ticks.length.y = unit(-1.4, "mm"),
        #axis.text.y = element_text(margin = unit(c(t = 2.5, r = -6, b = 0, l = 0), "mm")),
        #axis.title.y = element_text(margin = unit(c(t = 0, r = 3, b = 0, l = 0), "mm"))) + 
  xlim(as.Date('2020-03-01'), max(n_clust$date)) + 
  ylim(180, 300) + 
  ylab('Number of Communities') + 
  ggtitle('b')

im <- im %>% 
  group_by(date) %>% 
  group_split()

p_pop <- pop_res %>% 
  group_by(date) %>% 
  summarise(pop = median(pop, na.rm = T)) %>% 
  drop_na(date) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = pop)) + 
  ylab('Median population size') + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) + 
  ggtitle('a')

p_area <- size_res %>% 
  group_by(date) %>% 
  summarise(area = as.numeric(median(area, na.rm = T))) %>% 
  drop_na(date) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = area)) +
  ylab('Median area (km ^ 2)') + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) + 
  ggtitle('b')

shared_communities <- unique(lapply(list(im[[1]], im[[29]], im[[length(im) - 2]]), function(x){return(x %>% pull(cluster) %>% unique())}) %>% unlist())
custom_pal <- qualitative_pal(shared_communities, 100)

p_data <- im[[1]]
testthat::expect_equal(unique(p_data$date), panel_dates[['a']])
title <- paste0(p_data$date %>% unique(), ' - ', p_data$cluster %>% unique() %>% length(), ' communities\nPre-lockdown')
p1 <- plot_clusters(p_data, uk, title = title, uk_lims = T, custom_pal = custom_pal)

p_data <- im[[29]]
testthat::expect_equal(unique(p_data$date), panel_dates[['b']])
title <- paste0(p_data$date %>% unique(), ' - ', p_data$cluster %>% unique() %>% length(), ' communities\nLockdown')
p2 <- plot_clusters(p_data, uk, title = title, uk_lims = T, custom_pal = custom_pal)

p_data <- im[[length(im) - 2]]
testthat::expect_equal(unique(p_data$date), panel_dates[['c']])
title <- paste0(p_data$date %>% unique(), ' - ', p_data$cluster %>% unique() %>% length(), ' communities\nFinal')
p3 <- plot_clusters(p_data, uk, title = title, uk_lims = T, custom_pal = custom_pal)

p_map <- cowplot::plot_grid(p1, p2, p3, nrow = 1)

p <- cowplot::plot_grid(p_map, p_n, nrow = 2, rel_heights = c(0.62, 0.38))

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

titlea <- letter_title('a')

p <- cowplot::plot_grid(titlea, p, rel_heights = c(0.05, 1), nrow = 2)

#p <- cowplot::plot_grid(p, p_det, ncol = 2, rel_widths = c(0.6, 0.4))

ggsave(tail(.args, 1), p,
       width = 8.5, height = 7,
       units = 'in')

ggsave(gsub('png', 'pdf', tail(.args, 1)), p,
       width = 8.5, height = 7,
       useDingbats = F,
       units = 'in')

p_det <- cowplot::plot_grid(p_pop, p_area, ncol = 2)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/infomap_comm_detail.png', p_det,
       width = 8.5, height = 4,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/infomap_comm_detail.pdf', p_det,
       width = 8.5, height = 4,
       useDingbats = F,
       units = 'in')
