suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  library(RColorBrewer)
})

#DO LINES WITH FLOW & Colors & Size

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_bank_holidays.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_weekends.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_biweek/communities_lei.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_biweek/communities_sbm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/comm_method_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"),
         method = 'InfoMap')

lei <- read_csv(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"),
         method = 'Leiden')

sbm <- read_csv(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"),
         method = 'Stochastic\nBlock\nModel')

tiles <- st_read(.args[4]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

mob <- read_csv(.args[6]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

oa_pop <- read_csv(.args[7]) %>% 
  rename(quadkey = quadkey_12) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')


get_n_clusters_date <- function(comm){
  
  comm <- comm %>% 
    group_by(date) %>% 
    summarise(n_clust = length(unique(cluster)),
              method = unique(method))
  
  return(comm)
  
}


n_clust <- do.call(rbind, lapply(list(im, lei, sbm), get_n_clusters_date))

p_n <- n_clust %>% 
  filter(method == 'InfoMap') %>% 
  ggplot() + 
  plot_weekends(n_clust) + 
  geom_vline(aes(xintercept = as.Date('2020-03-19')), color = 'red') + 
  geom_vline(aes(xintercept = as.Date('2020-04-12')), color = 'red') + 
  geom_vline(aes(xintercept = as.Date('2020-09-09')), color = 'red') + 
  geom_path(aes( x = date, y = n_clust, group = method), size = 0.4) + 
  plot_bank_holidays() + 
  theme_bw() + 
  plot_default_theme + 
  theme(legend.title = element_blank(),
        text = element_text(size = 12),
        axis.title.x = element_blank()) + 
  ylab('Number of Communities') + 
  ggtitle('b') #+ 
  #ylim(0, 304)

tiles %>% 
  mutate(area = st_area(geometry),
         area = units::set_units(area, 'km^2')) 
  pull(area) %>% class()


size_res <- list()
pop_res <- list()

for (i in 1:length(im)){
  size_res[[i]] <- tiles %>% left_join(im[[i]], by = c('quadkey')) %>% 
    group_by(cluster) %>% 
    summarise(size = n(),
              date = unique(date),
              .groups = 'drop') %>% 
    mutate(area = st_area(geometry),
           area = units::set_units(area, 'km^2')) %>% 
    st_drop_geometry()
  
  pop_res[[i]] <- oa_pop %>% 
    left_join(im[[i]], by = c('quadkey')) %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(pop = sum(pop, na.rm = T),
              date = unique(date),
              .groups = 'drop')
  
}

p_pop <- do.call(rbind, pop_res) %>% 
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
  ggtitle('c')

p_area <- do.call(rbind, size_res) %>% 
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
  ggtitle('d')


split_date <- function(comm){
  
  comm <- comm %>% 
    group_by(date) %>% 
    group_split()
  
  return(comm)
  
}

im <- split_date(im)
#lei <- split_date(lei)
#sbm <- split_date(sbm)

shared_communities <- unique(lapply(list(im[[1]], im[[25]], im[[length(im)]]), function(x){return(x %>% pull(cluster) %>% unique())}) %>% unlist())
custom_pal <- qualitative_pal(shared_communities, 100)

p_data <- im[[1]]
title <- paste0(p_data$date %>% unique(), ' - ', p_data$cluster %>% unique() %>% length(), ' communities\nMaxiumum travel')
p1 <- plot_clusters(p_data, uk, title = title, uk_lims = T, custom_pal = custom_pal)

p_data <- im[[25]]
title <- paste0(p_data$date %>% unique(), ' - ', p_data$cluster %>% unique() %>% length(), ' communities\nMinimum travel')
p2 <- plot_clusters(p_data, uk, title = title, uk_lims = T, custom_pal = custom_pal)

p_data <- im[[length(im)]]
title <- paste0(p_data$date %>% unique(), ' - ', p_data$cluster %>% unique() %>% length(), ' communities\nFinal timepoint')
p3 <- plot_clusters(p_data, uk, title = title, uk_lims = T, custom_pal = custom_pal)

#p2 <- plot_clusters(lei[[1]], uk, title = 'b) Leiden', uk_lims = T)

#p3 <- plot_clusters(sbm[[1]], uk, title = 'c) Stochastic Block Model', uk_lims = T)

p_map <- cowplot::plot_grid(p1, p2, p3, nrow = 1)

p_det <- cowplot::plot_grid(p_pop, p_area, ncol = 1)

p <- cowplot::plot_grid(p_map, p_n, nrow = 2, rel_heights = c(0.62, 0.38))

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

titlea <- letter_title('a')

p <- cowplot::plot_grid(titlea, p, rel_heights = c(0.05, 1), nrow = 2)

p <- cowplot::plot_grid(p, p_det, ncol = 2, rel_widths = c(0.6, 0.4))

ggsave(tail(.args, 1), p,
       width = 13, height = 7,
       units = 'in')

ggsave(gsub('png', 'pdf', tail(.args, 1)), p,
       width = 13, height = 7,
       useDingbats = F,
       units = 'in')

g_df <- mob %>% filter(date %in% c(unique(im[[1]]$date))) %>% 
  left_join(im[[1]], by = c('start_quadkey' = 'quadkey')) %>% 
  rename(start_cluster = cluster) %>% 
  select(start_quadkey, end_quadkey, n_crisis, start_cluster) %>% 
  left_join(im[[1]], by = c('end_quadkey' = 'quadkey')) %>% 
  rename(end_cluster = cluster) %>% 
  filter(start_cluster == end_cluster) %>% 
  select(start_quadkey, end_quadkey, n_crisis, start_cluster) %>% 
  group_by(start_cluster) %>% 
  group_split()


res <- list()

for (i in 1:length(g_df)){
  
  if (length(g_df[[i]]$n_crisis) > 2){
    
    g <- igraph::graph_from_data_frame(g_df[[i]])
    
    modul <- igraph::modularity(g, membership = g_df[[i]]$start_cluster, weights = g_df[[i]]$n_crisis)
    
    res[[unique(g_df[[i]]$start_cluster)]] <- data.frame(cluster = i, modularity = modul)
    
  }
  
  
  
}


res <- do.call(rbind, res)

tiles %>% 
  left_join(im[[1]] %>% left_join(res)) %>% 
  drop_na(cluster) %>% 
  ggplot() + 
  geom_sf(aes(fill = modularity), size = 0 )






