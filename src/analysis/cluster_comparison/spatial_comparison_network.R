suppressPackageStartupMessages({
  require(RColorBrewer)
  require(tidyverse)
  require(sf)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  group_by(date) %>% 
  group_split()

lei <- read_csv(.args[2]) %>% 
  group_by(date) %>% 
  group_split()

a3 <- read_csv(.args[3])

tiles <- st_read(.args[4], quiet = T) %>% 
  st_set_crs(4326)

uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

a <- im[[1]]

b <- lei[[1]]


compare_clusters_small_to_large <- function(a, b){
  
  n_clust_a <- a %>% pull(cluster) %>% unique() %>% length()
  n_clust_b <- b %>% pull(cluster) %>% unique() %>% length()
  
  if (n_clust_a > n_clust_b){
    larger = b
    smaller = a
  } else {
    larger = a
    smaller = b
  }
  
  #% overlap between smaller and larger
  smaller <- smaller %>% 
    group_by(cluster) %>% 
    group_split()
  
  results <- list()
  for (sm_clust in smaller){
    l_clust <- larger %>% 
      filter(quadkey %in% c(sm_clust$quadkey))
    
    #get percentage of each larger module that intersects smaller cluster
    n_small <- sm_clust$quadkey %>% length()
    
    n_large <- l_clust %>% 
      group_by(cluster) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      mutate(perc_overlap = n / n_small) %>% pull(perc_overlap)
    
    results[[unique(sm_clust$cluster)]] <- n_large
    
  }
  
  perc_overlap <- list()
  n_overlap <- list()
  for (i in 1:length(results)){
    perc_overlap[[names(results[i])]] <- max(results[[names(results[i])]])
    n_overlap[[names(results[i])]] <- length(results[[names(results[i])]])
  }
  
  results_df <- data.frame(cluster = names(results), max_overlap = unlist(perc_overlap), n_overlap = unlist(n_overlap))
  
  return(results_df)
  
}

compare_clusters_large_to_small <- function(a, b){
  
  n_clust_a <- a %>% pull(cluster) %>% unique() %>% length()
  n_clust_b <- b %>% pull(cluster) %>% unique() %>% length()
  
  if (n_clust_a > n_clust_b){
    larger = b
    smaller = a
  } else {
    larger = a
    smaller = b
  }
  
  larger <- larger %>% 
    group_by(cluster) %>% 
    group_split()
  
  results <- list()
  for (lg_clust in larger){
    sm_clust <- smaller %>% 
      filter(quadkey %in% c(lg_clust$quadkey))
    
    #get percentage of each larger module that intersects smaller cluster
    
    n_small_clusters <- sm_clust %>% 
      group_by(cluster) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      pull(cluster) %>% length()
    
    results[[unique(lg_clust$cluster)]] <- n_small_clusters
    
  }
  
  results_df <- tibble(cluster = names(results), n_overlap = unlist(results))
  
  return(results_df)  
}

results_s_to_l <- list()
for( i in 1:length(im)){
  date <- im[[i]]$date %>% unique()
  
  results_df <- compare_clusters_small_to_large(im[[i]], lei[[i]])
  
  results_df$date <- date
  
  results_s_to_l[[i]] <- results_df
  
  print(i)
  
}

results_l_to_s <- list()
for( i in 1:length(im)){
  date <- im[[i]]$date %>% unique()
  
  results_df <- compare_clusters_large_to_small(im[[i]], lei[[i]])
  
  results_df$date <- date
  
  results_l_to_s[[i]] <- results_df
  
  print(i)
  
}

results_s_to_l_df <- as_tibble(do.call(rbind, results_s_to_l))

results_l_to_s_df <- as_tibble(do.call(rbind, results_l_to_s))


pl <- list()
pl[[1]] <- results_s_to_l_df %>% group_by(date) %>% 
  summarise(max_overlap = mean(max_overlap)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = max_overlap)) + 
  ylim(0, 1) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank()) + 
  ylab('Largest % overlap with larger modules (daily mean)') + 
  ggtitle('Small communites to large communities')

pl[[2]] <- results_s_to_l_df %>% group_by(date) %>% 
  summarise(n_overlap = mean(n_overlap)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank()) + 
  ylab('N modules with overlap % > 0 (daily mean)') + 
  ggtitle('Small communites to large communities')

pl[[3]] <- results_l_to_s_df %>% group_by(date) %>% 
  summarise(n_overlap = mean(n_overlap)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank()) + 
  ylab('N modules with overlap % > 0 (daily mean)') + 
  ggtitle('Large communites to small communities')

p1 <- cowplot::plot_grid(plotlist = pl, nrow = 1)

pl2 <- list()

daily_df <- results_s_to_l_df %>% 
  filter(date == min(date)) %>% 
  arrange(max_overlap)

daily_df$cluster <- factor(daily_df$cluster, levels = daily_df$cluster)
pl2[[1]] <- daily_df %>% 
  ggplot() + 
  geom_point(aes(x = cluster, y = max_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.text.x = element_blank()) + 
  xlab('Cluster ID') + 
  ylab('Largest % overlap with larger modules') + 
  ggtitle(paste(unique(daily_df$date), ' - Small communites to large communities'))

daily_df <- results_s_to_l_df %>% 
  filter(date == min(date)) %>% 
  arrange(-n_overlap)

daily_df$cluster <- factor(daily_df$cluster, levels = daily_df$cluster)
pl2[[2]] <- daily_df %>% 
  ggplot() + 
  geom_point(aes(x = cluster, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.text.x = element_blank()) + 
  xlab('Cluster ID') + 
  ylab('N modules with overlap % > 0') + 
  ggtitle(paste(unique(daily_df$date), ' - Small communites to large communities'))

daily_df <- results_l_to_s_df %>% 
  filter(date == min(date)) %>% 
  arrange(n_overlap)

daily_df$cluster <- factor(daily_df$cluster, levels = daily_df$cluster)
pl2[[3]] <- daily_df %>% 
  ggplot() + 
  geom_point(aes(x = cluster, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.text.x = element_blank()) + 
  xlab('Cluster ID') + 
  ylab('N modules with overlap % > 0') + 
  ggtitle(paste(unique(daily_df$date), ' - Large communites to small communities'))

p2 <- cowplot::plot_grid(plotlist = pl2, nrow = 1)

p <- cowplot::plot_grid(p1, p2, nrow = 2)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.png', p,
       width = 8.5, height = 6,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.pdf', p,
       width = 14, height = 6,
       units = 'in', useDingbats = F)


max_ol_clust <- results_l_to_s_df %>% 
  filter(n_overlap == max(n_overlap))

lei_intersect <- do.call(rbind, lei) %>% 
  filter(date == max_ol_clust$date, cluster == max_ol_clust$cluster)

im_intersect <- do.call(rbind, im) %>% 
  filter(date == max_ol_clust$date, quadkey %in% lei_intersect$quadkey)

lei_intersect <- tiles %>% 
  left_join(lei_intersect) %>% 
  drop_na(cluster) %>% 
  mutate(n = 1) %>% 
  group_by(n) %>% 
  summarise()

im_intersect <- tiles %>% 
  left_join(im_intersect) %>% 
  drop_na(cluster)

m <- ggplot() + 
  geom_sf(data = lei_intersect, fill = 'transparent', color = 'blue', size = 1.5) + 
  geom_sf(data = im_intersect, aes(fill = cluster), size = 0) + 
  geom_sf(data = uk, fill = 'transparent', size = 0.1, color = 'black') + 
  ylim(51.9, 53.5) + 
  xlim(-0.7, 1.5) +  
  theme_void() + 
  theme(legend.position = 'none') + 
  ggtitle('Example - Leiden module overlapping most IM modules')

p2 <- cowplot::plot_grid(p, m, ncol = 2, rel_widths = c(0.7, 0.3))

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.png', p2,
       width = 18, height = 6,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.pdf', p2,
       width = 14, height = 6,
       units = 'in', useDingbats = F)
