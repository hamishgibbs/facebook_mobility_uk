#script to compare overlap of communities from different algorithms
#using node membership - not spatial extent

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
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_descriptive/lei_im_s_to_l.csv')
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

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

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

write_csv(results_s_to_l_df, tail(.args, 1))
write_csv(results_l_to_s_df, gsub('lei_im_s_to_l.csv', 'lei_im_l_to_s.csv', tail(.args, 1)))

