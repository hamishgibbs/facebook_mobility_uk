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

im <- read_csv(.args[1])

lei <- read_csv(.args[2])

a3 <- read_csv(.args[3])

community_tree <- function(im, title){
  
  clust_size <- im %>% 
    group_by(date, cluster) %>% 
    summarise(n_tiles = n(),
              .groups = 'drop') %>% 
    group_by(date) %>% 
    arrange(n_tiles)
  
  label_size <- clust_size %>% 
    group_by(cluster) %>% 
    summarise(n = n()) %>% 
    arrange(n) %>% 
    pull(cluster)
  
  cluster_names <- clust_size %>% pull(cluster) %>% unique()
  
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), round(clust_size %>% pull(cluster) %>% unique() %>% length() / 74 , 0))
  
  pal <- pal[1:length(cluster_names)]
  names(pal) = cluster_names
  
  p <- clust_size %>% 
    slice(1:100) %>% 
    ggplot() + 
    geom_bar(aes(x=date, y=n_tiles, fill=factor(cluster, levels = label_size)), position="fill", stat="identity") + 
    scale_fill_manual(values = pal) +
    ggtitle(title) + 
    theme_bw() + 
    plot_default_theme + 
    theme(legend.position = 'none',
          axis.title.x = element_blank()) + 
    ylab('Tile proportion')
  
  return(p)
  
}

plot_n_daily <- function(im, ymin = 0, ymax = 200){
  n_daily_clusters(im) %>% 
    ggplot() + 
    geom_bar(aes(x = date, y = n), stat="identity", fill = 'grey') + 
    theme_bw() + 
    plot_default_theme + 
    ylab('Number of communities') + 
    theme(axis.title.x = element_blank()) + 
    ylim(ymin, ymax)
}

p1 <- community_tree(im, paste0('Infomap - ', n_daily_clusters_mean(im), ' average communities daily'))
p2 <- community_tree(lei, paste0('Leiden - ', n_daily_clusters_mean(lei), ' average communities daily'))

p <- cowplot::plot_grid(p1, p2, ncol = 1)

p

n_daily_clusters <- function(im){
  n_daily <- im %>% group_by(date, cluster) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    group_by(date) %>% 
    summarise(n = n(), .groups = 'drop')
  
  return(n_daily)
}

n_daily_clusters_mean <- function(im){
  n_daily <- n_daily_clusters(im) %>% 
    pull(n) %>% mean()
  
  return(round(n_daily, 0))
}

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/cluster_algo_comparison.png', p)

london <- c('City of London', 'Barking and Dagenham', 'Barnet', 'Bexley', 'Brent', 'Bromley', 'Camden', 'Croydon', 'Ealing', 'Enfield', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow', 'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Lambeth', 'Lewisham', 'Merton', 'Newham', 'Redbridge', 'Richmond upon Thames', 'Southwark', 'Sutton', 'Tower Hamlets', 'Waltham Forest', 'Wandsworth', 'Westminster')


im_focus <- im %>% 
  left_join(a3, by = c('quadkey')) %>% 
  filter(NAME_2 %in% c('Leicestershire', 'Leicester')) 
  #filter(NAME_3 %in% london)

lei_focus <- lei %>% 
  left_join(a3, by = c('quadkey')) %>% 
  filter(NAME_2 %in% c('Leicestershire', 'Leicester')) 

p1 <- community_tree(im_focus, 'Infomap -  Leicestershire')
p2 <- community_tree(lei_focus, paste0('Leiden - Leicestershire'))

im_focus <- im %>% 
  left_join(a3, by = c('quadkey')) %>% 
  filter(NAME_3 %in% london)

lei_focus <- lei %>% 
  left_join(a3, by = c('quadkey')) %>% 
  filter(NAME_3 %in% london)

p3 <- community_tree(im_focus, 'Infomap -  London')
p4 <- community_tree(lei_focus, 'Leiden - London')


p <- cowplot::plot_grid(p1, p2, ncol = 1)

