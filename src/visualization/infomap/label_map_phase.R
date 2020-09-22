suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(igraph)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

london <- c('City of London', 'Barking and Dagenham', 'Barnet', 'Bexley', 'Brent', 'Bromley', 'Camden', 'Croydon', 'Ealing', 'Enfield', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow', 'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Lambeth', 'Lewisham', 'Merton', 'Newham', 'Redbridge', 'Richmond upon Thames', 'Southwark', 'Sutton', 'Tower Hamlets', 'Waltham Forest', 'Wandsworth', 'Westminster')

mob <- read_csv(.args[1]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0")) %>% 
  group_by(date) %>% 
  group_split()

label_map <- read_csv(.args[2]) %>% 
  filter(date %in% c(sort(unique(date)))[1:5]) %>% 
  group_by(date) %>% 
  group_split()

a3 <-  read_csv(.args[5]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

cluster_network <- function(mob, label_map){
  
  mob <- mob %>% 
    filter(start_quadkey != end_quadkey) %>% 
    left_join(label_map %>% dplyr::select(quadkey, cluster), by = c('start_quadkey' = 'quadkey')) %>% 
    rename(start_cluster = cluster) %>% 
    left_join(label_map %>% dplyr::select(quadkey, cluster), by = c('end_quadkey' = 'quadkey')) %>% 
    rename(end_cluster = cluster) %>% 
    group_by(start_cluster, end_cluster) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>% 
    drop_na(start_cluster, end_cluster)
  
  return(mob)
  
}

c_net <- cluster_network(mob[[1]], label_map[[1]])

c_net <- igraph::graph_from_data_frame(c_net, directed = T)

sum(c_net)

get_y_vals(c_net)

get_y_vals <- function(graph, default.dist = NULL){
  
  if (is.null(default.dist)){
    default.dist=sqrt(length(V(graph)))
  }
  
  raw <- as.matrix(as_adjacency_matrix(graph))
  
  raw[raw==0]<-t(raw)[raw==0]
  
  raw <- pmin(raw)
  
  dg <- sna::geodist(raw, inf.replace=default.dist)$gdist
  
  dg <- structure(sapply(dg, jitter, amount = 0.001), dim=dim(dg))
  
  print(dg)
  
  y_coords <- MASS::sammon(dg)$points
  
  return(y_coords[,1])
  
}


coords <- list()
for (i in 1:5){
  
  geo_quadkeys <- label_map[[i]] %>% left_join(a3, by = c('quadkey')) %>% drop_na(NAME_3) %>% filter(NAME_2 %in% c('Leicester', 'Leicestershire')) %>% pull(quadkey)
  
  c_net <- cluster_network(mob[[i]] %>% filter(start_quadkey %in% geo_quadkeys & end_quadkey %in% geo_quadkeys), 
                           label_map[[i]] %>% filter(quadkey %in% geo_quadkeys))
  
  size_ref <- label_map[[i]] %>% group_by(cluster) %>% summarise(size = n())
  
  c_net <- igraph::graph_from_data_frame(c_net, directed = T)
  c_adj <- as.matrix(as_adjacency_matrix(c_net))
  
  clusters <- rownames(c_adj)
  
  coords[[i]] <- data.frame(x = rep(mob[[i]] %>% pull(date) %>% unique(), length(clusters)), y = get_y_vals(c_net, default.dist = 6), cluster = clusters) %>% mutate(group = row_number())
  
  coords[[i]] <- coords[[i]] %>% 
    left_join(size_ref, by = c('cluster'))
  
  print(i)
}

coords[[1]]

line_df <- list()
for (i in 1:length(coords)){
  if (i > 1){
    line_df[[i]] <- rbind(coords[[i]], coords[[i - 1]]) %>% mutate(line = i, group_line = paste(group, line, sep = '_'))
  }
}

line_df <- do.call(rbind, line_df)

line_df %>% 
  filter(size > 10) %>% 
  ggplot() + 
  geom_line(aes(x = x, y = y, group = group_line, color = as.character(group), size = size)) + 
  scale_size(range = c(0.1, 3)) + 
  theme_bw() + 
  theme(legend.position = 'none') 


#a different test that didnt work

im_full <- do.call(rbind, im) 

geo_clust <- im_full %>% 
  left_join(a3, by = c('quadkey')) %>% 
  filter(NAME_2 %in% c('Leicester', 'Leicestershire')) %>% 
  filter(date %in% unique(date)[1:5]) %>% 
  group_by(date, cluster) %>% 
  summarise(size = length(flow), 
            flow = sum(flow),
            duration = length(date)) 

period <- geo_clust$date %>% unique() %>% sort()

geo_clust <- geo_clust %>% group_by(cluster) %>% group_split()

splines <- list()
for (i in 1:length(geo_clust)){
  
  SplineFun <- splinefun(x = as.numeric(geo_clust[[i]]$date), y = geo_clust[[i]]$flow, 
                         method = 'natural')
  dates_n <- seq(as.numeric(min(geo_clust[[i]]$date)), as.numeric(max(geo_clust[[i]]$date)), by = 0.01)
  SplineFit <- SplineFun(dates_n)
  
  splines[[i]] <- data.frame(x = dates_n, y = SplineFit)
  splines[[i]]$cluster = unique(geo_clust[[i]]$cluster)
  splines[[i]]$min_date = min(geo_clust[[i]]$date)
  splines[[i]]$flow = sum(geo_clust[[i]]$flow)
  splines[[i]]$size = sum(geo_clust[[i]]$size)
}

do.call(rbind, splines) %>% 
  mutate(y = y - min(y)) %>% 
  mutate(y = log(y)) %>%
  ggplot() + 
  geom_path(aes(x = x, 
                y = y, 
                color = as.character(cluster), 
                group = cluster, 
                size = size), alpha = 0.7) + 
  scale_size(range = c(2, 10)) + 
  ylim(-15, 0) + 
  theme_void() +
  theme(legend.position = 'none') + 
  plot_default_theme



geo_clust %>% 
  ggplot() + 
  geom_path(aes(x = date, y = log(flow, 10), color = cluster, group = cluster, size = size)) + 
  scale_size(range = c(2, 5)) + 
  theme_void() + 
  theme(legend.position = 'none') + 
  ylim(-7, 0)

geo_clust
