#install blockmodels BM_gaussian or other

#Compare with "nmi" in igraph::compare
#aggregate into 2 week periods?


require(blockmodels)

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character()))

mob <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(journey, week) %>% 
  summarise(start_quadkey = unique(start_quadkey),
            end_quadkey = unique(end_quadkey),
            n_crisis = sum(n_crisis, na.rm = T)) %>%
  group_by(week) %>% 
  group_split()

m <- mob[[1]] %>%
  rename(start = start_quadkey, end = end_quadkey, weight = n_crisis) %>% 
  select(start, end, weight)

g <- igraph::graph_from_data_frame(m, directed = TRUE, vertices = NULL)

g_adj <- igraph::as_adjacency_matrix(g, attr = 'weight')

g_adj <- as.matrix(g_adj)

g_adj <- g_adj / sum(g_adj)

r <- BM_poisson('SBM', g_adj)

r$explore_min = 50
r$explore_max = 50

r

r$estimate()


extract_membership <- function(Z, m){
  cluster = c()
  
  for (i in 1:length(Z[, 1])){
    cluster = append(cluster, which(Z[i, ] == max(Z[i, ])))
  }
  
  clust <- data.frame(quadkey = row.names(m), cluster = cluster)
  
  return(clust)
}


clust <- extract_membership(r$memberships[[17]]$Z, as.matrix(g_adj))

pal <- qualitative_pal(unique(clust$cluster))

tiles %>% 
  left_join(clust) %>% 
  drop_na(cluster) %>% 
  ggplot() + 
  geom_sf(aes(fill = as.character(cluster)), size = 0) + 
  scale_fill_manual(values = pal) + 
  theme(legend.position = 'none')
  



