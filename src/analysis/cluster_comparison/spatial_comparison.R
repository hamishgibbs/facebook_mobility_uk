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

spatial_compare(im[[1]], lei[[1]])

sp_compare <- mapply(spatial_compare, im[1:2], lei[1:2])

sp_compare <- list()
for (i in 1:20){
  print(i)
  sp_compare[[i]] <- spatial_compare(im[[i]], lei[[i]])
}

#may be better to do this based on cluster membership? because they are tiles - the 

spatial_compare <- function(smaller_comms, larger_comms){
  
  im_shp <- tiles %>% left_join(smaller_comms, by = c('quadkey')) %>% 
    drop_na(cluster) %>% 
    group_by(date, cluster) %>% 
    summarise(.groups = 'drop') %>% 
    st_transform(27700) %>% 
    st_as_sf()
  
  lei_shp <- tiles %>% left_join(larger_comms, by = c('quadkey')) %>% 
    drop_na(cluster) %>% 
    group_by(date, cluster) %>% 
    summarise(.groups = 'drop') %>% 
    st_transform(27700) %>% 
    st_as_sf()
  
  lapply(lei_shp$geometry, st_geometry_type) %>% unlist %>% unique() %>% as.character()
  lapply(im_shp$geometry, st_geometry_type) %>% unlist %>% unique() %>% as.character()
  
  
  intersections <- list()
  for (i in 1:length(im_shp$geometry)){
    
    intersections[[im_shp[i,]$cluster]] <- suppressWarnings({ st_intersection(im_shp[i,], lei_shp) })
    intersections[[im_shp[i,]$cluster]]$total_area <- as.numeric(st_area(im_shp[i,]))
    
    intersections[[im_shp[i,]$cluster]] <- intersections[[im_shp[i,]$cluster]] %>% 
      mutate(intersect_area = as.numeric(st_area(geometry)))
  }
  
  
  intersections <- do.call(rbind, intersections)
  
  #intersections <- intersections %>% 
  #  filter(st_geometry_type(geometry) %in% c('POLYGON', 'MULTIPOLYGON', 'GEOMETRYCOLLECTION'))
  
  intersections <- intersections %>% 
    mutate(intersect_prop = intersect_area / total_area) 
  
  print(setdiff(im[[1]] %>% pull(cluster) %>% unique(), intersections %>% pull(cluster) %>% unique()))
  
  #must be 0
  if(length(setdiff(im[[1]] %>% pull(cluster) %>% unique(), intersections %>% pull(cluster) %>% unique())) > 0){stop('Some clusters are missing in the intersection')}
  
  small_intersect <- intersections %>% 
    group_by(cluster) %>%
    summarise(max_prop = max(intersect_prop),
              intersects_n_modules = n())
  
  large_intersect <- intersections %>% 
    group_by(cluster.1) %>% 
    summarise(n_intersecting_modules = n())
  
  return(list('small_intersect' = small_intersect,
       'large_intersect' = large_intersect))
}

tiles %>% 
  left_join(im[[1]]) %>% 
  drop_na(cluster) %>% 
  filter(cluster %in% setdiff(im[[1]] %>% pull(cluster) %>% unique(), intersections %>% pull(cluster) %>% unique())) %>% 
  group_by(cluster) %>% 
  summarise() %>% 
  ggplot() + 
  geom_sf(data = uk) + 
  geom_sf(fill = 'red')

intersections %>% 
  ggplot() +
  geom_density(aes(x = intersect_prop))

nmi <- c()
dates <- c()
for (i in 1:length(im)){
  
  a <- im[[i]] %>% 
    arrange(quadkey)
  
  b <- lei[[i]] %>% 
    arrange(quadkey)
  
  testthat::expect_true(identical(a$quadkey, b$quadkey))
  
  nmi <- append(nmi, igraph::compare(as.integer(factor(a$cluster)), as.integer(factor(b$cluster)), method = 'nmi'))
  dates <- append(dates, unique(a$date))
  
}

ggplot() + 
  geom_path(aes(x = dates, y = nmi)) + 
  theme_bw() + 
  plot_default_theme

