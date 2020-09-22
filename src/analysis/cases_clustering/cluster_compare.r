suppressPackageStartupMessages({
  require(RColorBrewer)
  require(tidyverse)
  require(sf)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full_norm_months.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/leiden_full_norm_months.csv',
              '/Users/hamishgibbs/Downloads/scan_results_marjun_nooverlap',
              '/Users/hamishgibbs/Downloads/Middle_Layer_Super_Output_Areas__December_2011__Boundaries-shp/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

#combine mob by month and repeat clustering
im <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  group_by(date) %>% 
  group_split()

im <- im[1:4]

lei <- read_csv(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  rename(date = month) %>%
  group_by(date) %>% 
  group_split()

lei <- lei[1:4]

cases_clust <- list.files(.args[3], pattern = '.shp', full.names = T)
cases_clust <- list('March' = cases_clust[3], 'April' = cases_clust[1], 'May' = cases_clust[4], 'June' = cases_clust[2])

msoa <- st_read(.args[4]) %>% 
  st_simplify(., dTolerance = 300)

a3 <- read_csv(.args[5])

tiles <- st_read(.args[6], quiet = T) %>% 
  st_set_crs(4326)

#make_comparison plots of 3 shapes - msoa, im, lei X
#compute comparison metrics for comparing deaths with both lei and im (different files) existing functions should work X
#sbm if possible 
process_scan <- function(scan_fn){
  
  scan <- st_read(scan_fn)
  
  scan <- st_transform(scan, crs = st_crs(msoa)) %>%
    filter(P_VALUE < 0.1)
  
  intersect <- st_intersection(st_centroid(msoa), scan)
  
  msoa_clust <- msoa %>%
    full_join(st_drop_geometry(intersect)) %>%
    mutate(CLUSTER = as.factor(CLUSTER)) %>%
    group_by(CLUSTER) %>%
    summarise(REL_RISK = unique(REL_RISK)) %>%
    ungroup() %>% 
    st_simplify(., dTolerance = 300) %>% 
    mutate(geometry = st_make_valid(geometry)) %>% 
    drop_na() %>% 
    st_as_sf()
  
  return(msoa_clust)
}

# Identify which LTLAs from shape fall within cluster circles

plot_comparison_data <- function(msoa_clust, im, tiles = tiles){
  
  im_data <- tiles %>% 
    left_join(im, by = c('quadkey')) %>% 
    drop_na(cluster) %>% 
    mutate(geometry = st_make_valid(geometry))
  
  msoa_clust <- msoa_clust %>% 
    st_transform(4326) %>% 
    mutate(geometry = st_make_valid(geometry))
  
  testthat::expect_equal(st_crs(msoa_clust), st_crs(im_data))
  
  im_intersection <- st_intersection(msoa_clust, im_data)
  
  im_clusters <- im %>% filter(cluster %in% unique(im_intersection$cluster))
  
  im_clusters <- tiles %>% left_join(im_clusters, by = c('quadkey')) %>% 
    drop_na(cluster) %>%
    group_by(cluster) %>% 
    summarise() %>% 
    mutate(type = 'Infomap') %>% 
    select(geometry, type)
  
  msoa_clust <- msoa_clust %>% 
    mutate(type = 'Death Cluster') %>% 
    select(geometry, type)
  
  p_data <- rbind(im_clusters, msoa_clust)
  
  p_data <- nngeo::st_remove_holes(p_data)
  
  return(p_data)
  
}

plot_comparison <- function(x, title){
  
  bbox <- st_bbox(x)
  
  p <- x %>% 
    ggplot() + 
    geom_sf(data = uk, fill = 'transparent', size = 0.05, color = 'black') + 
    geom_sf(aes(color = type), fill = 'transparent', size = 0.5) + 
    scale_color_manual(values = c('Death Cluster' = 'red', 'Infomap' = 'blue', 'Leiden' = 'darkgreen')) + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) + 
    theme_void() + 
    plot_default_theme + 
    theme(legend.position = 'none',
          legend.title = element_blank()) + 
    ggtitle(title)
  
  return(p)
}


p_list <- list()
for (i in 1:length(cases_clust)){
  scan <- process_scan(cases_clust[[i]])
  
  p_data_im <- plot_comparison_data(scan, im[[i]], tiles)
  p_data_lei <- plot_comparison_data(scan, lei[[i]], tiles) %>% 
    mutate(type = ifelse(type == 'Infomap', 'Leiden', type))
  
  p_im <- plot_comparison(p_data_im, paste0('Infomap ', names(cases_clust)[i]))
  p_lei <- plot_comparison(p_data_lei, paste0('Leiden ', names(cases_clust)[i]))
  
  p_list[[paste0('Infomap', names(cases_clust)[i])]] <- p_im
  p_list[[paste0('Leiden', names(cases_clust)[i])]] <- p_lei
  
}

p <- cowplot::plot_grid(plotlist = p_list, nrow = 2)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/death_cluster_compare_map.png', p,
       width = 16, height = 10,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/death_cluster_compare_map.pdf', p,
       width = 16, height = 10,
       units = 'in',
       useDingbats = F)

#cluster comparison metrics

scan <- lapply(cases_clust, process_scan)

compare_cluster_metrics <- function(scan, comms){
  scan <- scan %>% 
    group_by(CLUSTER) %>% 
    group_split()
  
  comms <- tiles %>% left_join(comms, by = c('quadkey')) %>% drop_na(cluster) %>% st_as_sf() %>% st_transform(27700)
  
  comms <- comms %>% 
    group_by(cluster) %>% 
    summarise()
  
  results <- list()
  for (i in 1:length(scan)){
    s_clust <- scan[[i]]
    c_intersection <- st_intersection(s_clust, comms)
    
    c_intersection$tot_area <- as.numeric(st_area(s_clust))
    
    c_intersection <- c_intersection %>% 
      mutate(int_area = as.numeric(st_area(geometry)),
             overlap_perc = int_area / tot_area)
    
    results[[i]] <- list('n_overlap' = length(c_intersection$overlap_perc), 
                         'max_overlap_perc' = max(c_intersection$overlap_perc, na.rm = T),
                         'mean_overlap_perc' = mean(c_intersection$overlap_perc, na.rm = T))
                         
  }
  
  return(results)
  
}

im_results <- list()
lei_results <- list()
for (i in 1:length(im)){

  im_results[[names(cases_clust)[i]]] <- compare_cluster_metrics(scan[[i]], im[[i]])
  lei_results[[names(cases_clust)[i]]] <- compare_cluster_metrics(scan[[i]], lei[[i]])
  
}

plot_comparison_metrics <- function(results, title){
  
  df <- do.call(rbind, lapply(results, as.data.frame)) %>% 
    mutate(id = row_number())
  
  p_max <- plot_metric(df, 'max_overlap_perc', 'Max % intersection')
  p_mean <- plot_metric(df, 'mean_overlap_perc', 'Mean % intersection')
  p_overlap <- plot_metric(df, 'n_overlap', 'Number of intersecting modules')
  
  title <- cowplot::ggdraw() + 
    cowplot::draw_label(title,fontface = 'bold',x = 0,hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  p <- cowplot::plot_grid(p_max, p_mean, p_overlap, nrow = 1)
  p <- cowplot::plot_grid(title, p, rel_heights = c(0.1, 0.9), nrow = 2)
  
  return(p)

}

plot_metric <- function(data, attr, y){
  
  data <- data %>% arrange(-!! sym(attr))
  
  data$id <- factor(data$id, levels = data$id)
  
  p <- data %>% 
    ggplot() + 
    geom_point(aes(x = id, y = !! sym(attr))) + 
    theme_bw() + 
    plot_default_theme + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + 
    ylab(y) + 
    xlab('Cluster')
  
  return(p)
  
}

plist <- list()
plist[[1]] <- plot_comparison_metrics(im_results[['March']], 'March - Infomap')
plist[[2]] <- plot_comparison_metrics(im_results[['April']], 'April - Infomap')
plist[[3]] <- plot_comparison_metrics(im_results[['May']], 'May - Infomap')
plist[[4]] <- plot_comparison_metrics(im_results[['June']], 'June - Infomap')

plist[[5]] <- plot_comparison_metrics(lei_results[['March']], 'March - Leiden')
plist[[6]] <- plot_comparison_metrics(lei_results[['April']], 'April - Leiden')
plist[[7]] <- plot_comparison_metrics(lei_results[['May']], 'May - Leiden')
plist[[8]] <- plot_comparison_metrics(lei_results[['June']], 'June - Leiden')

p <- cowplot::plot_grid(plist[[1]], plist[[5]], plist[[2]], plist[[6]], plist[[3]], plist[[7]], plist[[4]], plist[[8]], nrow = 8)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/death_cluster_metrics.png', p,
       width = 18, height = 16,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/death_cluster_metrics.pdf', p,
       width = 18, height = 16,
       units = 'in',
       useDingbats = F)

############################################################################
p_data_im <- plot_comparison_data(msoa_clust, im[[1]], tiles)
p_data_lei <- plot_comparison_data(msoa_clust, lei[[1]], tiles) %>% 
  mutate(type = ifelse(type == 'Infomap', 'Leiden', type))

p_data_im <- nngeo::st_remove_holes(p_data_im)
p_data_lei <- nngeo::st_remove_holes(p_data_lei)

plot_comparison(p_data_im, 'Infomap')
plot_comparison(p_data_lei, 'Leiden')

bbox <- st_bbox(p_data_lei)
p2 <- p_data_lei %>% 
  ggplot() + 
  geom_sf(data = uk, fill = 'transparent', size = 0.05, color = 'black') + 
  geom_sf(aes(color = type), fill = 'transparent', size = 0.5) + 
  scale_color_manual(values = c('Death Cluster' = 'red', 'Infomap' = 'blue', 'Leiden' = 'darkgreen')) + 
  ylim(bbox$ymin, bbox$ymax) + 
  xlim(bbox$xmin, bbox$xmax) + 
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_blank())

p <- cowplot::plot_grid(p1, p2)
p

ggplot() + 
  geom_sf(data = msoa_clust, aes(fill = CLUSTER), size = 0) + 
  geom_sf(data = uk, fill = 'transparent', size = 0.1, color = 'black') + 
  theme_void() + 
  plot_default_theme + 
  theme(legend.position = 'none')

tile_intersection <- as.data.frame(st_intersection(tiles, msoa_clust %>% drop_na() %>% st_as_sf() %>% st_transform(4326)))
tiles %>% left_join(tile_intersection, by = c('quadkey'))  %>% 
  drop_na(CLUSTER) %>% 
  ggplot() + 
  geom_sf(data = uk, fill = 'transparent', size = 0.1, color = 'black') + 
  geom_sf(aes(fill = CLUSTER), size = 0) + 
  theme_void() + 
  plot_default_theme + 
  theme(legend.position = 'none')


nmi <- c()
dates <- c()
for (i in lei){
  
  c_compare <- tile_intersection %>% 
    group_by(quadkey) %>% 
    summarise(CLUSTER = unique(CLUSTER)[1]) %>% 
    left_join(i, by = c('quadkey')) %>% 
    drop_na(CLUSTER, cluster)
  
  nmi <- append(nmi, igraph::compare(c_compare$CLUSTER, as.integer(factor(c_compare$cluster)), method = 'nmi'))
  dates <- append(dates, unique(i$date))
  
  if (unique(i$date) == 5 & nmi[length(nmi)] == max_nmi){
    stop()
  }
  
}
df = data.frame(x = 1:length(nmi), nmi = nmi, month = dates)

df = df %>% 
  filter(month == 5)

max_nmi = df %>% pull(nmi) %>% max()

df %>% ggplot() + geom_path(aes(x = x, y = nmi, color = as.character(month))) + theme_bw() + plot_default_theme

p1 <- tiles %>% left_join(c_compare) %>% 
  drop_na(CLUSTER) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = CLUSTER), size = 0) + 
  theme(legend.position = 'none')

p2 <- tiles %>% left_join(c_compare) %>% 
  drop_na(cluster) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = as.character(cluster)), size = 0) + 
  theme(legend.position = 'none')

cowplot::plot_grid(p1, p2)

#repeat for im


