suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])
label_map_lei <- read_csv(.args[2])

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

#most persistent cluster
get_cluster_persistence <- function(label_map){
  persistence <- label_map %>% 
    group_by(cluster, date) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    group_by(cluster) %>% 
    summarise(n_days_present = n(), .groups = 'drop') %>% 
    arrange(-n_days_present)
  
  return(persistence)
}

persistence <- get_cluster_persistence(label_map)
persistence_lei <- get_cluster_persistence(label_map_lei)

persistence %>% 
  ggplot() + 
  geom_density(aes(x = n_days_present)) + 
  geom_density(data = persistence_lei, aes(x = n_days_present), color = 'red')

persistence %>% 
  ggplot() + 
  geom_density(aes(x = log(n_days_present, 10))) + 
  geom_density(data = persistence_lei, aes(x = log(n_days_present, 10)), color = 'red')

longest_lasting <- persistence %>% 
  filter(n_days_present == max(n_days_present)) %>% 
  pull(cluster)

longest_lasting_lei <- persistence_lei %>% 
  filter(n_days_present == max(n_days_present)) %>% 
  pull(cluster)

cluster_date_data <- label_map %>% filter(cluster == longest_lasting[1]) %>% 
  group_by(date) %>% 
  group_split()

geography_intersection <- function(label_map, geo_names, level = 'NAME_2'){
  cluster_date_data <- label_map %>% 
    left_join(a3, by = c('quadkey')) %>% 
    filter(!!sym(level) %in% geo_names) %>% 
    select(date, quadkey, cluster)
  
  cluster_names <- cluster_date_data %>% pull(cluster) %>% unique()
  
  cluster_date_data <- label_map %>% 
    filter(cluster %in% cluster_names) %>% 
    group_by(date) %>% 
    group_split() 
  
  return(cluster_date_data)
}

plot_geography_intersection <- function(label_map, geo_names, title, level = 'NAME_2', n = 5, nrow = 1){
  
  cluster_date_data <- geography_intersection(label_map, geo_names, level)
  
  total_extent <- tiles %>% 
    filter(quadkey %in% c(do.call(rbind, cluster_date_data) %>% pull(quadkey))) %>% 
    st_bbox()
  
  pal <- qualitative_pal(c(do.call(rbind, cluster_date_data) %>% pull(cluster) %>% unique()))
  
  p <- lapply(cluster_date_data[1:n], plot_clusters, basemap = uk, custom_bbox = total_extent, custom_pal = pal)
  
  p <- cowplot::plot_grid(plotlist = p, nrow = nrow)
  
  title <- cowplot::ggdraw() + 
    cowplot::draw_label(title, x = 0, hjust = 0, size = 12) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  p <- cowplot::plot_grid(title, p, rel_heights = c(0.04, 0.9), ncol = 1)
  
  
  return(p)
  
}

p_lon <- plot_geography_intersection(label_map, 
                                     london, 
                                     "c", 
                                     'NAME_3', 
                                     18,
                                     9)

p_lei <- plot_geography_intersection(label_map, 
                                     c('Leicestershire', 'Leicester'), 
                                     "b",
                                     'NAME_2', 
                                     8, 
                                     4)

p_pers <- plot_clusters(most_persistent[[length(most_persistent)]], uk, 'a', uk_lims = T)

p <- cowplot::plot_grid(p_pers, p_lei, p_lon, nrow = 1, rel_widths = c(0.5, 0.25, 0.25))
fn <- '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map.png'

ggsave(fn, p,
       width = 8.5, height = 6,
       units = 'in')

#try to put the snakey figure on top of this - 20 mins to try 16:00

#plot persistent clusters, present on all days
get_most_persistant <- function(label_map, longest_lasting){
  
  most_persistent <- label_map %>% 
    filter(cluster %in% longest_lasting) %>% 
    left_join(a3, by = c('quadkey')) %>% 
    drop_na(NAME_2) %>% 
    group_by(date) %>% 
    group_split()
  
  return(most_persistent)
  
}

most_persistent <- get_most_persistant(label_map, longest_lasting)
most_persistent_lei <- get_most_persistant(label_map_lei, longest_lasting_lei)

plot_clusters(most_persistent[[1]], uk, uk_lims = T)

qualitative_pal <- function(names, rep_n = 3){
  
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), rep_n)
  
  pal <- pal[1:length(names)]
  names(pal) = names
  
  return(pal)
  
}

pal <- qualitative_pal(longest_lasting)
pal_lei <- qualitative_pal(longest_lasting_lei)

plot_national_geograpny <- function(df, pal, title = NULL){
  
  d <- tiles %>% 
    left_join(df, by = c('quadkey')) %>% 
    drop_na(cluster)
  
  date = d %>% pull(date) %>% unique()
  
  p <- d %>% 
    ggplot() + 
    geom_sf(aes(fill = cluster), size = 0.1, colour = 'black') + 
    scale_fill_manual(values = pal) +
    geom_sf(data = uk, size = 0.01, colour = 'black', fill = 'transparent') + 
    geom_sf(data = world, size = 0.1, colour = 'black', fill = 'transparent') + 
    xlim(-8, 2) + 
    ylim(50.4, 58.4) +
    theme_void() + 
    ggtitle(paste0(title, format(date, "%a %b %d"))) + 
    theme(legend.position = 'none')
  
  return(p)
}

p <- plot_national_geograpny(most_persistent[[1]], pal, 'Infomap most persistent ')
p_lei <- plot_national_geograpny(most_persistent_lei[[1]], pal_lei, 'Leiden most persistent ')

p <- cowplot::plot_grid(p, p_lei)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_most_persistent.png', p,
       width = 8.5, height = 6,
       units = 'in')

london <- c('City of London', 'Barking and Dagenham', 'Barnet', 'Bexley', 'Brent', 'Bromley', 'Camden', 'Croydon', 'Ealing', 'Enfield', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow', 'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Lambeth', 'Lewisham', 'Merton', 'Newham', 'Redbridge', 'Richmond upon Thames', 'Southwark', 'Sutton', 'Tower Hamlets', 'Waltham Forest', 'Wandsworth', 'Westminster')
