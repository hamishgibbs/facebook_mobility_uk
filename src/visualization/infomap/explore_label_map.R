suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(RColorBrewer)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map.png')
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
london <- c('City of London', 'Barking and Dagenham', 'Barnet', 'Bexley', 'Brent', 'Bromley', 'Camden', 'Croydon', 'Ealing', 'Enfield', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow', 'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Lambeth', 'Lewisham', 'Merton', 'Newham', 'Redbridge', 'Richmond upon Thames', 'Southwark', 'Sutton', 'Tower Hamlets', 'Waltham Forest', 'Wandsworth', 'Westminster')

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

get_most_persistent <- function(label_map, longest_lasting){
  
  most_persistent <- label_map %>% 
    filter(cluster %in% longest_lasting) %>% 
    left_join(a3, by = c('quadkey')) %>% 
    drop_na(NAME_2) %>% 
    group_by(date) %>% 
    group_split()
  
  return(most_persistent)
  
}

most_persistent <- get_most_persistent(label_map, longest_lasting)
most_persistent_lei <- get_most_persistent(label_map_lei, longest_lasting_lei)

p_pers <- plot_clusters(most_persistent[[1]], uk, 'a', uk_lims = T)

p <- cowplot::plot_grid(p_pers, p_lei, p_lon, nrow = 1, rel_widths = c(0.5, 0.25, 0.25))

fn <- '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map.png'

ggsave(fn, p,
       width = 8.5, height = 6,
       units = 'in')

ggsave(gsub('.png', '.pdf', fn), p,
       width = 8.5, height = 6,
       units = 'in')

#try to put the snakey figure on top of this - 20 mins to try 16:00

qualitative_pal <- function(names, rep_n = 3){
  
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), rep_n)
  
  pal <- pal[1:length(names)]
  names(pal) = names
  
  return(pal)
  
}

label_map <- label_map %>% 
  group_by(date) %>% 
  group_split()

label_map_lei <- label_map_lei %>% 
  group_by(date) %>% 
  group_split()

im <- label_map[[1]]
lei <- label_map_lei[[1]]

pal <- qualitative_pal(im$cluster, 100)
pal_lei <- qualitative_pal(lei$cluster, 100)

p <- plot_clusters(im, uk, title = paste0(unique(im$date), ' Infomap'))
p_lei <- plot_clusters(lei, uk, title = paste0(unique(lei$date), ' Leiden'))

p <- cowplot::plot_grid(p, p_lei)

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/method_comparison.png', p,
       width = 6, height = 6,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/method_comparison.pdf', p,
       width = 6, height = 6,
       units = 'in')
