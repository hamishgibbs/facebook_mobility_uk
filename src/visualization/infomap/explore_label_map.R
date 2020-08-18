suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[3])

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

#most persistent cluster
persistence <- label_map %>% 
  group_by(cluster, date) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  group_by(cluster) %>% 
  summarise(n_days_present = n(), .groups = 'drop') %>% 
  arrange(-n_days_present)

persistence %>% 
  ggplot() + 
  geom_density(aes(x = n_days_present))

persistence %>% 
  ggplot() + 
  geom_density(aes(x = log(n_days_present, 10)))

longest_lasting <- persistence %>% 
  filter(n_days_present == max(n_days_present)) %>% 
  pull(cluster)

plot_cluster_timepoint = function(cluster_date){
  d <- tiles %>% 
    left_join(cluster_date, by = c('quadkey')) %>% 
    drop_na(cluster)
  
  date = d %>% pull(date) %>% unique()
  
  bbox <- st_bbox(d)
  
  p <- d %>% 
    ggplot() + 
    geom_sf(size = 0.1, colour = 'black', fill = 'lightblue') + 
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) +
    theme_void() + 
    ggtitle(date)
  
  return(p)
  
}

cluster_date_data <- label_map %>% filter(cluster == longest_lasting[1]) %>% 
  group_by(date) %>% 
  group_split()

plot_cluster_timepoint(cluster_date_data[[1]])

p <- lapply(cluster_date_data[1:20], plot_cluster_timepoint)

p <- cowplot::plot_grid(plotlist = p)

ggsave(tail(.args, 1), p,
       width = 8.5, height = 6,
       units = 'in')


#which modules intersect x geography
cluster_date_data <- label_map %>% 
  left_join(a3, by = c('quadkey')) %>% 
  #filter(NAME_2 %in% c('Leicestershire', 'Leicester')) %>% 
  filter(NAME_3 %in% london) %>% 
  select(date, quadkey, cluster)

cluster_names <- cluster_date_data %>% pull(cluster) %>% unique()

cluster_date_data <- label_map %>% 
  filter(cluster %in% cluster_names) %>% 
  group_by(date) %>% 
  group_split() 


plot_geography_clusters = function(cluster_date, pal){
  d <- tiles %>% 
    left_join(cluster_date, by = c('quadkey')) %>% 
    drop_na(cluster)
  
  date = d %>% pull(date) %>% unique()
  
  bbox <- st_bbox(d)
  
  p <- d %>% 
    ggplot() + 
    geom_sf(aes(fill = cluster), size = 0.1, colour = 'black') + 
    scale_fill_manual(values = pal) +
    geom_sf(data = uk, size = 0.1, colour = 'black', fill = 'transparent') + 
    ylim(bbox$ymin, bbox$ymax) + 
    xlim(bbox$xmin, bbox$xmax) +
    theme_void() + 
    ggtitle(format(date, "%a %b %d")) + 
    theme(legend.position = 'none')
  
  return(p)
  
}

library(RColorBrewer)
qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), 3)

pal <- pal[1:length(cluster_names)]
names(pal) = cluster_names

p <- lapply(cluster_date_data[1:30], plot_geography_clusters, pal = pal)

p <- cowplot::plot_grid(plotlist = p)

title <- cowplot::ggdraw() + 
  cowplot::draw_label("London modules with label inheritance",
  fontface = 'bold', x = 0, hjust = 0, size = 40) +
  theme(plot.margin = margin(0, 0, 0, 7))

p <- cowplot::plot_grid(title, p, rel_heights = c(0.04, 0.9), ncol = 1)

ggsave(tail(.args, 1), p,
       width = 25, height = 15,
       units = 'in')

#plot persistent clusters, present on all days
most_persistent <- label_map %>% 
  filter(cluster %in% longest_lasting) %>% 
  left_join(a3, by = c('quadkey')) %>% 
  drop_na(NAME_2) %>% 
  group_by(date) %>% 
  group_split()

qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), 3)

pal <- pal[1:length(longest_lasting)]
names(pal) = longest_lasting


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

p <- plot_national_geograpny(most_persistent[[1]], pal, 'Most persistent modules ')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_most_persistent.png', p,
       width = 4.5, height = 6,
       units = 'in')

london <- c('City of London', 'Barking and Dagenham', 'Barnet', 'Bexley', 'Brent', 'Bromley', 'Camden', 'Croydon', 'Ealing', 'Enfield', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow', 'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Lambeth', 'Lewisham', 'Merton', 'Newham', 'Redbridge', 'Richmond upon Thames', 'Southwark', 'Sutton', 'Tower Hamlets', 'Waltham Forest', 'Wandsworth', 'Westminster')
