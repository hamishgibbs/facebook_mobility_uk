suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/backbone_summary.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/backbone_date_focus.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/backbone_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

bb <- read_csv(.args[1]) %>% 
  select(-degree_sequence, -X1) %>% 
  mutate(ratio_diff = flow_ratio - edge_ratio) 

bb_focus <- read_csv(.args[2])

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

r_diff <- bb %>% 
  group_by(date) %>% 
  summarise(ratio_diff = max(ratio_diff, na.rm = T)) %>% 
  group_by(date) %>% 
  group_split()

bb_raw <- bb %>% 
  group_by(date) %>% 
  group_split()

bb <- bb %>%   
  select(edge_ratio, flow_ratio, alpha, date) %>% 
  pivot_longer(c(-alpha, -date)) %>% 
  mutate(name = ifelse(name == 'edge_ratio', 'Edge Ratio', name),
         name = ifelse(name == 'flow_ratio', 'Flow Ratio', name)) %>% 
  group_by(date) %>% 
  group_split()

plot_backbone_sensitivity <- function(bb, bb_raw, r_diff, title){
  
  line_data <- bb_raw %>% filter(ratio_diff == r_diff$ratio_diff)
  
  annotation <- paste0('Alpha = ', line_data$alpha, ': ', round(line_data$ratio_diff, 3))
  
  p <- bb %>% 
    ggplot() + 
    geom_path(aes(x = alpha, y = value, color = name)) + 
    scale_color_manual(values = c('Edge Ratio' = 'red', 'Flow Ratio' = 'black')) + 
    #geom_segment(aes(x = line_data$alpha, y = line_data$edge_ratio, xend = line_data$alpha, yend = line_data$flow_ratio), color = 'blue') + 
    #annotate("text", x = line_data$alpha + 0.3, y = 0.5, label = annotation, size = 3) + 
    theme_bw() + 
    plot_default_theme + 
    theme(legend.title = element_blank(),
          strip.background = element_rect(fill = 'white'),
          legend.position = 'none') + 
    ylab('Proportion') + 
    xlab('Alpha') + 
    ggtitle(title) 
  
  
  return(p)
}

p1 <- plot_backbone_sensitivity(bb[[1]], bb_raw[[1]], r_diff[[1]], 'a')
p2 <- plot_backbone_sensitivity(bb[[2]], bb_raw[[2]], r_diff[[2]], 'b')
p3 <- plot_backbone_sensitivity(bb[[3]], bb_raw[[3]], r_diff[[3]], 'c')
p4 <- plot_backbone_sensitivity(bb[[4]], bb_raw[[4]], r_diff[[4]], 'd')

p <- cowplot::plot_grid(p1, p2, p3, p4, nrow = 2)
p

ggsave(tail(.args, 1), p, width = 8.5, height = 3.5, units = 'in')
ggsave(tail(.args, 1), p, width = 8.5, height = 3.5, units = 'in')


journey_line <- function(journey){
  
  journey <- str_split(journey, '_')
  
  start_poly <- tiles %>% filter(quadkey == journey[1])
  end_poly <- tiles %>% filter(quadkey == journey[2])
  
  line <- suppressWarnings(st_linestring(rbind(st_coordinates(st_geometry(st_centroid(start_poly))), st_coordinates(st_geometry(st_centroid(end_poly))))))
  
  return(line)
}

bb_f <- bb_focus %>% 
  group_by(date) %>% 
  group_split()

journey_names <- bb_focus %>% pull(journey) %>% unique() %>% str_split(., '_')
lines <- st_as_sf(st_sfc(lapply(journey_names, journey_line)))
st_crs(lines) <- 4326
lines$journey = bb_focus %>% pull(journey) %>% unique() 

focus_lines <- bb_focus %>% left_join(lines) %>% st_as_sf() %>% 
  group_by(date) %>% 
  group_split(., .keep = 'drop')
require(leaflet)

l <- leaflet() %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% 
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  addPolylines(data = focus_lines[[1]], weight = 0.7, opacity = 1, color = 'black', group = as.character(unique(focus_lines[[1]]$date))) %>% 
  addPolylines(data = focus_lines[[2]], weight = 0.7, opacity = 1, color = 'black', group = as.character(unique(focus_lines[[2]]$date))) %>% 
  addPolylines(data = focus_lines[[3]], weight = 0.7, opacity = 1, color = 'black', group = as.character(unique(focus_lines[[3]]$date))) %>% 
  addLayersControl(position = "bottomleft",
                   baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                   overlayGroups = c(as.character(unique(focus_lines[[1]]$date)), 
                                     as.character(unique(focus_lines[[2]]$date)),
                                     as.character(unique(focus_lines[[3]]$date))))

htmlwidgets::saveWidget(l, file="~/Downloads/m.html")









