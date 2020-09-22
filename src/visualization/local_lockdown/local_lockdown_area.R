suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/utils.R')
if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Downloads/gadm36_GBR_shp (2)/gadm36_GBR_3.shp',
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

mob <- read_csv(.args[5]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

gadm <- st_read(.args[6]) %>% 
  st_simplify(dTolerance = 0.001)
#function to get week preceding intervention and week following and compute % change from baseline + maximum extent of all intersecting modules

p <- plot_lockdown_extent('Leicester', '2020-06-29', F)

#p2 <- plot_lockdown_extent('Caerphilly', '2020-09-08', F)

legend <- cowplot::get_legend(
  p_change_perc + theme(legend.box.margin = margin(0, 0, 0, 12))
)

#p <- cowplot::plot_grid(p1, p2, nrow = 2)

p <- cowplot::plot_grid(p, legend, rel_widths = c(1, 0.15))


ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/local_lockdown.png', 
       p,
       width = 11, height = 5,
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/local_lockdown.pdf', 
       p,
       width = 9, height = 7,
       units = 'in',
       useDingbats = F)
