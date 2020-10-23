#Creates summary firgure of population size and area for each community
suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  library(RColorBrewer)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_bank_holidays.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_weekends.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_descriptive/community_size.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"),
         method = 'InfoMap')

tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

oa_pop <- read_csv(.args[3]) %>% 
  rename(quadkey = quadkey_12) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

im <- im %>% 
  group_by(date) %>% 
  group_split()
 
size_res <- list()
pop_res <- list()

for (i in 1:length(im)){
 size_res[[i]] <- tiles %>% left_join(im[[i]], by = c('quadkey')) %>%
   group_by(cluster) %>%
   summarise(size = n(),
             date = unique(date),
             .groups = 'drop') %>%
   mutate(area = st_area(geometry),
          area = units::set_units(area, 'km^2')) %>%
   st_drop_geometry()

 pop_res[[i]] <- oa_pop %>%
   left_join(im[[i]], by = c('quadkey')) %>%
   drop_na(cluster) %>%
   group_by(cluster) %>%
   summarise(pop = sum(pop, na.rm = T),
             date = unique(date),
             .groups = 'drop')

 print(i / length(im))

}

size_res <- do.call(rbind, size_res)
pop_res <- do.call(rbind, pop_res)

write_csv(size_res, tail(.args, 1))

write_csv(pop_res, gsub('community_size.csv', 'community_pop.csv', tail(.args, 1)))

