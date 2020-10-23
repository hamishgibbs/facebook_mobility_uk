suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(RColorBrewer)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/qualitative_pal.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/utils.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/google_comparison_functions.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Downloads/gadm36_GBR_shp (2)/gadm36_GBR_3.shp',
              '/Users/hamishgibbs/Downloads/Local_Authority_Districts__December_2019__Boundaries_UK_BFC-shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFC.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/la_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain-shp/la_pop.csv',
              '/Users/hamishgibbs/Downloads/Region_Mobility_Report_CSVs/2020_GB_Region_Mobility_Report.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/label_map_geography_test.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

label_map <- read_csv(.args[1])

tiles <- st_read(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

cases <- readr::read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>% 
  rename(date = `Specimen date`) %>% 
  rename(code = `Area code`) %>% 
  rename(name = `Area name`) %>% 
  rename(count = `Daily lab-confirmed cases`) %>% 
  mutate(area_type = stringr::str_sub(code, 1, 2)) %>% 
  filter(area_type == 'E0')

a3 <- read_csv(.args[4])

mob <- read_csv(.args[5]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

la <- st_read(.args[7]) %>% 
  st_simplify(dTolerance = 0.001) %>% 
  mutate(country = stringr::str_sub(lad19cd, 1, 1)) %>% 
  filter(country == 'E') %>% 
  st_transform(4326)

goog <- read_csv(.args[10], col_types = cols(sub_region_2 = col_character()))

name_lei <- c('Leicester')

la_name <- name_lei

les <- area_quadkeys(la_name)

area <- la %>% filter(lad19nm == la_name)

les_mob <- get_area_mob(les)

min_date <- les_mob %>% pull(date) %>% min()
max_date <- les_mob %>% pull(date) %>% max()

i_date <- as.Date('2020-06-29')

p1 <- plot_google_mobility('GB-LCE', min_date, max_date, i_date)

p2 <- plot_facebook_mobility(les_mob, i_date) 

p_cases <- plot_area_cases(la_name, min_date, max_date, i_date, cases)

early_community <- get_time_agg_community(les, i_date, type='early')

late_community <- get_time_agg_community(les, i_date, type='late')

p_early <- plot_time_agg_community(early_community, 'Pre-lockdown community', area)

p_late <- plot_time_agg_community(late_community, 'Post-lockdown community', area)

p_early <- cowplot::plot_grid(letter_title('a'), p_early, rel_heights = c(0.1, 1), nrow = 2)
p_late <- cowplot::plot_grid(letter_title('b'), p_late, rel_heights = c(0.1, 1), nrow = 2)
p2 <- cowplot::plot_grid(letter_title('c'), p2, rel_heights = c(0.1, 1), nrow = 2)
p1 <- cowplot::plot_grid(letter_title('d'), p1, rel_heights = c(0.1, 1), nrow = 2)
p_cases <- cowplot::plot_grid(letter_title('e'), p_cases, rel_heights = c(0.1, 1), nrow = 2)

p_map <- cowplot::plot_grid(p_early, p_late, nrow = 2)

legend <- cowplot::get_legend(plot_time_agg_community(early_community, 'Pre-lockdown community', area, legend = T))
p_map <- cowplot::plot_grid(p_map, legend, nrow = 1, rel_widths = c(0.8, 0.2))

p <- cowplot::plot_grid(p2, p1, p_cases, nrow = 3)

p <- cowplot::plot_grid(p_map, p, rel_widths = c(0.4, 0.6))

#p

#code comments and clean run - then single function with component parts - then clean run

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/leicester.png', p, 
       width = 13, 
       height = 7, 
       units = 'in')

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/leicester.pdf', p, 
       width = 12, 
       height = 7, 
       units = 'in')

                        
