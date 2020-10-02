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

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
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


les <- st_intersection(tiles, la %>% filter(lad19nm == 'Leicester'))

les <- les %>% pull(quadkey) %>% unique()


les_mob <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  mutate(type = ifelse(start_quadkey %in% les, 'Outflow', NA),
         type = ifelse(end_quadkey %in% les, 'Inflow', type),
         type = ifelse(start_quadkey %in% les & end_quadkey %in% les, 'Internal flow', type)) %>% 
  filter(!is.na(type)) %>% 
  group_by(date, type) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T),
            n_baseline = sum(n_baseline, na.rm = T),
            perc_change = ((n_crisis - n_baseline) / n_baseline) * 100) 

min_date <- les_mob %>% pull(date) %>% min()
max_date <- les_mob %>% pull(date) %>% max()

i_date <- as.Date('2020-06-29')

p1 <- goog %>% 
  filter(iso_3166_2_code == 'GB-LCE',
         date > min_date,
         date < max_date) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = retail_and_recreation_percent_change_from_baseline, colour = 'Retail and recreation')) + 
  geom_path(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, colour = 'Grocery and pharmacy')) + 
  geom_path(aes(x = date, y = transit_stations_percent_change_from_baseline, colour = 'Transit stations')) + 
  geom_path(aes(x = date, y = workplaces_percent_change_from_baseline, colour = 'Workplaces')) + 
  geom_path(aes(x = date, y = residential_percent_change_from_baseline, colour = 'Residential')) +  
  scale_color_manual(values = c('Retail and recreation' = '#984ea3', 'Grocery and pharmacy' = '#377eb8', 'Transit stations' = '#e41a1c', 'Workplaces' = '#4daf4a', 'Residential' = '#ff7f00')) + 
  geom_vline(aes(xintercept = i_date), linetype = 'dashed') + 
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank()) + 
  ylab('Percent change from baseline') + 
  ggtitle('Google')

p2 <- les_mob %>% 
  ggplot() + 
  geom_path(aes(x = date, y = perc_change, group = type, color = type)) + 
  scale_color_manual(values = c('#a6cee3', '#1f78b4', '#33a02c')) + 
  geom_vline(aes(xintercept = i_date), linetype = 'dashed') + 
  theme_bw() +
  theme(legend.title = element_blank()) + 
  theme(axis.title.x = element_blank(),
        legend.margin = margin(r = 0.76, unit = 'in')) + 
  ylab('Percent change from baseline') + 
  ggtitle('Facebook')

comm_min_date <- i_date - 14
comm_max_date <- i_date + 14

connected_community <- label_map %>% 
  filter(date > comm_min_date, date < comm_max_date) %>% 
  filter(quadkey %in% les) %>% 
  pull(cluster) %>% unique()

early_community <- label_map %>% 
  filter(date > comm_min_date, date <= i_date) %>% 
  filter(cluster %in% connected_community) %>% 
  group_by(quadkey) %>% 
  summarise(cluster = unique(cluster),
            n_dates = length(unique(date)))

late_community <- label_map %>% 
  filter(date >= i_date, date < comm_max_date) %>% 
  filter(cluster %in% connected_community) %>% 
  group_by(quadkey) %>% 
  summarise(cluster = unique(cluster),
            n_dates = length(unique(date)))

p_early_data <- tiles %>% 
  left_join(early_community, by = c('quadkey')) %>% 
  drop_na(cluster) #%>% 
  #group_by(cluster) %>% 
  #summarise(n = n())

bbox <- st_bbox(p_early_data)

p_early <- ggplot() + 
  geom_sf(data = p_early_data, aes(fill = n_dates), color = 'black', size = 0.2) + 
  geom_sf(data = la, fill = 'transparent', size = 0.2, color = 'black') + 
  colorspace::scale_fill_continuous_sequential('Mint', rev = F) +
  xlim(bbox[1], bbox[3]) + 
  ylim(bbox[2], bbox[4]) + 
  theme_void() + 
  ggtitle('Pre-lockdown community') + 
  theme(legend.position = 'none')

p_late_data <- tiles %>% 
  left_join(late_community, by = c('quadkey')) %>% 
  drop_na(cluster)

bbox <- st_bbox(p_late_data)

p_late <- ggplot() + 
  geom_sf(data = p_late_data, aes(fill = n_dates), color = 'black', size = 0.2) + 
  geom_sf(data = la, fill = 'transparent', size = 0.2, color = 'black') + 
  colorspace::scale_fill_continuous_sequential('Mint', rev = F) +
  xlim(bbox[1], bbox[3]) + 
  ylim(bbox[2], bbox[4]) + 
  theme_void() + 
  ggtitle('Post-lockdown community') + 
  labs(fill = 'Days in\nLeicester\ncommunity') + 
  theme(legend.position = 'none')

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

p_early <- cowplot::plot_grid(letter_title('a'), p_early, rel_heights = c(0.1, 1), nrow = 2)
p_late <- cowplot::plot_grid(letter_title('b'), p_late, rel_heights = c(0.1, 1), nrow = 2)
p1 <- cowplot::plot_grid(letter_title('c'), p1, rel_heights = c(0.1, 1), nrow = 2)
p2 <- cowplot::plot_grid(letter_title('d'), p2, rel_heights = c(0.1, 1), nrow = 2)

p_map <- cowplot::plot_grid(p_early, p_late, nrow = 2)

p_map <- cowplot::plot_grid(p_map, legend, nrow = 1, rel_widths = c(0.8, 0.2))
#legend <- cowplot::get_legend(p_late)

p <- cowplot::plot_grid(p1, p2, nrow = 2)

p <- cowplot::plot_grid(p_map, p, rel_widths = c(0.4, 0.6))

#p

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/leicester.png', p, width = 10, height = 5, units = 'in')
                        
