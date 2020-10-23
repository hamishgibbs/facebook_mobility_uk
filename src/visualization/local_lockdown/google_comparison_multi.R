suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(RColorBrewer)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/qualitative_pal.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_clusters.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/utils.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/local_lockdown/google_comparison_functions.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_12.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Downloads/Local_Authority_Districts__December_2019__Boundaries_UK_BFC-shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFC.shp',
              '/Users/hamishgibbs/Downloads/Region_Mobility_Report_CSVs/2020_GB_Region_Mobility_Report.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/manchester.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

#load communities with labels mapped through time
label_map <- read_csv(.args[1])

#load tiles
tiles <- st_read(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0")) %>% 
  st_set_crs(4326)

#load case data
cases <- readr::read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>% 
  rename(date = `Specimen date`) %>% 
  rename(code = `Area code`) %>% 
  rename(name = `Area name`) %>% 
  rename(count = `Daily lab-confirmed cases`) %>% 
  mutate(area_type = stringr::str_sub(code, 1, 2)) %>% 
  filter(area_type == 'E0')

#load a3 data
a3 <- read_csv(.args[3])

#load daily movement data
mob <- read_csv(.args[4]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

#load local authority data
la <- st_read(.args[5]) %>% 
  st_simplify(dTolerance = 100) %>% 
  #mutate(country = stringr::str_sub(lad19cd, 1, 1)) %>% 
  #filter(country == 'E') %>% 
  st_transform(4326)

#load google mobility data
goog <- read_csv(.args[6], col_types = cols(sub_region_2 = col_character()))

la_name_ne <- c('Newcastle upon Tyne', 'Gateshead', 'Northumberland', 'North Tyneside', 'South Tyneside', 'Sunderland')
i_date_ne <- as.Date('2020-09-18')
goog_code_ne <- 'GB-NET'
la_name_mn <- c('Manchester', 'Trafford', 'Bury', 'Tameside', 'Rochdale', 'Salford', 'Oldham', 'Stockport', 'Wigan', 'Bolton')
i_date_mn <- as.Date('2020-07-31')
goog_code_mn <- 'GB-MAN'
la_name_ln <- c('Blackburn with Darwen', 'Blackpool', 'Burnley', 'Chorley', 'Fylde', 'Hyndburn', 'Lancaster', 'Pendle', 'Preston', 'Ribble Valley', 'South Ribble', 'Rossendale', 'West Lancashire', 'Wyre')
i_date_ln <- as.Date('2020-08-22')
goog_code_ln <- 'GB-BPL'
la_name_lei <- c('Leicester')
i_date_lei <- as.Date('2020-06-29')
goog_code_lei <- 'GB-LCE'

plot_local_comparison <- function(la_name, i_date, goog_code){
  
  les <- area_quadkeys(la_name)
  
  area <- la %>% filter(lad19nm %in% la_name)
  
  les_mob <- get_area_mob(les)
  
  min_date <- les_mob %>% pull(date) %>% min()
  max_date <- les_mob %>% pull(date) %>% max()
  
  p1 <- plot_google_mobility(goog_code, min_date, max_date, i_date)
  
  p2 <- plot_facebook_mobility(les_mob, i_date)
  
  p_cases <- plot_area_cases(la_name, min_date, max_date, i_date, cases)
  
  early_community <- get_time_agg_community(les, i_date, type='early')
  
  late_community <- get_time_agg_community(les, i_date, type='late')
  
  p_early <- plot_time_agg_community(early_community, 'Pre-lockdown community', area)
  
  p_late <- plot_time_agg_community(late_community, 'Post-lockdown community', area)
  
  p_early <- cowplot::plot_grid(letter_title('a'), p_early, rel_heights = c(0.1, 1), nrow = 2)
  p_late <- cowplot::plot_grid(letter_title('b'), p_late, rel_heights = c(0.1, 1), nrow = 2)
  p1 <- cowplot::plot_grid(letter_title('d'), p1, rel_heights = c(0.1, 1), nrow = 2)
  p2 <- cowplot::plot_grid(letter_title('c'), p2, rel_heights = c(0.1, 1), nrow = 2)
  p_cases <- cowplot::plot_grid(letter_title('e'), p_cases, rel_heights = c(0.1, 1), nrow = 2)
  
  p_map <- cowplot::plot_grid(p_early, p_late, nrow = 2, rel_widths = c(0.2, 0.2, 0.6))
  
  legend <- cowplot::get_legend(plot_time_agg_community(early_community, 'Pre-lockdown community', area, legend = T))
  p_map <- cowplot::plot_grid(p_map, legend, nrow = 1, rel_widths = c(0.8, 0.2))
  
  p_det <- cowplot::plot_grid(p2, p1, p_cases, nrow = 3)
  
  p <- cowplot::plot_grid(p_map, p_det, nrow = 1, rel_widths = c(0.4, 0.6))
  
  return(p)
  
}

print('Plotting Manchester')
p <- plot_local_comparison(la_name_mn, i_date_mn, goog_code_mn)

ggsave(tail(.args, 1), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       useDingbats = FALSE,
       units = 'in')

print('Plotting NE')
p <- plot_local_comparison(la_name_ne, i_date_ne, goog_code_ne)

ggsave(gsub('manchester.png', 'ne.png', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')

ggsave(gsub('manchester.png', 'ne.pdf', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')

print('Plotting LN')
p <- plot_local_comparison(la_name_ln, i_date_ln, goog_code_ln)

ggsave(gsub('manchester.png', 'ln.png', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')

ggsave(gsub('manchester.png', 'ln.pdf', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')

print('Plotting LEI')
p <- plot_local_comparison(la_name_lei, i_date_lei, goog_code_lei)

ggsave(gsub('manchester.png', 'leicester.png', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')

ggsave(gsub('manchester.png', 'leicester.pdf', tail(.args, 1)), 
       p, 
       width = 13, 
       height = 7, 
       units = 'in')
