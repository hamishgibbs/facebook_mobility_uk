suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(lubridate)
})



if(interactive()){
  .args <-  c('https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/cases/summary/cases_by_report.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/rt_data/nhs/tiles_nhs.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

rt <- read_csv(.args[1])

mob <- read_csv(.args[2]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

a3 <-  read_csv(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

nhs <- read_csv(.args[4]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

#join a3 data onto mob
nhs_mob <- mob %>% 
  left_join(a3, by = c('start_quadkey' = 'quadkey')) %>% 
  select(date, start_quadkey, end_quadkey, n_crisis, n_baseline, NAME_1) %>% 
  rename(NAME_1_start = NAME_1) %>% 
  left_join(a3, by = c('end_quadkey' = 'quadkey')) %>% 
  select(date, start_quadkey, end_quadkey, n_crisis, n_baseline, NAME_1_start, NAME_1) %>% 
  rename(NAME_1_end = NAME_1) %>% 
  left_join(nhs, by = c('start_quadkey' = 'quadkey')) %>% 
  select(date, start_quadkey, end_quadkey, n_crisis, n_baseline, NAME_1_start, NAME_1_end, nhs_name) %>% 
  rename(nhs_name_start = nhs_name) %>% 
  left_join(nhs, by = c('end_quadkey' = 'quadkey')) %>% 
  select(date, start_quadkey, end_quadkey, n_crisis, n_baseline, NAME_1_start, NAME_1_end, nhs_name_start, nhs_name) %>% 
  rename(nhs_name_end = nhs_name) %>% 
  mutate(nhs_name_start = ifelse(NAME_1_start %in% c('Scotland', 'Wales', 'Northern Ireland'), NAME_1_start, nhs_name_start),
         nhs_name_end = ifelse(NAME_1_end %in% c('Scotland', 'Wales', 'Northern Ireland'), NAME_1_end, nhs_name_end),
         nhs_name_end = ifelse(nhs_name_end %in% c('Midlands'), 'West Midlands', nhs_name_end),
         nhs_name_start = ifelse(nhs_name_start %in% c('Midlands'), 'West Midlands', nhs_name_start),
         nhs_name_end = ifelse(nhs_name_end %in% c('North East and Yorkshire'), 'North East', nhs_name_end),
         nhs_name_start = ifelse(nhs_name_start %in% c('North East and Yorkshire'), 'North East', nhs_name_start)) %>% 
  drop_na(nhs_name_start, nhs_name_end)

rt_names <- rt %>% pull(region) %>% unique()
  
mob_names <- nhs_mob %>%  pull(nhs_name_start) %>% unique()
  
setdiff(rt_names, mob_names)

setdiff(mob_names, rt_names)


nhs_mob_rt <- nhs_mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  mutate(weekday = as.character(wday(date, label = T))) %>% 
  #filter(!weekday %in% c('Sun', 'Sat')) %>% 
  #filter(!date %in% c(as.Date('2020-08-31'))) %>% 
  filter(nhs_name_start == nhs_name_start) %>% 
  group_by(date, nhs_name_start) %>% 
  summarise(n_crisis = sum(n_crisis),
            n_baseline = sum(n_baseline)) %>% 
  left_join(rt, by = c('date' = 'date', 'nhs_name_start' = 'region')) %>% 
  drop_na(mean)


nhs_mob_rt %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n_crisis / sum(n_crisis), group = nhs_name_start)) + 
  geom_path(aes(x = date, y = mean / sum(mean), group = nhs_name_start), color = 'red') + 
  facet_wrap(~nhs_name_start)


rt %>% pull(region) %>% unique()

as.data.frame()

nhs %>% pull(nhser17nm)
