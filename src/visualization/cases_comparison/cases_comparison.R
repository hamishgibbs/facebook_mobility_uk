suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore


if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Downloads/data_2020-Sep-10.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

a3 <-  read_csv(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

mob <- mob %>% 
  left_join(a3, by = c('start_quadkey' = 'quadkey')) %>% 
  drop_na(NAME_1)

lag <- 14

mob_sum <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  group_by(date) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T)) %>% 
  mutate(date = date)

cases <- read_csv(.args[3])

ggplot() + 
  geom_path(data = cases, aes(x = date, y = newCasesBySpecimenDate / sum(newCasesBySpecimenDate))) + 
  geom_path(data = mob_sum, aes(x = date, y = n_crisis / sum(n_crisis)), color = 'red') + 
  xlim(min(mob_sum$date), max(mob_sum$date))

cases_corr <- cases %>% 
  left_join(mob_sum, by = 'date') %>% drop_na(n_crisis) %>% 
  mutate(n_crisis = n_crisis / sum(n_crisis),
         newCasesBySpecimenDate = newCasesBySpecimenDate / sum(newCasesBySpecimenDate))

cor(cases_corr$newCasesBySpecimenDate, cases_corr$n_crisis)


