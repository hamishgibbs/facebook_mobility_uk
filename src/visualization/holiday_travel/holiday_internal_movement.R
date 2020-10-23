suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(colorspace)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore
source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/population_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference_13.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
              '/Users/hamishgibbs/Downloads/gadm36_GBR_shp (2)/gadm36_GBR_3.shp',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/holiday_pop.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

pop <- read_csv(.args[1]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))

london <- c('City of London', 'Barking and Dagenham', 'Barnet', 'Bexley', 'Brent', 'Bromley', 'Camden', 'Croydon', 'Ealing', 'Enfield', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow', 'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Lambeth', 'Lewisham', 'Merton', 'Newham', 'Redbridge', 'Richmond upon Thames', 'Southwark', 'Sutton', 'Tower Hamlets', 'Waltham Forest', 'Wandsworth', 'Westminster')

a3 <-  read_csv(.args[2]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))
#NAME_3 = ifelse(NAME_3 %in% london, 'London', NAME_3))

a3_12 <-  read_csv(.args[3]) %>% 
  mutate(quadkey = str_pad(quadkey, 12, pad = "0"))
#NAME_3 = ifelse(NAME_3 %in% london, 'London', NAME_3))

#need ref between population data and a3 areas - combine population to a3 area 
#combine this with oa pop values (sketchy but ok)

#2 panel figure - abs change in population, agreement between baseline pop and oa pop for admin areas

oa_pop <-  read_csv(.args[4]) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0")) 

gadm <- st_read(.args[5]) %>% 
  st_simplify(dTolerance = 0.015)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

oa_pop <- oa_pop %>% 
  left_join(a3_12, by = c('quadkey_12' = 'quadkey')) %>% 
  drop_na(NAME_1) %>% 
  group_by(NAME_3) %>% 
  summarise(pop = sum(pop, na.rm = T))

pop <- pop %>% 
  left_join(a3, by = c('quadkey')) %>% 
  drop_na(NAME_1) 

p_data <- pop %>% 
  group_by(date, NAME_3) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T),
            n_baseline = sum(n_baseline, na.rm = T)) %>% 
  filter(date <= as.Date('2020-08-7')) %>% 
  group_by(NAME_3) %>% 
  arrange(date) %>% 
  mutate(n_crisis = zoo::rollmean(n_crisis, k = 7, fill = NA),
         n_baseline = zoo::rollmean(n_baseline, k = 7, fill = NA))

med <- p_data %>% 
  mutate(perc = ((n_crisis - n_baseline) / n_baseline) * 100,
         raw = n_crisis - n_baseline) %>% 
  group_by(date) %>% 
  summarise(perc = median(perc, na.rm = T),
            raw = median(raw, na.rm = T))

p_norm <- p_data %>% 
  ggplot() + 
  geom_rect(aes(xmin=as.Date('2020-07-01'), xmax=as.Date('2020-07-31'), ymin=-Inf, ymax=Inf), fill = 'lightgrey') + 
  geom_rect(aes(xmin=as.Date('2020-04-01'), xmax=as.Date('2020-04-30'), ymin=-Inf, ymax=Inf), fill = 'lightgrey') + 
  geom_path(aes(x = date, y = ((n_crisis - n_baseline) / n_baseline) * 100, group = NAME_3), size = 0.1, color = 'darkblue', alpha = 0.7) + 
  geom_path(data = med, aes(x = date, y = perc), color = 'red', size = 0.6) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.text.y = element_text(margin = unit(c(0, 0, 0, 0.22), "in"))) + 
  ylab('% Change from Baseline') + 
  ggtitle('a')

p_raw <- p_data %>% 
  ggplot() + 
  geom_rect(aes(xmin=as.Date('2020-07-01'), xmax=as.Date('2020-07-31'), ymin=-Inf, ymax=Inf), fill = 'lightgrey') + 
  geom_rect(aes(xmin=as.Date('2020-04-01'), xmax=as.Date('2020-04-30'), ymin=-Inf, ymax=Inf), fill = 'lightgrey') + 
  geom_path(aes(x = date, y = (n_crisis - n_baseline), group = NAME_3), size = 0.1, color = 'darkblue', alpha = 0.7) + 
  geom_path(data = med, aes(x = date, y = raw), color = 'red', size = 0.6) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) + 
  ylab('Absolute Change from Baseline') + 
  ggtitle('b')

#need to account for change in total people recorded in each month?

month_totals <- p_data %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month) %>% 
  summarise(n_crisis = mean(n_crisis), .groups = 'drop') %>% 
  filter(month %in% c(4, 7))

#diff = month_totals$n_crisis[2] - month_totals$n_crisis[1]

p_compare_data <- p_data %>%
  left_join(oa_pop, by = c('NAME_3')) %>% 
  mutate(month = lubridate::month(date)) %>%  
  group_by(month, NAME_3) %>% 
  summarise(n_baseline = mean(n_baseline, na.rm = T),
            n_crisis = mean(n_crisis, na.rm = T),
            pop = unique(pop)) %>% 
  drop_na(pop) %>% 
  mutate(in_london = ifelse(NAME_3 %in% london, 'London', 'Other')) %>% 
  filter(month %in% c(4, 7)) %>% 
  mutate(month = as.character(month),
         month = ifelse(month == 4, 'April', month),
         month = ifelse(month == 7, 'July', month))
         #n_crisis = ifelse(month == 4, n_crisis + diff, n_crisis))

xint <- pop %>% 
  group_by(date, NAME_3) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T),
            n_baseline = sum(n_baseline, na.rm = T)) %>% 
  filter(date <= as.Date('2020-08-7')) %>% left_join(oa_pop, by = c('NAME_3')) %>% drop_na(pop) %>% 
  mutate(prop = n_crisis / pop) %>% pull(prop) %>% mean()

p_oa_compare <- p_compare_data %>% 
  ggplot() + 
  geom_abline(intercept = 0, slope = xint, color = 'blue', linetype = 'dashed') + 
  geom_point(aes(x = pop, y = n_crisis, color = as.character(month), group = NAME_3), size = 0.5) + 
  geom_smooth(aes(x = pop, y = n_crisis, color = as.character(month), group = month), size = 0.3, 
              method = 'lm', 
              formula = y ~ 0 + x,
              fill = 'transparent', 
              linetype = 'dashed') + 
  geom_path(aes(x = pop, y = n_crisis, group = NAME_3), size = 0.1) +
  scale_color_manual(values = c('April' = 'black', 'July' = 'red')) + 
  theme_bw() + 
  plot_default_theme + 
  ylab('Facebook Population') + 
  xlab('Small Area Population') + 
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.5),
        text = element_text(size = 12)) + 
  ggtitle('c')

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 0, 21)))
}

map_data <- p_data %>% 
  mutate(month = lubridate::month(date)) %>% 
  filter(month == 7) %>% 
  group_by(NAME_3) %>% 
  summarise(n_baseline = mean(n_baseline, na.rm = T),
            n_crisis = mean(n_crisis, na.rm = T),
            diff =  n_crisis - n_baseline)

p_compare_data_a  <- p_compare_data %>% filter(month == 'April') %>% ungroup() %>% select(NAME_3, n_crisis) %>% rename(n_crisis_a = n_crisis)
p_compare_data_j  <- p_compare_data %>% filter(month == 'July') %>% ungroup() %>% select(NAME_3, n_crisis) %>% rename(n_crisis_j = n_crisis)

map_data <- p_compare_data_a %>% 
  left_join(p_compare_data_j, by = c('NAME_3')) %>% 
  mutate(n_crisis_diff = n_crisis_j - n_crisis_a)

p_map <- gadm %>% 
  left_join(map_data, by = c('NAME_3')) %>% 
  ggplot() +
  geom_sf(data = world, size = 0.1, fill = 'transparent') + 
  geom_sf(aes(fill = n_crisis_diff), size = 0.1, color = 'black') + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) + 
  scale_fill_gradient2(low = 'red3', mid = 'white', high = 'royalblue4', breaks = c(-5931, -3000, 0, 2000, 4085)) + 
  theme_void() + 
  plot_default_theme + 
  theme(legend.position = c(0.8, 0.8),
        text = element_text(size = 10)) + 
  ggtitle('Population change April to July') + 
  labs(fill = 'Population\nChange')

map_title <- letter_title('d')

p_map <- cowplot::plot_grid(map_title, p_map, rel_heights = c(0.05, 1), nrow = 2)

p_ts <- cowplot::plot_grid(p_norm, p_raw, nrow = 2)

p_oa <- p_oa_compare

p <- cowplot::plot_grid(p_ts, p_oa, p_map, nrow = 1)

ggsave(tail(.args, 1), 
       p,
       width = 11, height = 5,
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), 
       p,
       width = 11, height = 5,
       units = 'in',
       useDingbats = F)


# Supplemental figure - 

dens <- gadm %>% 
  mutate(area = units::set_units(st_area(geometry), km^2)) %>% 
  left_join(oa_pop, by = c('NAME_3')) %>% 
  mutate(density = pop / as.numeric(area)) %>% 
  st_drop_geometry() %>% 
  select(NAME_3, pop, density)

m_compare <- p_compare_data %>% 
  select(NAME_3, month, n_crisis, in_london) %>% 
  pivot_wider(names_from = month, values_from = n_crisis) %>% 
  mutate(diff = July - April) %>% 
  left_join(dens, by = c('NAME_3'))


plot_change <- function(m_compare){
  
  p <- m_compare %>% 
    ggplot() + 
    geom_point(aes(x = pop, y = diff), size = 0.2) + 
    geom_point(data = m_compare %>% filter(in_london == 'London'), aes(x = pop, y = diff, color = 'London\nborough'), 
               size = 0.5) + 
    scale_color_manual(values = c('London\nborough' = 'red')) +
    #geom_smooth(aes(x = pop, y = diff), method = 'lm', color = 'black') + 
    geom_hline(aes(yintercept = 0), linetype = 'dashed') + 
    #ggpubr::stat_cor(aes(x = pop, y = diff)) + 
    theme_bw() + 
    plot_default_theme + 
    ylab('Population difference April to July') + 
    xlab('Local Authority Population') + 
    theme(legend.title = element_blank(),
          #legend.position = c(0.8, 0.8),
          text = element_text(size = 12))
  
  return(p)
  
}

p <- plot_change(m_compare)

fn <- '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/pop_change.png'

ggsave(fn, 
       p,
       width = 8, height = 6,
       units = 'in')

ggsave(gsub('.png', '.pdf', fn), 
       p,
       width = 8, height = 6,
       units = 'in',
       useDingbats = F)

#p <- plot_change(m_compare %>% select(-pop) %>% rename(pop = density))








