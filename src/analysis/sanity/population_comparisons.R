source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

d <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv') %>% 
  pull(pop) %>% sum()

ni_pop <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/OA_Population/NI_Mid_Pop.csv')
scot_pop <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/OA_Population/simd2020_withinds.csv')
ew_pop <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/OA_Population/Eng_Wal_OA_Mid_Pop.csv')

a <- ni_pop %>% pull(MYE) %>% sum() #OK

b <- scot_pop %>% pull(Total_population) %>% sum() #OK

c <- ew_pop %>% pull(Pop) %>% sum() #OK

a + b  + c
d

oa_total_pop <- a + b + c

#get total population of mean internal pop from omb - see if is similar to UK pop (in magnitude)

e <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv')
e %>% 
  filter(start_quadkey == end_quadkey) %>% 
  group_by(journey) %>% 
  summarise(n_crisis = mean(n_crisis, na.rm = T)) %>% 
  pull(n_crisis) %>% 
  sum()

total_mean_pop <- e %>% 
  filter(date <= as.Date('2020-07-01')) %>% 
  group_by(date) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T)) %>% 
  pull(n_crisis) %>% 
  mean()

total_mean_pop / oa_total_pop

daily_fb_pop <- e %>% 
  filter(date <= as.Date('2020-07-01')) %>% 
  group_by(date) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>% 
  mutate(pop_prop = n_crisis / oa_total_pop)

p <- daily_fb_pop %>% ggplot() + 
  geom_path(aes(x = date, y = n_crisis)) + 
  ylab('Total number of Facebook users') + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank())

ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/total_population.png', p, 
       width = 8.5, height = 2.5, units = 'in')

c(daily_fb_pop %>% pull(date))[which(c(daily_fb_pop %>% pull(n_crisis)) == max(daily_fb_pop %>% pull(n_crisis)))]
c(daily_fb_pop %>% pull(date))[which(c(daily_fb_pop %>% pull(n_crisis)) == min(daily_fb_pop %>% pull(n_crisis)))]

max(daily_fb_pop %>% pull(n_crisis))
min(daily_fb_pop %>% pull(n_crisis))
sd(daily_fb_pop %>% pull(n_crisis))
