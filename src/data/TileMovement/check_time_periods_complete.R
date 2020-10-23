mob <- read_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_hours.csv')

n_periods <- mob %>% 
  group_by(date_time) %>% 
  summarise(n = n()) %>% 
  mutate(date = as.Date(date_time)) %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  pull(n) %>% 
  unique()

testthat::expect_equal(n_periods, 3)
