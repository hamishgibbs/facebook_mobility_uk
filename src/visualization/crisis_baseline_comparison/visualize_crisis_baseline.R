suppressPackageStartupMessages({
  require(tidyverse)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/crisis_baseline_comparison.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/crisis_baseline_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

df <- read_csv(.args[1], col_types = cols()) %>% 
  mutate(period = paste('Period', period))

p <- df %>% ggplot() + 
  geom_point(aes(x = n_baseline, y = n_crisis), size = 0.1) + 
  geom_abline(aes(intercept = 0, slope = 1)) + 
  facet_wrap(~period, nrow = 1) + 
  theme_bw() + 
  plot_default_theme + 
  theme(strip.background = element_rect(fill="white"))

ggsave(tail(.args, 1), 
       p,
       width = 8.5,
       height = 4,
       units = 'in')

