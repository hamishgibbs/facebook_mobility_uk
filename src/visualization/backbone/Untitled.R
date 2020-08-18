suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})
#most persistent cluster
#largest cluster 
#largest variance in cluster size
#join to a3 & explore

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/backbone_summary.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/backbone_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

bb <- read_csv(.args[1]) %>% 
  select(-degree_sequence)

p <- bb %>% 
  ggplot() + 
  geom_path(aes(x = alpha, y = flow_ratio)) + 
  geom_path(aes(x = alpha, y = edge_ratio), color = 'red') + 
  geom_path(aes(x = alpha, y = clustering_coefficient)) + 
  geom_point(aes(x = alpha, y = clustering_coefficient)) + 
  facet_wrap(~date) + 
  theme_bw()


a_diff <- c()

for (df in bb %>% group_by(date) %>% group_split()) {
  
  fr = DescTools::AUC(df$alpha, df$flow_ratio)
  er = DescTools::AUC(df$alpha, df$edge_ratio)
  
  a_diff <- append(a_diff, fr - er)
  
}
a_diff

ggplot() + 
  geom_path(aes(x = 1:length(a_diff), y = a_diff))


