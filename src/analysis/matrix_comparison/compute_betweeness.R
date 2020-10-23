suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(igraph)
  require(colorspace)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/betweeness.csv')
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1]) %>% 
  mutate(start_quadkey = str_pad(start_quadkey, 12, pad = "0"),
         end_quadkey = str_pad(end_quadkey, 12, pad = "0"))

mob <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  group_by(date) %>% 
  group_split()

compute_betweeness <- function(mob_date){
  
  g <- igraph::graph_from_data_frame(mob_date %>% select(start_quadkey, end_quadkey, n_crisis))
  
  btw <- edge_betweenness(g)
  
  mob_date$betweeness = btw
  
  return(mob_date)
}

m_btw <- lapply(mob, compute_betweeness)

m_btw <- do.call(rbind, m_btw)

write_csv(m_btw, tail(.args, 1))
