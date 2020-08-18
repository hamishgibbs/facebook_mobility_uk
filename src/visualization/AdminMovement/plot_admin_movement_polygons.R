polys <- st_read('/Users/hamishgibbs/Downloads/admin_geography_test.shp')
polys <- polys %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  arrange(-area)

p <- polys %>% 
  ggplot() + 
  geom_sf(fill = '#EBEBEB') + 
  ylim(50, 59) + 
  xlim(-8, 2) +
  theme_bw() + 
  plot_default_theme +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  ggtitle('Approximate FB Administrative Geography')

p
ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/admin_geography_approx.png', p,
       width = 5.5, height = 8, units = 'in')
