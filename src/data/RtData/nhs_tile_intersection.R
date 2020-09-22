nhs <- read_sf('/Users/hamishgibbs/Downloads/NHS_England_Regions__April_2019__EN_BFC-shp/NHS_England_Regions__April_2019__EN_BFC.shp') %>% 
  st_transform(4326)

tiles

tiles_nhs <- st_intersection(st_centroid(tiles), nhs)

tiles_nhs <- tiles_nhs %>% 
  select(quadkey, nhser19nm) %>% 
  rename(nhs_name = nhser19nm) %>% 
  st_drop_geometry()

st_write(tiles_nhs, '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/rt_data/nhs/tiles_nhs.csv')
