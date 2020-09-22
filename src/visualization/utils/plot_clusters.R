source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/qualitative_pal.R')

plot_clusters <- function(clust, basemap, title = NULL, uk_lims = F, custom_bbox = NULL, custom_pal = NULL){
  clust <- tiles %>% 
    left_join(clust, by = 'quadkey') %>% 
    drop_na(cluster)
  
  
  if (is.null(custom_bbox)){
    bbox <- st_bbox(clust)
  } else {
    bbox = custom_bbox
  }
  
  if (uk_lims){
    xlims <- xlim(-8, 2)
    ylims <- ylim(50.4, 58.4)
  } else {
    xlims <- xlim(bbox$xmin, bbox$xmax)
    ylims <- ylim(bbox$ymin, bbox$ymax)
  }
  
  if (is.null(custom_pal)){
    pal <- qualitative_pal(unique(clust$cluster), round(length(unique(clust$cluster)) / 80, 0) + 100) 
  } else {
    pal <- custom_pal
  }
  
  p <- clust %>% 
    ggplot() + 
    geom_sf(aes(fill = as.character(cluster)), size = 0, show.legend = FALSE) + 
    geom_sf(data = world, size = 0.1, colour = 'black', fill = 'transparent') + 
    geom_sf(data = basemap, size = 0.1, colour = 'black', fill = 'transparent') + 
    xlims + 
    ylims +
    scale_fill_manual(values = pal) + 
    theme_void() + 
    theme(legend.position = 'none') + 
    ggtitle(title)
  
  return(p)
  
}
