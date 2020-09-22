lockdown_communities <- function(area, date, label_map, date_interval = 7){
  
  area_qks <- area %>% pull(quadkey)
  
  min_date <- date - date_interval
  max_date <- date + date_interval
  
  label_map_area <- label_map %>% 
    filter(quadkey %in% area_qks, date >= min_date, date <= max_date)
  
  label_map_area <- label_map %>% filter(cluster %in% c(label_map_area %>% pull(cluster) %>% unique()), date >= min_date, date <= max_date)
  
  return(label_map_area)
  
}

lockdown_communities_shp <- function(i_date, label_map_area){
  
  shp <- tiles %>% left_join(label_map_area %>% filter(date == i_date), by = c('quadkey'))
  
  return(shp)
  
}

lockdown_change <- function(label_map_area){
  
  lma_qks <- label_map_area %>% pull(quadkey)
  
  change <- mob %>% 
    filter(start_quadkey %in% lma_qks | end_quadkey %in% lma_qks) %>% 
    mutate(journey_type = ifelse(start_quadkey %in% lma_qks & !end_quadkey %in% lma_qks, 'Outflow', NA),
           journey_type = ifelse(!start_quadkey %in% lma_qks & end_quadkey %in% lma_qks, 'Inflow', journey_type),
           journey_type = ifelse(start_quadkey %in% lma_qks & end_quadkey %in% lma_qks, 'Internal Flow', journey_type)) %>% 
    group_by(date, journey_type) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T),
              n_baseline = sum(n_baseline, na.rm = T)) %>% 
    mutate(perc_change = ((n_crisis - n_baseline) / n_baseline) * 100)
  
  return(change)
  
}

plot_lockdown_change <- function(label_map_area, i_date, legend = T, variable = 'perc_change', y_label = '% Change from baseline', log = F, title = NULL){
  
  if (legend){
    legend <- NULL
  } else {
    legend <- 'none'
  }
  
  if (log){
    g_path <- geom_path(aes(x = date, y = log(!! sym(variable), 10), color = journey_type))
  } else {
    g_path <- geom_path(aes(x = date, y = !! sym(variable), color = journey_type))
  }
  
  p_data <- lockdown_change(label_map_area)
  
  p <- p_data %>% 
    ggplot() + 
    plot_weekends(p_data) + 
    plot_bank_holidays() + 
    g_path + 
    scale_color_manual(values = c('Inflow' = '#009762', 'Internal Flow' = 'black', 'Outflow' = '#00ADFF')) + 
    geom_vline(aes(xintercept = i_date), linetype = 'dashed') + 
    theme_bw() + 
    plot_default_theme + 
    ylab(y_label) + 
    theme(axis.title.x = element_blank(),
          legend.position = legend, 
          text = element_text(size = 12)) + 
    labs(color = 'Journey Type') + 
    ggtitle(title)
  
  return(p)
  
}

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

plot_lockdown_extent <- function(name, date, legend = F){
  
  area <- a3 %>% filter(NAME_3 %in% name)
  
  i_date <- as.Date(date)
  
  print(date)
  
  label_map_area <- lockdown_communities(area, i_date, label_map)
  
  p_change_perc <- plot_lockdown_change(label_map_area, i_date, legend, 'perc_change', title = 'b')
  p_change_abs <- plot_lockdown_change(label_map_area, i_date, legend, 'n_crisis', 'Travellers (log)', log = T, title = 'c')
  
  p_map <- plot_clusters(label_map_area %>% filter(date == i_date), basemap = gadm, title = paste0(name, ' - ', i_date)) + 
    geom_sf(data = gadm %>% filter(NAME_3 == name), aes(color = 'Local Intervention\nExtent'), fill = 'transparent') + 
    scale_color_manual(values = c('Local Intervention\nExtent' = 'blue')) + 
    theme(legend.position = c(0.84, 0.95), legend.title = element_blank())
  
  title <- letter_title('a')
  
  p_map <- cowplot::plot_grid(title, p_map, rel_heights = c(0.05, 1), nrow = 2)
  
  p_change <- cowplot::plot_grid(p_change_perc, p_change_abs, nrow = 2)
  
  p <- cowplot::plot_grid(p_map, p_change, rel_widths = c(0.4, 0.6))
  
  return(p)
  
}
