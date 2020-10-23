source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/qualitative_pal.R')

area_quadkeys <- function(name){
  #return the quadkeys intersecting a local authority area
  
  ai <- st_intersection(tiles, la %>% filter(lad19nm %in% name)) %>% 
    pull(quadkey) %>% 
    unique()
  
  return(ai)
}

get_area_mob <- function(area_qks){
  #get mobiltity into/out of/within an area
  
  area_mob <- mob %>% 
    filter(start_quadkey != end_quadkey) %>% 
    mutate(type = ifelse(start_quadkey %in% area_qks, 'Outflow', NA),
           type = ifelse(end_quadkey %in% area_qks, 'Inflow', type),
           type = ifelse(start_quadkey %in% area_qks & end_quadkey %in% area_qks, 'Internal flow', type)) %>% 
    filter(!is.na(type)) %>% 
    group_by(date, type) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T),
              n_baseline = sum(n_baseline, na.rm = T),
              perc_change = ((n_crisis - n_baseline) / n_baseline) * 100)   
  
  return(area_mob)
  
}

plot_google_mobility <- function(area_code, min_date, max_date, i_date){
  #plot google mobility for an area
  
  p1 <- goog %>% 
    filter(iso_3166_2_code == area_code,
           date > min_date,
           date < max_date) %>% 
    ggplot() + 
    geom_path(aes(x = date, y = retail_and_recreation_percent_change_from_baseline, colour = 'Retail and recreation')) + 
    geom_path(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, colour = 'Grocery and pharmacy')) + 
    geom_path(aes(x = date, y = transit_stations_percent_change_from_baseline, colour = 'Transit stations')) + 
    geom_path(aes(x = date, y = workplaces_percent_change_from_baseline, colour = 'Workplaces')) + 
    geom_path(aes(x = date, y = residential_percent_change_from_baseline, colour = 'Residential')) +  
    scale_color_manual(values = c('Retail and recreation' = '#984ea3', 'Grocery and pharmacy' = '#377eb8', 'Transit stations' = '#e41a1c', 'Workplaces' = '#4daf4a', 'Residential' = '#ff7f00')) + 
    geom_vline(aes(xintercept = i_date), linetype = 'dashed') + 
    theme_bw() + 
    plot_default_theme + 
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 12)) + 
    ylab('% change from baseline') + 
    ggtitle('Google')
  
  return(p1)
  
  
}

plot_facebook_mobility <- function(area_mob, i_date){
  #plot facebook mobility for an area
  
  p2 <- area_mob %>% 
    ggplot() + 
    geom_path(aes(x = date, y = perc_change, group = type, color = type)) + 
    scale_color_manual(values = c('#a6cee3', '#1f78b4', '#33a02c')) + 
    geom_vline(aes(xintercept = i_date), linetype = 'dashed') + 
    theme_bw() +
    plot_default_theme + 
    theme(legend.title = element_blank()) + 
    theme(axis.title.x = element_blank(),
          text = element_text(size = 12),
          legend.margin = margin(r = 0.78, unit = 'in')) + 
    ylab('% change from baseline') + 
    ggtitle('Facebook')
  
  return(p2)
  
}

get_time_agg_community <- function(area_qks, i_date, interval = 14, type = 'early'){
  #get the extent of a community aggregated through time
  
  comm_min_date <- i_date - interval
  comm_max_date <- i_date + interval
  
  connected_community <- label_map %>% 
    filter(date > comm_min_date, date < comm_max_date) %>% 
    filter(quadkey %in% area_qks) %>% 
    pull(cluster) %>% unique()
  
  if (type == 'early'){
    
    early_community <- label_map %>% 
      filter(date > comm_min_date, date <= i_date) %>% 
      filter(cluster %in% connected_community) %>% 
      group_by(quadkey) %>% 
      summarise(cluster = names(sort(table(cluster),decreasing=TRUE)[1]),
                n_dates = length(unique(date)))
    
    return(early_community)
    
  }
  
  if (type == 'late'){
    
    late_community <- label_map %>% 
      filter(date >= i_date, date < comm_max_date) %>% 
      filter(cluster %in% connected_community) %>% 
      group_by(quadkey) %>% 
      summarise(cluster = names(sort(table(cluster),decreasing=TRUE)[1]),
                n_dates = length(unique(date)))
    
    return(late_community)
    
  }
  
  
}


plot_time_agg_community <- function(comm, title, area, legend = F, rep_n = 5){
  #plot a time aggregated community
  
  p_data <- tiles %>% 
    left_join(comm, by = c('quadkey')) %>% 
    drop_na(cluster)
  
  bbox <- st_bbox(p_data)
  
  if (legend){
    legend_theme <- labs(fill = 'Days in\nconnected\ncommunity')
  } else {
    legend_theme <- theme(legend.position = 'none')
  }
  
  pal <- qualitative_pal(p_data %>% pull(cluster) %>% unique(), rep_n = rep_n)
  
  p <- ggplot() + 
    geom_sf(data = p_data, aes(fill = cluster), color = 'black', size = 0.1) + 
    scale_fill_manual(values = pal) + 
    geom_sf(data = la, fill = 'transparent', size = 0.2, color = 'black') + 
    geom_sf(data = area, fill = 'transparent', size = 0.3, aes(color = 'Intervention\nArea')) + 
    scale_color_manual(values = c('Intervention\nArea' = 'red')) + 
    xlim(bbox[1], bbox[3]) + 
    ylim(bbox[2], bbox[4]) + 
    theme_void() + 
    ggtitle(title) + 
    legend_theme + 
    guides(fill = guide_none()) + 
    theme(legend.title = element_blank()) + 
    theme(text = element_text(size = 12))
  
  return(p)
  
}

plot_area_cases <- function(la_name, min_date, max_date, i_date, cases){
  
    area_cases <- cases %>% filter(name %in% la_name) %>% 
      group_by(date) %>% 
      summarise(count = sum(count, na.rm = T)) %>% 
      filter(date >= min_date & date <= max_date)
    
    p_cases <- area_cases %>% 
      ggplot() + 
      geom_path(aes(x = date, y = count)) + 
      geom_vline(aes(xintercept = i_date), linetype = 'dashed') + 
      theme_bw() + 
      plot_default_theme + 
      ylab('Cases') + 
      theme(axis.title.x = element_blank(),
            text = element_text(size = 12),
            plot.margin = margin(r = 2.1, unit = 'in')) + 
      ggtitle('Cases')
    
    return(p_cases)
    
}

letter_title <- function(letter){
  #get an arbitrary title for a ggplot object
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

