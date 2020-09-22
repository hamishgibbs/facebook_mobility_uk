
plot_weekends <- function(data){
  #plot all weekends in the dataset starting at the first saturday + 1
  #requires a column named `date`
  min_date <- data %>% pull(date) %>% min()
  max_date <- data %>% pull(date) %>% max()
  
  day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  date_seq <- seq.Date(from = min_date, to = max_date, by = 1)
  
  dates <- tibble(date = date_seq) %>% 
    mutate(day = day_names[as.POSIXlt(date)$wday + 1],
          weekend = ifelse(day %in% c('Saturday', 'Sunday'), T, F)) %>% 
    filter(day == 'Saturday')
  
  w <- 200 / date_seq %>% length()

  date_polys <- c()
  
  for (dv in dates %>% pull(date) %>% as.list()){
    
    #date_polys <- append(date_polys, eval(substitute( geom_rect(aes(xmin = dv, xmax = dv + 1, ymin = ymin, ymax = ymax), fill = 'lightgrey', alpha = 0.5))))
    date_polys <- append(date_polys, eval(substitute( geom_vline(xintercept = dv, colour = 'lightgrey', alpha = 0.5, size = w))))
    date_polys <- append(date_polys, eval(substitute( geom_vline(xintercept = dv + 1, colour = 'lightgrey', alpha = 0.5, size = w))))
    
  }
  
  return(date_polys)
}
