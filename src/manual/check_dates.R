fns <- list.files('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/Britain_TilePopulation', full.names = T)

sizes <- list()
for (fn in fns){
  sizes[[fn]] <- data.frame(filename = fn, size = file.info(fn)$size)
}

sizes <- do.call(rbind, sizes)

sizes %>% 
  ggplot() + 
  geom_point(aes(x = filename, y = size)) + 
  theme(axis.text.x = element_blank())

fns <- sizes %>% 
  filter(size < 1000000) %>% 
  pull(filename) %>% 
  as.character()

fns <- lapply(fns, function(x){tail(str_split(x, '/')[[1]], 1)}) %>% unlist()

fns = str_replace(fns, 'Britain_', '')
fns = str_replace(fns, '.csv', '')


lubridate::parse_date_time(fns[1], 1)
tibble(dates = as.POSIXct(strptime(fns, format='%Y_%m_%d_%H%M')), x = 1) %>% 
  ggplot()  +
  geom_point(aes(x = dates, y = x))

dates <- as.POSIXct(strptime(fns, format='%Y_%m_%d_%H%M'))


add_8_hrs <- function(x){
  return(x + (8 * 60 * 60))
}

d = min(dates)
expected_dates <- c()

while(d <= max(dates)){
  
  d <- add_8_hrs(d)
  
  expected_dates <- append(expected_dates, d)
  
}

expected_dates <- as.character(expected_dates)
dates <- as.character(dates)

setdiff(expected_dates, dates)

as.POSIXct()


fns

tmp <- '/Users/hamishgibbs/Downloads/tmp/'
tmp2 <- '/Users/hamishgibbs/Downloads/tmp2/'

fns = list.files(tmp, full.names = F)

for (fn in fns){
  
  data <- read_csv(paste0(tmp, fn))
  
  new_name <- str_sub(fn,-19,-1)
  new_name <- str_replace(new_name, '-', '_')
  new_name <- str_replace(new_name, '-', '_')
  new_name <- str_replace(new_name, ' ', '_')
  
  new_name <- paste0('Britain_', new_name)
  
  write_csv(data, paste0(tmp2, new_name))
  
}
