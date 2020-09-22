plot_bank_holidays <- function(){
  #https://publicholidays.co.uk/england/2020-dates/
  bank_holidays <- c(as.Date('2020-01-01'),
                     as.Date('2020-04-10'),
                     as.Date('2020-04-13'),
                     as.Date('2020-05-08'),
                     as.Date('2020-05-25'),
                     as.Date('2020-08-31'),
                     as.Date('2020-12-25'),
                     as.Date('2020-12-26'),
                     as.Date('2020-12-28'))
  
  bh <- geom_vline(xintercept = bank_holidays, size = 0.7, colour = 'blue', alpha = 0.5)
  
  return(bh)
  
}
